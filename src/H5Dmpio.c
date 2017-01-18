/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  rky 980813
 * KY 2005 revised the code and made the change to support and optimize
 * collective IO support.
 * Purpose:    Functions to read/write directly between app buffer and file.
 *
 *         Beware of the ifdef'ed print statements.
 *         I didn't make them portable.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h"          /* This source code file is part of the H5D module */


/***********/
/* Headers */
/***********/
#include "H5private.h"        /* Generic Functions */
#include "H5Dpkg.h"           /* Datasets          */
#include "H5Eprivate.h"       /* Error handling    */
#include "H5Fprivate.h"       /* File access       */
#include "H5FDprivate.h"      /* File drivers      */
#include "H5Iprivate.h"       /* IDs               */
#include "H5MMprivate.h"      /* Memory management */
#include "H5Oprivate.h"       /* Object headers    */
#include "H5Pprivate.h"       /* Property lists    */
#include "H5Sprivate.h"       /* Dataspaces        */
#include "H5VMprivate.h"       /* Vector            */

#ifdef H5_HAVE_PARALLEL

/****************/
/* Local Macros */
/****************/

/* Macros to represent different IO options */
#define H5D_ONE_LINK_CHUNK_IO          0
#define H5D_MULTI_CHUNK_IO             1
#define H5D_ONE_LINK_CHUNK_IO_MORE_OPT 2
#define H5D_MULTI_CHUNK_IO_MORE_OPT    3

/***** Macros for One linked collective IO case. *****/
/* The default value to do one linked collective IO for all chunks.
   If the average number of chunks per process is greater than this value,
      the library will create an MPI derived datatype to link all chunks to do collective IO.
      The user can set this value through an API. */

/* Macros to represent options on how to obtain chunk address for one linked-chunk IO case */
#define H5D_OBTAIN_ONE_CHUNK_ADDR_IND 0
#define H5D_OBTAIN_ALL_CHUNK_ADDR_COL 2

/* Macros to define the default ratio of obtaining all chunk addresses for one linked-chunk IO case */
#define H5D_ALL_CHUNK_ADDR_THRES_COL  30
#define H5D_ALL_CHUNK_ADDR_THRES_COL_NUM 10000

/***** Macros for multi-chunk collective IO case. *****/
/* The default value of the threshold to do collective IO for this chunk.
   If the average number of processes per chunk is greater than the default value,
   collective IO is done for this chunk.
*/

/* Macros to represent different IO modes(NONE, Independent or collective)for multiple chunk IO case */
#define H5D_CHUNK_IO_MODE_IND         0
#define H5D_CHUNK_IO_MODE_COL         1

/* Macros to represent the regularity of the selection for multiple chunk IO case. */
#define H5D_CHUNK_SELECT_REG          1
#define H5D_CHUNK_SELECT_IRREG        2
#define H5D_CHUNK_SELECT_NONE         0

#define PARALLEL_COMPRESS_DEBUG

#ifdef PARALLEL_COMPRESS_DEBUG
FILE *debug_file;
#endif

/******************/
/* Local Typedefs */
/******************/
/* Combine chunk address and chunk info into a struct for better performance. */
typedef struct H5D_chunk_addr_info_t {
  haddr_t chunk_addr;
  H5D_chunk_info_t chunk_info;
} H5D_chunk_addr_info_t;

typedef struct H5D_filtered_collective_io_info_t {
  H5D_chunk_info_t chunk_info;
  H5F_block_t      old_chunk;
  H5F_block_t      new_chunk;
  size_t           io_size;
  size_t           num_writers;
  int              owner;
  void            *buf;
} H5D_filtered_collective_io_info_t;

/********************/
/* Local Prototypes */
/********************/
static herr_t H5D__chunk_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, H5D_chunk_map_t *fm);
static herr_t H5D__multi_chunk_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, H5D_chunk_map_t *fm,
    H5P_genplist_t *dx_plist);
static herr_t H5D__multi_chunk_filtered_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, H5D_chunk_map_t *fm,
    H5P_genplist_t *dx_plist);
static herr_t H5D__link_chunk_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, H5D_chunk_map_t *fm, int sum_chunk,
    H5P_genplist_t *dx_plist);
static herr_t H5D__link_chunk_filtered_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, H5D_chunk_map_t *fm,
    H5P_genplist_t *dx_plist);
static herr_t H5D__inter_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, const H5S_t *file_space,
    const H5S_t *mem_space);
static herr_t H5D__final_collective_io(H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, hsize_t nelmts, MPI_Datatype *mpi_file_type,
    MPI_Datatype *mpi_buf_type);
static herr_t H5D__sort_chunk(H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
    H5D_chunk_addr_info_t chunk_addr_info_array[], int many_chunk_opt);
static herr_t H5D__obtain_mpio_mode(H5D_io_info_t *io_info, H5D_chunk_map_t *fm,
    H5P_genplist_t *dx_plist, uint8_t assign_io_mode[], haddr_t chunk_addr[]);
static herr_t H5D__ioinfo_xfer_mode(H5D_io_info_t *io_info, H5P_genplist_t *dx_plist,
    H5FD_mpio_xfer_t xfer_mode);
static herr_t H5D__ioinfo_coll_opt_mode(H5D_io_info_t *io_info, H5P_genplist_t *dx_plist,
    H5FD_mpio_collective_opt_t coll_opt_mode);
static herr_t H5D__mpio_get_min_chunk(const H5D_io_info_t *io_info,
    const H5D_chunk_map_t *fm, int *min_chunkf);
static herr_t H5D__mpio_get_sum_chunk(const H5D_io_info_t *io_info,
    const H5D_chunk_map_t *fm, int *sum_chunkf);
static herr_t H5D__construct_filtered_io_info_list(const H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info, const H5D_chunk_map_t *fm,
    H5D_filtered_collective_io_info_t **chunk_list, size_t *num_entries,
    size_t **_num_chunks_selected_array);
static herr_t H5D__mpio_array_gather(const H5D_io_info_t *io_info, void *local_array,
    size_t array_num_entries, size_t array_entry_size,
    void **gathered_array, size_t *gathered_array_num_entries,
    int (*sort_func)(const void *, const void *));
static herr_t H5D__mpio_filtered_collective_write_type(
    H5D_filtered_collective_io_info_t *chunk_list, size_t num_entries,
    MPI_Datatype *new_mem_type, hbool_t *mem_type_derived,
    MPI_Datatype *new_file_type, hbool_t *file_type_derived);
static herr_t H5D__update_filtered_collective_chunk_entry(
    H5D_filtered_collective_io_info_t *chunk_entry, const H5D_io_info_t *io_info,
    const H5D_type_info_t *type_info);
static int H5D__cmp_chunk_addr(const void *chunk_addr_info1, const void *chunk_addr_info2);
static int H5D__cmp_filtered_collective_io_entry(const void *filtered_collective_io_entry1,
    const void *filtered_collective_io_entry2);


/*********************/
/* Package Variables */
/*********************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_opt_possible
 *
 * Purpose:     Checks if an direct I/O transfer is possible between memory and
 *                  the file.
 *
 * Return:      Sauccess:   Non-negative: TRUE or FALSE
 *              Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, April 3, 2002
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5D__mpio_opt_possible(const H5D_io_info_t *io_info, const H5S_t *file_space,
    const H5S_t *mem_space, const H5D_type_info_t *type_info,
    const H5D_chunk_map_t *fm, H5P_genplist_t *dx_plist)
{
    int local_cause = 0;        /* Local reason(s) for breaking collective mode */
    int global_cause = 0;       /* Global reason(s) for breaking collective mode */
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(io_info);
    HDassert(mem_space);
    HDassert(file_space);
    HDassert(type_info);


    /* For independent I/O, get out quickly and don't try to form consensus */
    if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_INDEPENDENT)
        local_cause |= H5D_MPIO_SET_INDEPENDENT;

    /* Optimized MPI types flag must be set */
    /* (based on 'HDF5_MPI_OPT_TYPES' environment variable) */
    if(!H5FD_mpi_opt_types_g)
        local_cause |= H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED;

    /* Don't allow collective operations if datatype conversions need to happen */
    if(!type_info->is_conv_noop)
        local_cause |= H5D_MPIO_DATATYPE_CONVERSION;

    /* Don't allow collective operations if data transform operations should occur */
    if(!type_info->is_xform_noop)
        local_cause |= H5D_MPIO_DATA_TRANSFORMS;

    /* Check whether these are both simple or scalar dataspaces */
    if(!((H5S_SIMPLE == H5S_GET_EXTENT_TYPE(mem_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(mem_space))
            && (H5S_SIMPLE == H5S_GET_EXTENT_TYPE(file_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(file_space))))
        local_cause |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;

    /* Dataset storage must be contiguous or chunked */
    if(!(io_info->dset->shared->layout.type == H5D_CONTIGUOUS ||
            io_info->dset->shared->layout.type == H5D_CHUNKED))
        local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;

    /* check if external-file storage is used */
    if(io_info->dset->shared->dcpl_cache.efl.nused > 0)
        local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;

    /* The handling of memory space is different for chunking and contiguous
     *  storage.  For contiguous storage, mem_space and file_space won't change
     *  when it it is doing disk IO.  For chunking storage, mem_space will
     *  change for different chunks. So for chunking storage, whether we can
     *  use collective IO will defer until each chunk IO is reached.
     */

    /* Check for independent I/O */
    if(local_cause & H5D_MPIO_SET_INDEPENDENT)
        global_cause = local_cause;
    else {
        int mpi_code;               /* MPI error code */

        /* Form consensus opinion among all processes about whether to perform
         * collective I/O
         */
        if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&local_cause, &global_cause, 1, MPI_INT, MPI_BOR, io_info->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)
    } /* end else */

    /* Write the local value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_LOCAL_NO_COLLECTIVE_CAUSE_NAME, &local_cause) < 0)
       HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set local no collective cause property")

    /* Write the global value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_GLOBAL_NO_COLLECTIVE_CAUSE_NAME, &global_cause) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set global no collective cause property")

    /* Set the return value, based on the global cause */
    ret_value = global_cause > 0 ? FALSE : TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__mpio_opt_possible() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_read
 *
 * Purpose:     MPI-IO function to read directly from app buffer to file.
 *
 * Return:      non-negative on success, negative on failure.
 *
 * Programmer:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_read(const H5D_io_info_t *io_info, const H5D_type_info_t H5_ATTR_UNUSED *type_info,
    hsize_t mpi_buf_count, const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space)
{
    const H5D_contig_storage_t *store_contig = &(io_info->store->contig);    /* Contiguous storage info for this I/O operation */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if(H5F_block_read(io_info->dset->oloc.file, H5FD_MEM_DRAW, store_contig->dset_addr, (size_t)mpi_buf_count, io_info->raw_dxpl_id, io_info->u.rbuf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "can't finish collective parallel read")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_read() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_write
 *
 * Purpose:     MPI-IO function to write directly from app buffer to file.
 *
 * Return:      non-negative on success, negative on failure.
 *
 * Programmer:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_write(const H5D_io_info_t *io_info, const H5D_type_info_t H5_ATTR_UNUSED *type_info,
    hsize_t mpi_buf_count, const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space)
{
    const H5D_contig_storage_t *store_contig = &(io_info->store->contig);    /* Contiguous storage info for this I/O operation */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if(H5F_block_write(io_info->dset->oloc.file, H5FD_MEM_DRAW, store_contig->dset_addr, (size_t)mpi_buf_count, io_info->raw_dxpl_id, io_info->u.wbuf) < 0)
       HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't finish collective parallel write")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_array_gather
 *
 * Purpose:     Given arrays by MPI ranks, gathers them into a single large
 *              array which is then distributed back to all ranks. If the
 *              sort_func argument is specified, the list is sorted before
 *              being returned.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jordan Henderson
 *              Friday, January 6th, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_array_gather(const H5D_io_info_t *io_info, void *local_array,
    size_t array_num_entries, size_t array_entry_size,
    void **_gathered_array, size_t *_gathered_array_num_entries,
    int (*sort_func)(const void *, const void *))
{
    size_t  gathered_array_num_entries = 0;
    size_t  i;
    void   *gathered_array = NULL;
    int    *receive_counts_array = NULL;
    int    *displacements_array = NULL;
    int     mpi_code, mpi_size;
    int     sendcount;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(io_info);
    HDassert(local_array);
    HDassert(_gathered_array);
    HDassert(_gathered_array_num_entries);

    if ((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

    if (MPI_SUCCESS != (mpi_code = MPI_Allreduce(&array_num_entries, &gathered_array_num_entries, 1, MPI_INT, MPI_SUM, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

    if (NULL == (gathered_array = H5MM_malloc(gathered_array_num_entries * array_entry_size)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate total gathered array")

    if (NULL == (receive_counts_array = (int *) H5MM_malloc((size_t) mpi_size * sizeof(*receive_counts_array))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate receive counts array")

    if (NULL == (displacements_array = (int *) H5MM_malloc((size_t) mpi_size * sizeof(*displacements_array))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate displacements array")

    if (MPI_SUCCESS != (mpi_code = MPI_Allgather(&array_num_entries, 1, MPI_INT, receive_counts_array, 1, MPI_INT, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code)

    /* Multiply each receive count by the size of the array entry,
     * since the data is sent in a count of bytes */
    /* XXX: Check to make sure array_entry_size doesn't overflow an int */
    for (i = 0; i < (size_t) mpi_size; i++)
        receive_counts_array[i] *= array_entry_size;

    /* Set receive buffer offsets for MPI_Allgatherv */
    displacements_array[0] = 0;
    for (i = 1; i < (size_t) mpi_size; i++)
        displacements_array[i] = displacements_array[i - 1] + receive_counts_array[i - 1];

    H5_CHECKED_ASSIGN(sendcount, int, array_num_entries * array_entry_size, size_t);
    if (MPI_SUCCESS != (mpi_code = MPI_Allgatherv(local_array, sendcount, MPI_BYTE,
            gathered_array, receive_counts_array, displacements_array, MPI_BYTE, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allgatherv failed", mpi_code)

    if (sort_func) HDqsort(gathered_array, gathered_array_num_entries, array_entry_size, sort_func);

    *_gathered_array = gathered_array;
    *_gathered_array_num_entries = gathered_array_num_entries;

#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, " Contents of gathered array:\n");
    HDfprintf(debug_file, "------------------------------\n");
    for (size_t j = 0; j < (size_t) gathered_array_num_entries; j++) {
        HDfprintf(debug_file, "| Chunk Entry %zd:\n", j);
        HDfprintf(debug_file, "|  - Chunk Address: %a\n", ((H5D_filtered_collective_io_info_t *) gathered_array)[j].old_chunk.offset);
        HDfprintf(debug_file, "|  - Chunk Length: %zd\n", ((H5D_filtered_collective_io_info_t *) gathered_array)[j].old_chunk.length);
        HDfprintf(debug_file, "|  - Address of mspace: %x\n", ((H5D_filtered_collective_io_info_t *) gathered_array)[j].chunk_info.mspace);
    }
    HDfprintf(debug_file, "------------------------------\n\n");
#endif

done:
    if (receive_counts_array)
        H5MM_free(receive_counts_array);
    if (displacements_array)
        H5MM_free(displacements_array);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_array_gather() */


/*-------------------------------------------------------------------------
 * Function:    H5D__ioinfo_xfer_mode
 *
 * Purpose:     Switch to between collective & independent MPI I/O
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, August 12, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_xfer_mode(H5D_io_info_t *io_info, H5P_genplist_t *dx_plist,
    H5FD_mpio_xfer_t xfer_mode)
{
    herr_t  ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_STATIC

    /* Change the xfer_mode */
    io_info->dxpl_cache->xfer_mode = xfer_mode;
    if(H5P_set(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info->dxpl_cache->xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")

    /* Change the "single I/O" function pointers */
    if(xfer_mode == H5FD_MPIO_INDEPENDENT) {
        /* Set the pointers to the original, non-MPI-specific routines */
        io_info->io_ops.single_read = io_info->orig.io_ops.single_read;
        io_info->io_ops.single_write = io_info->orig.io_ops.single_write;
    } /* end if */
    else {
        HDassert(xfer_mode == H5FD_MPIO_COLLECTIVE);

        /* Set the pointers to the MPI-specific routines */
        io_info->io_ops.single_read = H5D__mpio_select_read;
        io_info->io_ops.single_write = H5D__mpio_select_write;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_xfer_mode() */


/*-------------------------------------------------------------------------
 * Function:    H5D__ioinfo_coll_opt_mode
 *
 * Purpose:     Switch between using collective & independent MPI I/O w/file
 *              set view
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  MuQun Yang
 *              Oct. 5th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_coll_opt_mode(H5D_io_info_t *io_info, H5P_genplist_t *dx_plist,
    H5FD_mpio_collective_opt_t coll_opt_mode)
{
    herr_t  ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_STATIC

    /* Change the optimal xfer_mode */
    io_info->dxpl_cache->coll_opt_mode = coll_opt_mode;
    if(H5P_set(dx_plist, H5D_XFER_MPIO_COLLECTIVE_OPT_NAME, &io_info->dxpl_cache->coll_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_coll_opt_mode() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_min_chunk
 *
 * Purpose:     Routine for obtaining minimum number of chunks to cover
 *              hyperslab selection selected by all processors.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_min_chunk(const H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
    int *min_chunkf)
{
    int num_chunkf;             /* Number of chunks to iterate over */
    int mpi_code;               /* MPI return code */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Get the number of chunks to perform I/O on */
    num_chunkf = H5SL_count(fm->sel_chunks);

    /* Determine the minimum # of chunks for all processes */
    if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&num_chunkf, min_chunkf, 1, MPI_INT, MPI_MIN, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_min_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_sum_chunk
 *
 * Purpose:     Routine for obtaining total number of chunks to cover
 *              hyperslab selection selected by all processors.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_sum_chunk(const H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
    int *sum_chunkf)
{
    int num_chunkf;             /* Number of chunks to iterate over */
    size_t ori_num_chunkf;
    int mpi_code;               /* MPI return code */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Get the number of chunks to perform I/O on */
    num_chunkf = 0;
    ori_num_chunkf = H5SL_count(fm->sel_chunks);
    H5_CHECKED_ASSIGN(num_chunkf, int, ori_num_chunkf, size_t);

    /* Determine the summation of number of chunks for all processes */
    /* XXX: In the case that more than one process is writing to the
     * same chunk, the summation of H5SL_count calls is incorrect. - JTH
     */
    if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&num_chunkf, sum_chunkf, 1, MPI_INT, MPI_SUM, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_sum_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5D__contig_collective_read
 *
 * Purpose:     Reads directly from contiguous data in file into application
 *              memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__contig_collective_read(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t H5_ATTR_UNUSED nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t H5_ATTR_UNUSED *fm)
{
    H5D_mpio_actual_io_mode_t actual_io_mode = H5D_MPIO_CONTIGUOUS_COLLECTIVE;
    H5P_genplist_t *dx_plist;           /* Pointer to DXPL */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(H5FD_MPIO == H5F_DRIVER_ID(io_info->dset->oloc.file));
    HDassert(TRUE == H5P_isa_class(io_info->raw_dxpl_id, H5P_DATASET_XFER));

    /* Call generic internal collective I/O routine */
    if(H5D__inter_collective_io(io_info, type_info, file_space, mem_space) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't finish shared collective MPI-IO")

    /* Obtain the data transfer properties */
    if(NULL == (dx_plist = H5I_object(io_info->raw_dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")

    /* Set the actual I/O mode property. internal_collective_io will not break to
     * independent I/O, so we set it here.
     */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__contig_collective_read() */


/*-------------------------------------------------------------------------
 * Function:    H5D__contig_collective_write
 *
 * Purpose:     Write directly to contiguous data in file from application
 *              memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__contig_collective_write(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t H5_ATTR_UNUSED nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t H5_ATTR_UNUSED *fm)
{
    H5D_mpio_actual_io_mode_t actual_io_mode = H5D_MPIO_CONTIGUOUS_COLLECTIVE;
    H5P_genplist_t *dx_plist;           /* Pointer to DXPL */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(H5FD_MPIO == H5F_DRIVER_ID(io_info->dset->oloc.file));
    HDassert(TRUE == H5P_isa_class(io_info->raw_dxpl_id, H5P_DATASET_XFER));

    /* Call generic internal collective I/O routine */
    if(H5D__inter_collective_io(io_info, type_info, file_space, mem_space) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't finish shared collective MPI-IO")

    /* Obtain the data transfer properties */
    if(NULL == (dx_plist = H5I_object(io_info->raw_dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")

    /* Set the actual I/O mode property. internal_collective_io will not break to
     * independent I/O, so we set it here.
     */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__contig_collective_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_collective_io
 *
 * Purpose:     Routine for
 *              1) choose an IO option:
 *                    a) One collective IO defined by one MPI derived datatype to link through all chunks
 *              or    b) multiple chunk IOs,to do MPI-IO for each chunk, the IO mode may be adjusted
 *                       due to the selection pattern for each chunk.
 *              For option a)
 *                      1. Sort the chunk address, obtain chunk info according to the sorted chunk address
 *                      2. Build up MPI derived datatype for each chunk
 *                      3. Build up the final MPI derived datatype
 *                      4. Set up collective IO property list
 *                      5. Do IO
 *              For option b)
 *                      1. Use MPI_gather and MPI_Bcast to obtain information of *collective/independent/none*
 *                         IO mode for each chunk of the selection
 *                      2. Depending on whether the IO mode is collective or independent or none,
 *                         Create either MPI derived datatype for each chunk to do collective IO or
 *                         just do independent IO or independent IO with file set view
 *                      3. Set up collective IO property list for collective mode
 *                      4. DO IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 * Modification:
 *  - Refctore to remove multi-chunk-without-opimization feature and update for
 *    multi-chunk-io accordingly
 * Programmer: Jonathan Kim
 * Date: 2012-10-10
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    H5D_chunk_map_t *fm)
{
    H5P_genplist_t *dx_plist;           /* Pointer to DXPL */
    H5FD_mpio_chunk_opt_t chunk_opt_mode;
    int         io_option = H5D_MULTI_CHUNK_IO_MORE_OPT;
    int         sum_chunk = -1;
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    htri_t      temp_not_link_io = FALSE;
#endif
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(io_info);
    HDassert(io_info->using_mpi_vfd);
    HDassert(type_info);
    HDassert(fm);

    /* Obtain the data transfer properties */
    if(NULL == (dx_plist = H5I_object(io_info->raw_dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* Check the optional property list on what to do with collective chunk IO. */
    if(H5P_get(dx_plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME, &chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't get chunk optimization option")
    if(H5FD_MPIO_CHUNK_ONE_IO == chunk_opt_mode)
        io_option = H5D_ONE_LINK_CHUNK_IO;      /*no opt*/
    /* direct request to multi-chunk-io */
    else if(H5FD_MPIO_CHUNK_MULTI_IO == chunk_opt_mode)
        io_option = H5D_MULTI_CHUNK_IO;         
    /* via default path. branch by num threshold */
    else {
        unsigned one_link_chunk_io_threshold;   /* Threshhold to use single collective I/O for all chunks */
        int mpi_size;                   /* Number of processes in MPI job */

        if(H5D__mpio_get_sum_chunk(io_info, fm, &sum_chunk) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL, "unable to obtain the total chunk number of all processes");
        if((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
            HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

        /* Get the chunk optimization option */
        if(H5P_get(dx_plist, H5D_XFER_MPIO_CHUNK_OPT_NUM_NAME, &one_link_chunk_io_threshold) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't get chunk optimization option")

        /* step 1: choose an IO option */
        /* If the average number of chunk per process is greater than a threshold, we will do one link chunked IO. */
        if((unsigned)sum_chunk / mpi_size >= one_link_chunk_io_threshold)
            io_option = H5D_ONE_LINK_CHUNK_IO_MORE_OPT;
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
        else
            temp_not_link_io = TRUE;
#endif
    } /* end else */

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
{
    H5P_genplist_t    *plist;           /* Property list pointer */
    htri_t            check_prop;
    int               new_value;

    /* Get the dataset transfer property list */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(io_info->raw_dxpl_id)))
        HGOTO_ERROR(H5E_IO, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    /*** Test collective chunk user-input optimization APIs. ***/
    check_prop = H5P_exist_plist(plist, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME);
    if(check_prop > 0) {
        if(H5D_ONE_LINK_CHUNK_IO == io_option) {
            new_value = 0;
            if(H5P_set(plist, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME, &new_value) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
    } /* end if */
    check_prop = H5P_exist_plist(plist, H5D_XFER_COLL_CHUNK_MULTI_HARD_NAME);
    if(check_prop > 0) {
        if(H5D_MULTI_CHUNK_IO == io_option) {
            new_value = 0;
            if(H5P_set(plist, H5D_XFER_COLL_CHUNK_MULTI_HARD_NAME, &new_value) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
    } /* end if */
    check_prop = H5P_exist_plist(plist, H5D_XFER_COLL_CHUNK_LINK_NUM_TRUE_NAME);
    if(check_prop > 0) {
        if(H5D_ONE_LINK_CHUNK_IO_MORE_OPT == io_option) {
            new_value = 0;
            if(H5P_set(plist, H5D_XFER_COLL_CHUNK_LINK_NUM_TRUE_NAME, &new_value) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
    } /* end if */
    check_prop = H5P_exist_plist(plist, H5D_XFER_COLL_CHUNK_LINK_NUM_FALSE_NAME);
    if(check_prop > 0) {
        if(temp_not_link_io) {
            new_value = 0;
            if(H5P_set(plist, H5D_XFER_COLL_CHUNK_LINK_NUM_FALSE_NAME, &new_value) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
    } /* end if */
}
#endif

    /* step 2:  Go ahead to do IO.*/
    switch (io_option) {
        case H5D_ONE_LINK_CHUNK_IO:
        case H5D_ONE_LINK_CHUNK_IO_MORE_OPT:
            if(io_info->dset->shared->dcpl_cache.pline.nused > 0) {
                if(H5D__link_chunk_filtered_collective_io(io_info, type_info, fm, dx_plist) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish filtered linked chunk MPI-IO")
            }
            else {
                if(H5D__link_chunk_collective_io(io_info, type_info, fm, sum_chunk, dx_plist) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish linked chunk MPI-IO")
            }
            break;

        case H5D_MULTI_CHUNK_IO: /* direct request to do multi-chunk IO */
        default:                 /* multiple chunk IO via threshold */
            if(io_info->dset->shared->dcpl_cache.pline.nused > 0) {
                if(H5D__multi_chunk_filtered_collective_io(io_info, type_info, fm, dx_plist) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish optimized multiple filtered chunk MPI-IO")
            }
            else {
                if(H5D__multi_chunk_collective_io(io_info, type_info, fm, dx_plist) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish optimized multiple chunk MPI-IO")
            }
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_collective_io */


/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_collective_read
 *
 * Purpose:     Reads directly from chunks in file into application memory
 *              using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_collective_read(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t H5_ATTR_UNUSED nelmts, const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space,
    H5D_chunk_map_t *fm)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if(H5D__chunk_collective_io(io_info, type_info, fm) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_collective_read() */


/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_collective_write
 *
 * Purpose:     Write directly to chunks in file from application memory
 *              using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_collective_write(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t H5_ATTR_UNUSED nelmts, const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space,
    H5D_chunk_map_t *fm)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

#ifdef PARALLEL_COMPRESS_DEBUG
    char name[10];

    snprintf(name, 10, "out - %d", H5F_mpi_get_rank(io_info->dset->oloc.file));

    debug_file = fopen(name, "w");
#endif

    /* Call generic selection operation */
    if(H5D__chunk_collective_io(io_info, type_info, fm) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_collective_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__link_chunk_collective_io
 *
 * Purpose:     Routine for one collective IO with one MPI derived datatype to link with all chunks
 *
 *                      1. Sort the chunk address and chunk info
 *                      2. Build up MPI derived datatype for each chunk
 *                      3. Build up the final MPI derived datatype
 *                      4. Use common collective IO routine to do MPI-IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 * Modification:
 *  - Set H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME and H5D_MPIO_ACTUAL_IO_MODE_NAME
 *    dxpl in this.
 * Programmer: Jonathan Kim
 * Date: 2012-10-10
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__link_chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    H5D_chunk_map_t *fm, int sum_chunk, H5P_genplist_t *dx_plist)
{
    H5D_chunk_addr_info_t *chunk_addr_info_array = NULL;
    MPI_Datatype chunk_final_mtype;         /* Final memory MPI datatype for all chunks with seletion */
    hbool_t chunk_final_mtype_is_derived = FALSE;
    MPI_Datatype chunk_final_ftype;         /* Final file MPI datatype for all chunks with seletion */
    hbool_t chunk_final_ftype_is_derived = FALSE;
    H5D_storage_t ctg_store;                /* Storage info for "fake" contiguous dataset */
    size_t              total_chunks;
    haddr_t            *total_chunk_addr_array = NULL;
    MPI_Datatype       *chunk_mtype = NULL;
    MPI_Datatype       *chunk_ftype = NULL;
    MPI_Aint           *chunk_disp_array = NULL;
    MPI_Aint           *chunk_mem_disp_array = NULL;
    hbool_t            *chunk_mft_is_derived_array = NULL;      /* Flags to indicate each chunk's MPI file datatype is derived */
    hbool_t            *chunk_mbt_is_derived_array = NULL;      /* Flags to indicate each chunk's MPI memory datatype is derived */
    int                *chunk_mpi_file_counts = NULL;   /* Count of MPI file datatype for each chunk */
    int                *chunk_mpi_mem_counts = NULL;    /* Count of MPI memory datatype for each chunk */
    int                 mpi_code;           /* MPI return code */
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode = H5D_MPIO_LINK_CHUNK;
    H5D_mpio_actual_io_mode_t actual_io_mode = H5D_MPIO_CHUNK_COLLECTIVE;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Set the actual-chunk-opt-mode property. */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")

    /* Set the actual-io-mode property.
     * Link chunk I/O does not break to independent, so can set right away */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Get the sum # of chunks, if not already available */
    if(sum_chunk < 0) {
        if(H5D__mpio_get_sum_chunk(io_info, fm, &sum_chunk) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL, "unable to obtain the total chunk number of all processes");
    } /* end if */

    /* Retrieve total # of chunks in dataset */
    H5_CHECKED_ASSIGN(total_chunks, size_t, fm->layout->u.chunk.nchunks, hsize_t);

    /* Handle special case when dataspace dimensions only allow one chunk in
     *  the dataset.  [This sometimes is used by developers who want the
     *  equivalent of compressed contiguous datasets - QAK]
     */
    if(total_chunks == 1) {
        H5SL_node_t *chunk_node;        /* Pointer to chunk node for selection */
        H5S_t *fspace;                  /* Dataspace describing chunk & selection in it */
        H5S_t *mspace;                  /* Dataspace describing selection in memory corresponding to this chunk */

        /* Check for this process having selection in this chunk */
        chunk_node = H5SL_first(fm->sel_chunks);

        if(chunk_node == NULL) {
            /* Set the dataspace info for I/O to NULL, this process doesn't have any I/O to perform */
            fspace = mspace = NULL;

            /* Initialize chunk address */
            ctg_store.contig.dset_addr = 0;
        } /* end if */
        else {
            H5D_chunk_ud_t udata;           /* User data for querying chunk info */
            H5D_chunk_info_t *chunk_info;   /* Info for chunk in skiplist */

            /* Get the chunk info, for the selection in the chunk */
            if(NULL == (chunk_info = (H5D_chunk_info_t *)H5SL_item(chunk_node)))
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk info from skip list")

            /* Set the dataspace info for I/O */
            fspace = chunk_info->fspace;
            mspace = chunk_info->mspace;

            /* Look up address of chunk */
            if(H5D__chunk_lookup(io_info->dset, io_info->md_dxpl_id, chunk_info->scaled, &udata) < 0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk address")
            ctg_store.contig.dset_addr = udata.chunk_block.offset;
        } /* end else */

        /* Set up the base storage address for this chunk */
        io_info->store = &ctg_store;

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before inter_collective_io for total chunk = 1 \n");
#endif

        /* Perform I/O */
        if(H5D__inter_collective_io(io_info, type_info, fspace, mspace) < 0)
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't finish shared collective MPI-IO")
    } /* end if */
    else {
        hsize_t mpi_buf_count;  /* Number of MPI types */
        size_t num_chunk;       /* Number of chunks for this process */
        size_t u;               /* Local index variable */

        /* Get the number of chunks with a selection */
        num_chunk = H5SL_count(fm->sel_chunks);
        H5_CHECK_OVERFLOW(num_chunk, size_t, int);

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"total_chunks = %Zu, num_chunk = %Zu\n", total_chunks, num_chunk);
#endif

        /* Set up MPI datatype for chunks selected */
        if(num_chunk) {
            /* Allocate chunking information */
            if(NULL == (chunk_addr_info_array = (H5D_chunk_addr_info_t *)H5MM_malloc(num_chunk * sizeof(H5D_chunk_addr_info_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk array buffer")
            if(NULL == (chunk_mtype           = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory datatype buffer")
            if(NULL == (chunk_ftype           = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file datatype buffer")
            if(NULL == (chunk_disp_array      = (MPI_Aint *)H5MM_malloc(num_chunk * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file displacement buffer")
            if(NULL == (chunk_mem_disp_array  = (MPI_Aint *)H5MM_calloc(num_chunk * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory displacement buffer")
            if(NULL == (chunk_mpi_mem_counts        = (int *)H5MM_calloc(num_chunk * sizeof(int))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory counts buffer")
            if(NULL == (chunk_mpi_file_counts       = (int *)H5MM_calloc(num_chunk * sizeof(int))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file counts buffer")
            if(NULL == (chunk_mbt_is_derived_array  = (hbool_t *)H5MM_calloc(num_chunk * sizeof(hbool_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory is derived datatype flags buffer")
            if(NULL == (chunk_mft_is_derived_array  = (hbool_t *)H5MM_calloc(num_chunk * sizeof(hbool_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file is derived datatype flags buffer")

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before sorting the chunk address \n");
#endif
            /* Sort the chunk address */
            if(H5D__sort_chunk(io_info, fm, chunk_addr_info_array, sum_chunk) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL, "unable to sort chunk address")
            ctg_store.contig.dset_addr = chunk_addr_info_array[0].chunk_addr;

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"after sorting the chunk address \n");
#endif

            /* Obtain MPI derived datatype from all individual chunks */
            for(u = 0; u < num_chunk; u++) {
                hsize_t *permute_map = NULL; /* array that holds the mapping from the old, 
                                                out-of-order displacements to the in-order 
                                                displacements of the MPI datatypes of the 
                                                point selection of the file space */
                hbool_t is_permuted = FALSE;

                /* Obtain disk and memory MPI derived datatype */
                /* NOTE: The permute_map array can be allocated within H5S_mpio_space_type
                 *              and will be fed into the next call to H5S_mpio_space_type
                 *              where it will be freed.
                 */
                if(H5S_mpio_space_type(chunk_addr_info_array[u].chunk_info.fspace,
                                       type_info->src_type_size, 
                                       &chunk_ftype[u], /* OUT: datatype created */ 
                                       &chunk_mpi_file_counts[u], /* OUT */
                                       &(chunk_mft_is_derived_array[u]), /* OUT */
                                       TRUE, /* this is a file space,
                                                so permute the
                                                datatype if the point
                                                selections are out of
                                                order */
                                       &permute_map,/* OUT: a map to indicate the
                                                       permutation of points
                                                       selected in case they
                                                       are out of order */
                                       &is_permuted /* OUT */) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI file type")
                /* Sanity check */
                if(is_permuted)
                    HDassert(permute_map);
                if(H5S_mpio_space_type(chunk_addr_info_array[u].chunk_info.mspace,
                                       type_info->dst_type_size, &chunk_mtype[u], 
                                       &chunk_mpi_mem_counts[u], 
                                       &(chunk_mbt_is_derived_array[u]), 
                                       FALSE, /* this is a memory
                                                 space, so if the file
                                                 space is not
                                                 permuted, there is no
                                                 need to permute the
                                                 datatype if the point
                                                 selections are out of
                                                 order*/
                                       &permute_map, /* IN: the permutation map
                                                        generated by the
                                                        file_space selection
                                                        and applied to the
                                                        memory selection */
                                       &is_permuted /* IN */) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI buf type")
                /* Sanity check */
                if(is_permuted)
                    HDassert(!permute_map);

                /* Chunk address relative to the first chunk */
                chunk_addr_info_array[u].chunk_addr -= ctg_store.contig.dset_addr;

                /* Assign chunk address to MPI displacement */
                /* (assume MPI_Aint big enough to hold it) */
                chunk_disp_array[u] = (MPI_Aint)chunk_addr_info_array[u].chunk_addr;
            } /* end for */

            /* Create final MPI derived datatype for the file */
            if(MPI_SUCCESS != (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_file_counts, chunk_disp_array, chunk_ftype, &chunk_final_ftype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
            if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&chunk_final_ftype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            chunk_final_ftype_is_derived = TRUE;

            /* Create final MPI derived datatype for memory */
            if(MPI_SUCCESS != (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_mem_counts, chunk_mem_disp_array, chunk_mtype, &chunk_final_mtype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
            if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&chunk_final_mtype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            chunk_final_mtype_is_derived = TRUE;

            /* Free the file & memory MPI datatypes for each chunk */
            for(u = 0; u < num_chunk; u++) {
                if(chunk_mbt_is_derived_array[u])
                    if(MPI_SUCCESS != (mpi_code = MPI_Type_free(chunk_mtype + u)))
                        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

                if(chunk_mft_is_derived_array[u])
                    if(MPI_SUCCESS != (mpi_code = MPI_Type_free(chunk_ftype + u)))
                        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            } /* end for */

            /* We have a single, complicated MPI datatype for both memory & file */
            mpi_buf_count  = (hsize_t)1;
        } /* end if */
        else {      /* no selection at all for this process */
            /* Allocate chunking information */
            if(NULL == (total_chunk_addr_array = (haddr_t *)H5MM_malloc(sizeof(haddr_t) * total_chunks)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate total chunk address arraybuffer")

            /* Retrieve chunk address map */
            if(H5D__chunk_addrmap(io_info, total_chunk_addr_array) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")

            /* Get chunk with lowest address */
            ctg_store.contig.dset_addr = HADDR_MAX;
            for(u = 0; u < total_chunks; u++)
                if(total_chunk_addr_array[u] < ctg_store.contig.dset_addr)
                    ctg_store.contig.dset_addr = total_chunk_addr_array[u];
            HDassert(ctg_store.contig.dset_addr != HADDR_MAX);

            /* Set the MPI datatype */
            chunk_final_ftype = MPI_BYTE;
            chunk_final_mtype = MPI_BYTE;

            /* No chunks selected for this process */
            mpi_buf_count  = (hsize_t)0;
        } /* end else */
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before coming to final collective IO\n");
#endif

        /* Set up the base storage address for this chunk */
        io_info->store = &ctg_store;

        /* Perform final collective I/O operation */
        if(H5D__final_collective_io(io_info, type_info, mpi_buf_count, &chunk_final_ftype, &chunk_final_mtype) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")
    } /* end else */

done:
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before freeing memory inside H5D_link_collective_io ret_value = %d\n", ret_value);
#endif
    /* Release resources */
    if(total_chunk_addr_array)
        H5MM_xfree(total_chunk_addr_array);
    if(chunk_addr_info_array)
        H5MM_xfree(chunk_addr_info_array);
    if(chunk_mtype)
        H5MM_xfree(chunk_mtype);
    if(chunk_ftype)
        H5MM_xfree(chunk_ftype);
    if(chunk_disp_array)
        H5MM_xfree(chunk_disp_array);
    if(chunk_mem_disp_array)
        H5MM_xfree(chunk_mem_disp_array);
    if(chunk_mpi_mem_counts)
        H5MM_xfree(chunk_mpi_mem_counts);
    if(chunk_mpi_file_counts)
        H5MM_xfree(chunk_mpi_file_counts);
    if(chunk_mbt_is_derived_array)
        H5MM_xfree(chunk_mbt_is_derived_array);
    if(chunk_mft_is_derived_array)
        H5MM_xfree(chunk_mft_is_derived_array);

    /* Free the MPI buf and file types, if they were derived */
    if(chunk_final_mtype_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_final_mtype)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if(chunk_final_ftype_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_final_ftype)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__link_chunk_collective_io */


/*-------------------------------------------------------------------------
 * Function:    H5D__link_chunk_filtered_collective_io
 *
 * Purpose:     Routine for one collective IO with one MPI derived datatype
 *              to link with all filtered chunks
 *
 *              XXX: Update later to reflect changes in structure
 *
 *              1. Construct a list of selected chunks in the collective IO
 *                 operation
 *                 A. If any chunk is being written to by more than 1
 *                    process, the process writing the most data to the
 *                    chunk will take ownership of the chunk (the first
 *                    process seen that is writing the most data becomes
 *                    the new owner in the case of ties)
 *              2. If the operation is a write operation
 *                 A. Loop through each chunk in the operation
 *                    I. If this is not a full overwrite of the chunk
 *                       a) Read the chunk from file and pass the chunk
 *                          through the filter pipeline in reverse order
 *                          (Unfilter the chunk)
 *                    II. Update the chunk data with the modifications from
 *                        the owning process
 *                    III. Receive any modification data from other
 *                         processes and update the chunk data with these
 *                         modifications
 *                    IV. Filter the chunk
 *                 B. Contribute the modified chunks to an array gathered
 *                    by all processes which contains the new sizes of
 *                    every chunk modified in the collective IO operation
 *                 C. All processes collectively re-allocate each chunk
 *                    from the gathered array with their new sizes after
 *                    the filter operation
 *                 D. If this process has any chunks selected in the IO
 *                    operation, create an MPI derived type for memory and
 *                    file to write out the process' selected chunks to the
 *                    file
 *                 E. Perform the collective write
 *                 F. All processes collectively re-insert each modified
 *                    chunk from the gathered array into the chunk index
 *              3. If the operation is a read operation
 *                 A. Loop through each chunk in the operation
 *                    I.
 *
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jordan Henderson
 *              Friday, Nov. 4th, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__link_chunk_filtered_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    H5D_chunk_map_t *fm, H5P_genplist_t *dx_plist)
{
    H5D_filtered_collective_io_info_t *chunk_list = NULL; /* The list of chunks being read/written */
    H5D_filtered_collective_io_info_t *collective_chunk_list = NULL;
    H5D_mpio_actual_chunk_opt_mode_t   actual_chunk_opt_mode = H5D_MPIO_LINK_CHUNK;
    H5D_mpio_actual_io_mode_t          actual_io_mode = H5D_MPIO_CHUNK_COLLECTIVE;
    H5D_storage_t                      ctg_store;
    MPI_Datatype                       mem_type = MPI_BYTE;
    MPI_Datatype                       file_type = MPI_BYTE;
    hbool_t                            mem_type_is_derived = FALSE;
    hbool_t                            file_type_is_derived = FALSE;
    size_t                             chunk_list_num_entries;
    size_t                             collective_chunk_list_num_entries;
    size_t                            *num_chunks_selected_array = NULL;
    size_t                             i;
    int                                mpi_rank, mpi_size, mpi_code;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Obtain the current rank of the process and the number of processes */
    if ((mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi rank")
    if ((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

    /* Set the actual-chunk-opt-mode property. */
    if (H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")

    /* Set the actual-io-mode property.
     * Link chunk I/O does not break to independent, so can set right away */
    if (H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Build a list of selected chunks in the collective io operation */
    /* XXX: Not sure about correct minor error code */
    if (H5D__construct_filtered_io_info_list(io_info, type_info, fm, &chunk_list, &chunk_list_num_entries, &num_chunks_selected_array) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't construct filtered I/O info list")

#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, "Incoming messages from other processes:\n");
    HDfprintf(debug_file, "-----------------------------------------\n");
    for (size_t j = 0; j < chunk_list_num_entries; j++) {
        HDfprintf(debug_file, "| Owner of chunk at address %a is expecting messages from %d other processes.\n",
                chunk_list[j].old_chunk.offset, chunk_list[j].num_writers - 1);
    }
    HDfprintf(debug_file, "-----------------------------------------\n\n");
#endif

    if (io_info->op_type == H5D_IO_OP_WRITE) { /* Filtered collective write */
        H5D_chk_idx_info_t index_info;
        H5D_chunk_ud_t     udata;
        hsize_t            mpi_buf_count;

        /* Construct chunked index info */
        index_info.f = io_info->dset->oloc.file;
        index_info.dxpl_id = io_info->md_dxpl_id;
        index_info.pline = &(io_info->dset->shared->dcpl_cache.pline);
        index_info.layout = &(io_info->dset->shared->layout.u.chunk);
        index_info.storage = &(io_info->dset->shared->layout.storage.u.chunk);

        /* Set up chunk information for insertion to chunk index */
        udata.common.layout = index_info.layout;
        udata.common.storage = index_info.storage;
        udata.filter_mask = 0;


#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "Processing chunks:\n");
        HDfprintf(debug_file, "---------------------------------------------------\n");
#endif

        /* Iterate through all the chunks in the collective write operation,
         * updating each chunk with the data modifications from other processes,
         * then re-filtering the chunk. */
        /* XXX: Not sure about minor error code */
        for (i = 0; i < chunk_list_num_entries; i++)
            if (H5D__update_filtered_collective_chunk_entry(&chunk_list[i], io_info, type_info) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't update filtered chunk entry")

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "---------------------------------------------------\n\n");
#endif

        /* Gather the new chunk sizes to all processes for a collective reallocation
         * of the chunks in the file */
        /* XXX: change minor error code */
        if (H5D__mpio_array_gather(io_info, chunk_list, chunk_list_num_entries, sizeof(*chunk_list),
                (void **) &collective_chunk_list, &collective_chunk_list_num_entries, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't gather new chunk sizes")

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "Reallocing chunks:\n");
        HDfprintf(debug_file, "------------------------------\n");
#endif

        /* Collectively re-allocate the modified chunks (from each process) in the file */
        for (i = 0; i < collective_chunk_list_num_entries; i++) {
            hbool_t insert;

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "| Reallocing chunk at address %a with new length of %zd.\n", collective_chunk_list[i].new_chunk.offset, collective_chunk_list[i].new_chunk.length);
#endif

            if (H5D__chunk_file_alloc(&index_info, &collective_chunk_list[i].old_chunk, &collective_chunk_list[i].new_chunk,
                    &insert, collective_chunk_list[i].chunk_info.scaled) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "| - Chunk now at address %a.\n|\n", collective_chunk_list[i].new_chunk);
#endif
        } /* end for */

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "------------------------------\n\n");
#endif

        /* If this process has any chunks selected, create a MPI type for collectively
         * writing out the chunks to file. Otherwise, the process contributes to the
         * collective write with a none type. */
        if (chunk_list_num_entries) {
            size_t offset;

            /* XXX: During the collective re-allocation of chunks in the file, the record for each
             * chunk is only update in the total array, not in the local copy of chunks on each
             * process. However, each process needs the updated chunk records so that they can create
             * a MPI type for the collective write that will write to the chunk's new locations instead
             * of the old ones. This ugly hack seems to be the best solution to copy the information
             * back to the local array and avoid having to modify the collective write type function
             * in an ugly way so that it will accept the total array instead of the local array.
             * This works correctly because the array gather function guarantees that the chunk
             * data in the total array is ordered in blocks by rank.
             */
            /* XXX: No need to use bytes here, should be able to simply find offset in
             * terms of H5D_filtered_collective_io_info_t's */
            offset = 0;
            for (i = 0; i < (size_t) mpi_rank; i++)
                offset += num_chunks_selected_array[i] * sizeof(H5D_filtered_collective_io_info_t);

            HDmemcpy(chunk_list, &((char *) collective_chunk_list)[offset], num_chunks_selected_array[mpi_rank] * sizeof(H5D_filtered_collective_io_info_t));

            /* Create single MPI type encompassing each selection in the dataspace */
            /* XXX: change minor error code */
            if (H5D__mpio_filtered_collective_write_type(chunk_list, chunk_list_num_entries,
                    &mem_type, &mem_type_is_derived, &file_type, &file_type_is_derived) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't create MPI link chunk I/O type")

            /* Override the write buffer to point to the address of the first
             * chunk data buffer */
            io_info->u.wbuf = chunk_list[0].buf;
        } /* end if */

        /* We have a single, complicated MPI datatype for both memory & file */
        mpi_buf_count = (mem_type_is_derived && file_type_is_derived) ? (hsize_t) 1 : (hsize_t) 0;

        /* Set up the base storage address for this operation */
        ctg_store.contig.dset_addr = 0;
        io_info->store = &ctg_store;

        /* Perform I/O */
        if (H5D__final_collective_io(io_info, type_info, mpi_buf_count, &file_type, &mem_type) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")

        /* Participate in the collective re-insertion of all chunks modified
         * in this iteration into the chunk index */
#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "Reinserting chunks into chunk index.\n");
        HDfprintf(debug_file, "---------------------------------------\n");
#endif

        for (i = 0; i < collective_chunk_list_num_entries; i++) {
            udata.chunk_block = collective_chunk_list[i].new_chunk;
            udata.common.scaled = collective_chunk_list[i].chunk_info.scaled;

            if ((index_info.storage->ops->insert)(&index_info, &udata, io_info->dset) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to insert chunk address into index")

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "| Successfully inserted chunk at address %a into the chunk index.\n", udata.chunk_block.offset);
#endif
        } /* end for */

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "---------------------------------------\n");
#endif
    } /* end if */
    else { /* Filtered collective read */

    } /* end else */

done:
    /* Free resources used by a process which had some selection */
    if (chunk_list) {
        for (i = 0; i < chunk_list_num_entries; i++)
            if (chunk_list[i].buf)
                H5MM_free(chunk_list[i].buf);

        H5MM_free(chunk_list);
    }

    if (num_chunks_selected_array)
        H5MM_free(num_chunks_selected_array);
    if (collective_chunk_list)
        H5MM_free(collective_chunk_list);

    /* Free the MPI buf and file types, if they were derived */
    if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__link_chunk_filtered_collective_io() */


/*-------------------------------------------------------------------------
 * Function:    H5D__multi_chunk_collective_io
 *
 * Purpose:     To do IO per chunk according to IO mode(collective/independent/none)
 *
 *              1. Use MPI_gather and MPI_Bcast to obtain IO mode in each chunk(collective/independent/none)
 *              2. Depending on whether the IO mode is collective or independent or none,
 *                 Create either MPI derived datatype for each chunk or just do independent IO
 *              3. Use common collective IO routine to do MPI-IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 * Modification:
 *  - Set H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME dxpl in this to go along with
 *    setting H5D_MPIO_ACTUAL_IO_MODE_NAME dxpl at the bottom.
 * Programmer: Jonathan Kim
 * Date: 2012-10-10
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__multi_chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    H5D_chunk_map_t *fm, H5P_genplist_t *dx_plist)
{
    H5D_io_info_t       ctg_io_info;          /* Contiguous I/O info object */
    H5D_storage_t       ctg_store;            /* Chunk storage information as contiguous dataset */
    H5D_io_info_t       cpt_io_info;          /* Compact I/O info object */
    H5D_storage_t       cpt_store;            /* Chunk storage information as compact dataset */
    hbool_t             cpt_dirty;            /* Temporary placeholder for compact storage "dirty" flag */
    uint8_t            *chunk_io_option = NULL;
    haddr_t            *chunk_addr = NULL;
    H5D_storage_t       store;                /* union of EFL and chunk pointer in file space */
    H5FD_mpio_xfer_t    last_xfer_mode = H5FD_MPIO_COLLECTIVE; /* Last parallel transfer for this request (H5D_XFER_IO_XFER_MODE_NAME) */
    H5FD_mpio_collective_opt_t last_coll_opt_mode = H5FD_MPIO_COLLECTIVE_IO; /* Last parallel transfer with independent IO or collective IO with this mode */
    size_t              total_chunk;          /* Total # of chunks in dataset */
#ifdef H5Dmpio_DEBUG
    int mpi_rank;
#endif
    size_t              u;                    /* Local index variable */
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode = H5D_MPIO_MULTI_CHUNK;  /* actual chunk optimization mode */
    H5D_mpio_actual_io_mode_t actual_io_mode = H5D_MPIO_NO_COLLECTIVE; /* Local variable for tracking the I/O mode used. */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Set the actual chunk opt mode property */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")

#ifdef H5Dmpio_DEBUG
    mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file);
#endif

    /* Retrieve total # of chunks in dataset */
    H5_CHECKED_ASSIGN(total_chunk, size_t, fm->layout->u.chunk.nchunks, hsize_t);
    HDassert(total_chunk != 0);

    /* Allocate memories */
    chunk_io_option = (uint8_t *)H5MM_calloc(total_chunk);
    chunk_addr      = (haddr_t *)H5MM_calloc(total_chunk * sizeof(haddr_t));
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D), "total_chunk %Zu\n", total_chunk);
#endif

    /* Obtain IO option for each chunk */
    if(H5D__obtain_mpio_mode(io_info, fm, dx_plist, chunk_io_option, chunk_addr) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "unable to obtain MPIO mode")

    /* Set up contiguous I/O info object */
    HDmemcpy(&ctg_io_info, io_info, sizeof(ctg_io_info));
    ctg_io_info.store = &ctg_store;
    ctg_io_info.layout_ops = *H5D_LOPS_CONTIG;

    /* Initialize temporary contiguous storage info */
    ctg_store.contig.dset_size = (hsize_t)io_info->dset->shared->layout.u.chunk.size;

    /* Set up compact I/O info object */
    HDmemcpy(&cpt_io_info, io_info, sizeof(cpt_io_info));
    cpt_io_info.store = &cpt_store;
    cpt_io_info.layout_ops = *H5D_LOPS_COMPACT;

    /* Initialize temporary compact storage info */
    cpt_store.compact.dirty = &cpt_dirty;

    /* Set dataset storage for I/O info */
    io_info->store = &store;

    /* Loop over _all_ the chunks */
    for(u = 0; u < total_chunk; u++) {
        H5D_chunk_info_t *chunk_info;    /* Chunk info for current chunk */
        H5S_t *fspace;              /* Dataspace describing chunk & selection in it */
        H5S_t *mspace;              /* Dataspace describing selection in memory corresponding to this chunk */

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"mpi_rank = %d, chunk index = %Zu\n", mpi_rank, u);
#endif
        /* Get the chunk info for this chunk, if there are elements selected */
        chunk_info = fm->select_chunk[u];

        /* Set the storage information for chunks with selections */
        if(chunk_info) {
            HDassert(chunk_info->index == u);

            /* Pass in chunk's coordinates in a union. */
            store.chunk.scaled  = chunk_info->scaled;
        } /* end if */

        /* Collective IO for this chunk,
         * Note: even there is no selection for this process, the process still
         *      needs to contribute MPI NONE TYPE.
         */
        if(chunk_io_option[u] == H5D_CHUNK_IO_MODE_COL) {
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"inside collective chunk IO mpi_rank = %d, chunk index = %Zu\n", mpi_rank, u);
#endif

            /* Set the file & memory dataspaces */
            if(chunk_info) {
                fspace = chunk_info->fspace;
                mspace = chunk_info->mspace;

                /* Update the local variable tracking the dxpl's actual io mode property.
                 *
                 * Note: H5D_MPIO_COLLECTIVE_MULTI | H5D_MPIO_INDEPENDENT = H5D_MPIO_MIXED
                 *      to ease switching between to mixed I/O without checking the current
                 *      value of the property. You can see the definition in H5Ppublic.h
                 */
                actual_io_mode = actual_io_mode | H5D_MPIO_CHUNK_COLLECTIVE;

            } /* end if */
            else {
                fspace = mspace = NULL;
            } /* end else */

            /* Switch back to collective I/O */
            if(last_xfer_mode != H5FD_MPIO_COLLECTIVE) {
                if(H5D__ioinfo_xfer_mode(io_info, dx_plist, H5FD_MPIO_COLLECTIVE) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't switch to collective I/O")
                last_xfer_mode = H5FD_MPIO_COLLECTIVE;
            } /* end if */
            if(last_coll_opt_mode != H5FD_MPIO_COLLECTIVE_IO) {
                if(H5D__ioinfo_coll_opt_mode(io_info, dx_plist, H5FD_MPIO_COLLECTIVE_IO) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't switch to collective I/O")
                last_coll_opt_mode = H5FD_MPIO_COLLECTIVE_IO;
            } /* end if */

            /* Initialize temporary contiguous storage address */
            ctg_store.contig.dset_addr = chunk_addr[u];

            /* Perform the I/O */
            if(H5D__inter_collective_io(&ctg_io_info, type_info, fspace, mspace) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish shared collective MPI-IO")
        } /* end if */
        else {  /* possible independent IO for this chunk */
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"inside independent IO mpi_rank = %d, chunk index = %Zu\n", mpi_rank, u);
#endif

            HDassert(chunk_io_option[u] == 0);

            /* Set the file & memory dataspaces */
            if(chunk_info) {
                fspace = chunk_info->fspace;
                mspace = chunk_info->mspace;

                /* Update the local variable tracking the dxpl's actual io mode. */
                actual_io_mode = actual_io_mode | H5D_MPIO_CHUNK_INDEPENDENT;
            } /* end if */
            else {
                fspace = mspace = NULL;
            } /* end else */

            /* Using independent I/O with file setview.*/
            if(last_coll_opt_mode != H5FD_MPIO_INDIVIDUAL_IO) {
                if(H5D__ioinfo_coll_opt_mode(io_info, dx_plist, H5FD_MPIO_INDIVIDUAL_IO) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't switch to individual I/O")
                last_coll_opt_mode = H5FD_MPIO_INDIVIDUAL_IO;
            } /* end if */

            /* Initialize temporary contiguous storage address */
            ctg_store.contig.dset_addr = chunk_addr[u];

            /* Perform the I/O */
            if(H5D__inter_collective_io(&ctg_io_info, type_info, fspace, mspace) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish shared collective MPI-IO")
#ifdef H5D_DEBUG
  if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"after inter collective IO\n");
#endif
        } /* end else */
    } /* end for */

    /* Write the local value of actual io mode to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

done:
    if(chunk_io_option)
        H5MM_xfree(chunk_io_option);
    if(chunk_addr)
        H5MM_xfree(chunk_addr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__multi_chunk_collective_io */


/*-------------------------------------------------------------------------
 * Function:    H5D__multi_chunk_filtered_collective_io
 *
 * Purpose:     To do filtered collective IO per chunk to save on memory,
 *              as opposed to collective IO of every chunk at once
 *
 *              XXX: Update later to reflect changes in structure
 *              XXX: Add read operation description
 *
 *              1. Construct a list of selected chunks in the collective IO
 *                 operation
 *                 A. If any chunk is being written to by more than 1
 *                    process, the process writing the most data to the
 *                    chunk will take ownership of the chunk (the first
 *                    process seen that is writing the most data becomes
 *                    the new owner in the case of ties)
 *              2. If the operation is a write operation
 *                 A. Loop through each chunk in the operation
 *                    I. If this is not a full overwrite of the chunk
 *                       a) Read the chunk from file and pass the chunk
 *                          through the filter pipeline in reverse order
 *                          (Unfilter the chunk)
 *                    II. Update the chunk data with the modifications from
 *                        the owning process
 *                    III. Receive any modification data from other
 *                         processes and update the chunk data with these
 *                         modifications
 *                    III. Filter the chunk
 *                    IV. Contribute the chunk to an array gathered by
 *                        all processes which contains every chunk
 *                        modified in this iteration (up to one chunk
 *                        per process, some processes may not have a
 *                        selection/may have less chunks to work on than
 *                        other processes)
 *                    II. All processes collectively re-allocate each
 *                        chunk from the gathered array with their new
 *                        sizes after the filter operation
 *                    IV. Proceed with the collective write operation
 *                        for the chunks modified on this iteration
 *                    V. All processes collectively re-insert each
 *                       chunk from the gathered array into the chunk
 *                       index
 *              3. If the operation is a read operation
 *                    I.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jordan Henderson
 *              Friday, Dec. 2nd, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__multi_chunk_filtered_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    H5D_chunk_map_t *fm, H5P_genplist_t *dx_plist)
{
    H5D_filtered_collective_io_info_t *chunk_list = NULL; /* The list of chunks being read/written */
    H5D_filtered_collective_io_info_t *collective_chunk_list = NULL;
    H5D_mpio_actual_chunk_opt_mode_t   actual_chunk_opt_mode = H5D_MPIO_MULTI_CHUNK;  /* actual chunk optimization mode */
    H5D_mpio_actual_io_mode_t          actual_io_mode = H5D_MPIO_CHUNK_COLLECTIVE; /* Local variable for tracking the I/O mode used. */
    H5D_storage_t                      store;                /* union of EFL and chunk pointer in file space */
    H5D_io_info_t                      ctg_io_info;          /* Contiguous I/O info object */
    H5D_storage_t                      ctg_store;            /* Chunk storage information as contiguous dataset */
    MPI_Datatype                       file_type = MPI_BYTE;
    MPI_Datatype                       mem_type = MPI_BYTE;
    hbool_t                            file_type_is_derived = FALSE;
    hbool_t                            mem_type_is_derived = FALSE;
    size_t                             chunk_list_num_entries;
    size_t                             collective_chunk_list_num_entries;
    size_t                            *num_chunks_selected_array = NULL;
    size_t                             i, j;                    /* Local index variable */
    int                                mpi_rank, mpi_size, mpi_code;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Obtain the current rank of the process and the number of processes */
    if ((mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi rank")
    if ((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

    /* Set the actual chunk opt mode property */
    if (H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")

    /* Set the actual_io_mode property. Filtered collective writes can't break
     * to independent, so set actual_io_mode right away */
    if (H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk io mode property")

    /* Build a list of selected chunks in the collective IO operation */
    /* XXX: change minor error code */
    if (H5D__construct_filtered_io_info_list(io_info, type_info, fm, &chunk_list, &chunk_list_num_entries, &num_chunks_selected_array) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't construct filtered I/O info list")

    /* Set up contiguous I/O info object */
    HDmemcpy(&ctg_io_info, io_info, sizeof(ctg_io_info));
    ctg_io_info.store = &ctg_store;
    ctg_io_info.layout_ops = *H5D_LOPS_CONTIG;

    /* Initialize temporary contiguous storage info */
    ctg_store.contig.dset_size = (hsize_t) io_info->dset->shared->layout.u.chunk.size;
    ctg_store.contig.dset_addr = 0;

    /* Set dataset storage for I/O info */
    io_info->store = &store;

    if (io_info->op_type == H5D_IO_OP_WRITE) { /* Filtered collective write */
        H5D_chk_idx_info_t index_info;
        H5D_chunk_ud_t     udata;
        size_t             max_num_chunks;
        hsize_t            mpi_buf_count;

        /* Construct chunked index info */
        index_info.f = io_info->dset->oloc.file;
        index_info.dxpl_id = io_info->md_dxpl_id;
        index_info.pline = &(io_info->dset->shared->dcpl_cache.pline);
        index_info.layout = &(io_info->dset->shared->layout.u.chunk);
        index_info.storage = &(io_info->dset->shared->layout.storage.u.chunk);

        /* Set up chunk information for insertion to chunk index */
        udata.common.layout = index_info.layout;
        udata.common.storage = index_info.storage;
        udata.filter_mask = 0;

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "Incoming messages from other processes:\n");
        HDfprintf(debug_file, "-----------------------------------------\n");
        for (size_t k = 0; k < chunk_list_num_entries; k++) {
            HDfprintf(debug_file, "| Owner of chunk at address %a is expecting messages from %d other processes.\n",
                    chunk_list[k].old_chunk.offset, chunk_list[k].num_writers - 1);
        }
        HDfprintf(debug_file, "-----------------------------------------\n\n");

        HDfprintf(debug_file, "Processing chunks:\n");
        HDfprintf(debug_file, "---------------------------------------------------\n");
#endif

        if (MPI_SUCCESS != (mpi_code = MPI_Allreduce(&chunk_list_num_entries, &max_num_chunks,
                1, MPI_UNSIGNED_LONG_LONG, MPI_MAX, io_info->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

        /* XXX: Iteration should be for the max number among processes, since a process could
         * have no chunks assigned to it */
        for (i = 0; i < max_num_chunks; i++) {
            /* Check if this process has a chunk to work on for this iteration */
            hbool_t have_chunk_to_process = i < chunk_list_num_entries;

            /* XXX: Not sure about minor error code */
            if (have_chunk_to_process)
                if (H5D__update_filtered_collective_chunk_entry(&chunk_list[i], io_info, type_info) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't update filtered chunk entry")

            /* Participate in the collective re-allocation of all chunks modified
             * in this iteration. Gather the new chunk sizes to all processes for
             * the collective re-allocation. */
            /* XXX: May access unavailable memory on processes with no selection */
            /* XXX: change minor error code */
            if (H5D__mpio_array_gather(io_info, &chunk_list[i], have_chunk_to_process ? 1 : 0, sizeof(*chunk_list),
                    (void **) &collective_chunk_list, &collective_chunk_list_num_entries, NULL) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't gather new chunk sizes")

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "Reallocing chunks:\n");
            HDfprintf(debug_file, "------------------------------\n");
#endif

            /* Collectively re-allocate the modified chunks (from each process) in the file */
            for (j = 0; j < collective_chunk_list_num_entries; j++) {
                hbool_t insert = FALSE;

#ifdef PARALLEL_COMPRESS_DEBUG
                HDfprintf(debug_file, "| Re-allocing chunk at address %a with new length of %llu bytes.\n",
                    collective_chunk_list[j].new_chunk.offset, collective_chunk_list[j].new_chunk.length);
#endif

                /* Collectively re-allocate the chunk in the file */
                if (H5D__chunk_file_alloc(&index_info, &collective_chunk_list[j].old_chunk, &collective_chunk_list[j].new_chunk,
                        &insert, chunk_list[j].chunk_info.scaled) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")

#ifdef PARALLEL_COMPRESS_DEBUG
                HDfprintf(debug_file, "| Chunk now at address %a.\n|\n", collective_chunk_list[j].new_chunk);
#endif
            } /* end for */

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "------------------------------\n\n");
#endif

            /* If this process has a chunk to work on, create a MPI type for the
             * memory and file for writing out the chunk */
            if (have_chunk_to_process) {
                int mpi_type_count;

                /* Collect the new chunk info back to the local copy */
                HDmemcpy(&chunk_list[i].new_chunk, &collective_chunk_list[mpi_rank].new_chunk, sizeof(chunk_list[i].new_chunk));

#ifdef PARALLEL_COMPRESS_DEBUG
                HDfprintf(debug_file, "New chunk record after memcpy back to local:\n");
                HDfprintf(debug_file, " - Chunk offset: %a, Chunk length: %lld\n", chunk_list[i].new_chunk.offset, chunk_list[i].new_chunk.length);
#endif

                H5_CHECKED_ASSIGN(mpi_type_count, int, chunk_list[i].new_chunk.length, hsize_t);

                /* Create MPI memory type for writing to chunk */
                if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous(mpi_type_count, MPI_BYTE, &mem_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code)
                if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&mem_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
                mem_type_is_derived = TRUE;

                /* Create MPI file type for writing to chunk */
                if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous(mpi_type_count, MPI_BYTE, &file_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code)
                if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&file_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
                file_type_is_derived = TRUE;

                /* Set up the base storage address for this operation */
                ctg_store.contig.dset_addr = chunk_list[i].new_chunk.offset;

                /* Override the write buffer to point to the address of the
                 * chunk data buffer */
                ctg_io_info.u.wbuf = chunk_list[i].buf;
            } /* end if */

            mpi_buf_count = (mem_type_is_derived && file_type_is_derived) ? 1 : 0;

            /* Perform the I/O */
            if (H5D__final_collective_io(&ctg_io_info, type_info, mpi_buf_count, &file_type, &mem_type) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")

            /* Participate in the collective re-insertion of all chunks modified
             * in this iteration into the chunk index */
#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "Reinserting chunks into chunk index.\n");
            HDfprintf(debug_file, "---------------------------------------\n");
#endif

            for (j = 0; j < collective_chunk_list_num_entries; j++) {
                udata.chunk_block = collective_chunk_list[j].new_chunk;
                udata.common.scaled = collective_chunk_list[j].chunk_info.scaled;

                if ((index_info.storage->ops->insert)(&index_info, &udata, io_info->dset) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to insert chunk address into index")

#ifdef PARALLEL_COMPRESS_DEBUG
                HDfprintf(debug_file, "| Successfully inserted chunk at address %a into the chunk index.\n", udata.chunk_block.offset);
#endif
            } /* end for */

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "---------------------------------------\n");
#endif

            /* Free the MPI memory and file type, if they were derived */
            /* XXX: For performance, collect each type into an array and free at end */
            if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

            mem_type_is_derived = FALSE;
            file_type_is_derived = FALSE;

            if (collective_chunk_list)
                collective_chunk_list = (H5D_filtered_collective_io_info_t *) H5MM_free(collective_chunk_list);
        } /* end for */
    } /* end if */
    else { /* Filtered collective read */
        unsigned filter_mask = 0;
        size_t   buf_size;

        for (i = 0; i < chunk_list_num_entries; i++) {
            buf_size = chunk_list[i].old_chunk.length;

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "Allocing %zd bytes for chunk read buffer.\n", buf_size);
#endif

            if (NULL == (chunk_list[i].buf = H5MM_malloc(buf_size)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk read buffer")

            /* Read the chunk from the file */
            if (H5F_block_read(io_info->dset->oloc.file, H5FD_MEM_DRAW, chunk_list[i].old_chunk.offset,
                    buf_size, H5AC_rawdata_dxpl_id, chunk_list[i].buf) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "couldn't read raw data chunk")

            /* Unfilter the chunk before modifying it */
            if (H5Z_pipeline(&io_info->dset->shared->dcpl_cache.pline, H5Z_FLAG_REVERSE, &filter_mask,
                    io_info->dxpl_cache->err_detect, io_info->dxpl_cache->filter_cb,
                    (size_t *) &chunk_list[i].old_chunk.length, &buf_size, &chunk_list[i].buf) < 0)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTFILTER, H5_ITER_ERROR, "couldn't unfilter chunk for modifying")

#ifdef PARALLEL_COMPRESS_DEBUG
            for (size_t k = 0; k < chunk_list[i].old_chunk.length / type_info->src_type_size; k++)
                HDfprintf(debug_file, "Read buf entry %d is %lld.\n", k, ((long *) chunk_list[i].buf)[k]);
#endif

            /* Scatter the unfiltered chunk data into the user's read buffer */

        } /* end for */
    } /* end else */

done:
    if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    if (chunk_list) {
        for (i = 0; i < chunk_list_num_entries; i++)
            if (chunk_list[i].buf)
                H5MM_free(chunk_list[i].buf);

        H5MM_free(chunk_list);
    }

    if (num_chunks_selected_array)
        H5MM_free(num_chunks_selected_array);
    if (collective_chunk_list)
        H5MM_free(collective_chunk_list);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__multi_chunk_filtered_collective_io() */


/*-------------------------------------------------------------------------
 * Function:    H5D__inter_collective_io
 *
 * Purpose:     Routine for the shared part of collective IO between multiple chunk
 *              collective IO and contiguous collective IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__inter_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    const H5S_t *file_space, const H5S_t *mem_space)
{
    int mpi_buf_count;                  /* # of MPI types */
    hbool_t mbt_is_derived = FALSE;
    hbool_t mft_is_derived = FALSE;
    MPI_Datatype        mpi_file_type, mpi_buf_type;
    int                 mpi_code;       /* MPI return code */
    herr_t       ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_STATIC

    if((file_space != NULL) && (mem_space != NULL)) {
        int  mpi_file_count;         /* Number of file "objects" to transfer */
        hsize_t *permute_map = NULL; /* array that holds the mapping from the old, 
                                        out-of-order displacements to the in-order 
                                        displacements of the MPI datatypes of the 
                                        point selection of the file space */
        hbool_t is_permuted = FALSE;

        /* Obtain disk and memory MPI derived datatype */
        /* NOTE: The permute_map array can be allocated within H5S_mpio_space_type
         *              and will be fed into the next call to H5S_mpio_space_type
         *              where it will be freed.
         */
        if(H5S_mpio_space_type(file_space, type_info->src_type_size, 
                               &mpi_file_type, &mpi_file_count, &mft_is_derived, /* OUT: datatype created */  
                               TRUE, /* this is a file space, so
                                        permute the datatype if the
                                        point selection is out of
                                        order */
                               &permute_map, /* OUT: a map to indicate
                                                the permutation of
                                                points selected in
                                                case they are out of
                                                order */ 
                               &is_permuted /* OUT */) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI file type")
        /* Sanity check */
        if(is_permuted)
            HDassert(permute_map);
        if(H5S_mpio_space_type(mem_space, type_info->src_type_size, 
                               &mpi_buf_type, &mpi_buf_count, &mbt_is_derived, /* OUT: datatype created */
                               FALSE, /* this is a memory space, so if
                                         the file space is not
                                         permuted, there is no need to
                                         permute the datatype if the
                                         point selections are out of
                                         order*/
                               &permute_map /* IN: the permutation map
                                               generated by the
                                               file_space selection
                                               and applied to the
                                               memory selection */, 
                               &is_permuted /* IN */) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI buffer type")
        /* Sanity check */
        if(is_permuted)
            HDassert(!permute_map);
    } /* end if */
    else {
        /* For non-selection, participate with a none MPI derived datatype, the count is 0.  */
        mpi_buf_type   = MPI_BYTE;
        mpi_file_type  = MPI_BYTE;
        mpi_buf_count  = 0;
        mbt_is_derived = FALSE;
        mft_is_derived = FALSE;
    } /* end else */

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before final collective IO \n");
#endif

    /* Perform final collective I/O operation */
    if(H5D__final_collective_io(io_info, type_info, (hsize_t)mpi_buf_count, &mpi_file_type, &mpi_buf_type) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish collective MPI-IO")

done:
    /* Free the MPI buf and file types, if they were derived */
    if(mbt_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mpi_buf_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if(mft_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mpi_file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before leaving inter_collective_io ret_value = %d\n",ret_value);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__inter_collective_io() */


/*-------------------------------------------------------------------------
 * Function:    H5D__final_collective_io
 *
 * Purpose:     Routine for the common part of collective IO with different storages.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__final_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t mpi_buf_count, MPI_Datatype *mpi_file_type, MPI_Datatype *mpi_buf_type)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Pass buf type, file type to the file driver.  */
    if(H5FD_mpi_setup_collective(io_info->raw_dxpl_id, mpi_buf_type, mpi_file_type) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O properties")

    if(io_info->op_type == H5D_IO_OP_WRITE) {
        if((io_info->io_ops.single_write)(io_info, type_info, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed")
    } /* end if */
    else {
        if((io_info->io_ops.single_read)(io_info, type_info, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed")
    } /* end else */

done:
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"ret_value before leaving final_collective_io=%d\n",ret_value);
#endif
      FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__final_collective_io */


/*-------------------------------------------------------------------------
 * Function:    H5D__cmp_chunk_addr
 *
 * Purpose:     Routine to compare chunk addresses
 *
 * Description: Callback for qsort() to compare chunk addresses
 *
 * Return:      -1, 0, 1
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__cmp_chunk_addr(const void *chunk_addr_info1, const void *chunk_addr_info2)
{
   haddr_t addr1, addr2;

   FUNC_ENTER_STATIC_NOERR

   addr1 = ((const H5D_chunk_addr_info_t *)chunk_addr_info1)->chunk_addr;
   addr2 = ((const H5D_chunk_addr_info_t *)chunk_addr_info2)->chunk_addr;

   FUNC_LEAVE_NOAPI(H5F_addr_cmp(addr1, addr2))
} /* end H5D__cmp_chunk_addr() */


/*-------------------------------------------------------------------------
 * Function:    H5D__cmp_filtered_collective_io_entry
 *
 * Purpose:     Routine to compare filtered collective chunk io info
 *              entries
 *
 * Description: Callback for qsort() to compare filtered collective chunk
 *              io info entries
 *
 * Return:      -1, 0, 1
 *
 * Programmer:  Jordan Henderson
 *              Wednesday, Nov. 30th, 2016
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__cmp_filtered_collective_io_entry(const void *filtered_collective_io_entry1, const void *filtered_collective_io_entry2)
{
    haddr_t addr1, addr2;

    FUNC_ENTER_STATIC_NOERR

    addr1 = ((const H5D_filtered_collective_io_info_t *) filtered_collective_io_entry1)->new_chunk.offset;
    addr2 = ((const H5D_filtered_collective_io_info_t *) filtered_collective_io_entry2)->new_chunk.offset;

    FUNC_LEAVE_NOAPI(H5F_addr_cmp(addr1, addr2))
} /* end H5D__cmp_filtered_collective_io_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5D__sort_chunk
 *
 * Purpose:     Routine to sort chunks in increasing order of chunk address
 *              Each chunk address is also obtained.
 *
 * Description:
 *              For most cases, the chunk address has already been sorted in increasing order.
 *              The special sorting flag is used to optimize this common case.
 *              quick sort is used for necessary sorting.
 *
 * Parameters:
 *              Input: H5D_io_info_t* io_info,
 *                      H5D_chunk_map_t *fm(global chunk map struct)
 *              Input/Output:  H5D_chunk_addr_info_t chunk_addr_info_array[]   : array to store chunk address and information
 *                     many_chunk_opt                         : flag to optimize the way to obtain chunk addresses
 *                                                              for many chunks
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__sort_chunk(H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
    H5D_chunk_addr_info_t chunk_addr_info_array[], int sum_chunk)
{
    H5SL_node_t    *chunk_node;         /* Current node in chunk skip list */
    H5D_chunk_info_t *chunk_info;         /* Current chunking info. of this node. */
    haddr_t         chunk_addr;         /* Current chunking address of this node */
    haddr_t        *total_chunk_addr_array = NULL; /* The array of chunk address for the total number of chunk */
    hbool_t         do_sort = FALSE;    /* Whether the addresses need to be sorted */
    int             bsearch_coll_chunk_threshold;
    int             many_chunk_opt = H5D_OBTAIN_ONE_CHUNK_ADDR_IND;
    int             mpi_size;                   /* Number of MPI processes */
    int             mpi_code;                   /* MPI return code */
    int             i;                          /* Local index variable */
    herr_t          ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_STATIC

    /* Retrieve # of MPI processes */
    if((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

    /* Calculate the actual threshold to obtain all chunk addresses collectively
     *  The bigger this number is, the more possible the use of obtaining chunk
     * address collectively.
     */
    /* For non-optimization one-link IO, actual bsearch threshold is always
     *   0, we would always want to obtain the chunk addresses individually
     *   for each process.
     */
    bsearch_coll_chunk_threshold = (sum_chunk * 100) / ((int)fm->layout->u.chunk.nchunks * mpi_size);
    if((bsearch_coll_chunk_threshold > H5D_ALL_CHUNK_ADDR_THRES_COL)
            && ((sum_chunk / mpi_size) >= H5D_ALL_CHUNK_ADDR_THRES_COL_NUM))
        many_chunk_opt = H5D_OBTAIN_ALL_CHUNK_ADDR_COL;

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D), "many_chunk_opt= %d\n", many_chunk_opt);
#endif

    /* If we need to optimize the way to obtain the chunk address */
    if(many_chunk_opt != H5D_OBTAIN_ONE_CHUNK_ADDR_IND) {
        int mpi_rank;

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D), "Coming inside H5D_OBTAIN_ALL_CHUNK_ADDR_COL\n");
#endif
        /* Allocate array for chunk addresses */
        if(NULL == (total_chunk_addr_array = H5MM_malloc(sizeof(haddr_t) * (size_t)fm->layout->u.chunk.nchunks)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory chunk address array")

        /* Retrieve all the chunk addresses with process 0 */
        if((mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file)) < 0)
            HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi rank")

        if(mpi_rank == 0) {
            if(H5D__chunk_addrmap(io_info, total_chunk_addr_array) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")
        } /* end if */

        /* Broadcasting the MPI_IO option info. and chunk address info. */
        if(MPI_SUCCESS != (mpi_code = MPI_Bcast(total_chunk_addr_array, (int)(sizeof(haddr_t) * fm->layout->u.chunk.nchunks), MPI_BYTE, (int)0, io_info->comm)))
           HMPI_GOTO_ERROR(FAIL, "MPI_BCast failed", mpi_code)
    } /* end if */

    /* Start at first node in chunk skip list */
    i = 0;
    if(NULL == (chunk_node = H5SL_first(fm->sel_chunks)))
        HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get chunk node from skipped list")

    /* Iterate over all chunks for this process */
    while(chunk_node) {
        if(NULL == (chunk_info = H5SL_item(chunk_node)))
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get chunk info from skipped list")

        if(many_chunk_opt == H5D_OBTAIN_ONE_CHUNK_ADDR_IND) {
            H5D_chunk_ud_t udata;   /* User data for querying chunk info */

            /* Get address of chunk */
            if(H5D__chunk_lookup(io_info->dset, io_info->md_dxpl_id, chunk_info->scaled, &udata) < 0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk info from skipped list")
            chunk_addr = udata.chunk_block.offset;
        } /* end if */
        else
            chunk_addr = total_chunk_addr_array[chunk_info->index];

        /* Check if chunk addresses are not in increasing order in the file */
        if(i > 0 && chunk_addr < chunk_addr_info_array[i - 1].chunk_addr)
            do_sort = TRUE;

        /* Set the address & info for this chunk */
        chunk_addr_info_array[i].chunk_addr = chunk_addr;
        chunk_addr_info_array[i].chunk_info = *chunk_info;

        /* Advance to next chunk in list */
        i++;
        chunk_node = H5SL_next(chunk_node);
    } /* end while */

#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D), "before Qsort\n");
#endif
    if(do_sort) {
        size_t num_chunks = H5SL_count(fm->sel_chunks);

        HDqsort(chunk_addr_info_array, num_chunks, sizeof(chunk_addr_info_array[0]), H5D__cmp_chunk_addr);
    } /* end if */

done:
    if(total_chunk_addr_array)
        H5MM_xfree(total_chunk_addr_array);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__sort_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5D__obtain_mpio_mode
 *
 * Purpose:     Routine to obtain each io mode(collective,independent or none) for each chunk;
 *              Each chunk address is also obtained.
 *
 * Description:
 *
 *              1) Each process provides two piece of information for all chunks having selection
 *                 a) chunk index
 *                 b) wheather this chunk is regular(for MPI derived datatype not working case)
 *
 *              2) Gather all the information to the root process
 *
 *              3) Root process will do the following:
 *                 a) Obtain chunk addresses for all chunks in this data space
 *                 b) With the consideration of the user option, calculate IO mode for each chunk
 *                 c) Build MPI derived datatype to combine "chunk address" and "assign_io" information
 *                      in order to do MPI Bcast only once
 *                 d) MPI Bcast the IO mode and chunk address information for each chunk.
 *              4) Each process then retrieves IO mode and chunk address information to assign_io_mode and chunk_addr.
 *
 * Parameters:
 *
 *              Input: H5D_io_info_t* io_info,
 *                      H5D_chunk_map_t *fm,(global chunk map struct)
 *              Output: uint8_t assign_io_mode[], : IO mode, collective, independent or none
 *                      haddr_t chunk_addr[],     : chunk address array for each chunk
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__obtain_mpio_mode(H5D_io_info_t* io_info, H5D_chunk_map_t *fm,
    H5P_genplist_t *dx_plist, uint8_t assign_io_mode[], haddr_t chunk_addr[])
{
    int               total_chunks;
    unsigned          percent_nproc_per_chunk, threshold_nproc_per_chunk;
    uint8_t*          io_mode_info = NULL;
    uint8_t*          recv_io_mode_info = NULL;
    uint8_t*          mergebuf = NULL;
    uint8_t*          tempbuf;
    H5SL_node_t*      chunk_node;
    H5D_chunk_info_t* chunk_info;
    int               mpi_size, mpi_rank;
    MPI_Comm          comm;
    int               ic, root;
    int               mpi_code;
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    int new_value;
    htri_t check_prop;
#endif
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Assign the rank 0 to the root */
    root              = 0;
    comm              = io_info->comm;

    /* Obtain the number of process and the current rank of the process */
    if((mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi rank")
    if((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

    /* Setup parameters */
    H5_CHECKED_ASSIGN(total_chunks, int, fm->layout->u.chunk.nchunks, hsize_t);
    if(H5P_get(dx_plist, H5D_XFER_MPIO_CHUNK_OPT_RATIO_NAME, &percent_nproc_per_chunk) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't get percent nproc per chunk")
    /* if ratio is 0, perform collective io */
    if(0 == percent_nproc_per_chunk) {
        if(H5D__chunk_addrmap(io_info, chunk_addr) < 0)
           HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address");
        for(ic = 0; ic < total_chunks; ic++)
           assign_io_mode[ic] = H5D_CHUNK_IO_MODE_COL;

        HGOTO_DONE(SUCCEED)
    } /* end if */
    threshold_nproc_per_chunk = mpi_size * percent_nproc_per_chunk/100;

    /* Allocate memory */
    if(NULL == (io_mode_info = (uint8_t *)H5MM_calloc(total_chunks)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate I/O mode info buffer")
    if(NULL == (mergebuf = H5MM_malloc((sizeof(haddr_t) + 1) * total_chunks)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate mergebuf buffer")
    tempbuf           = mergebuf + total_chunks;
    if(mpi_rank == root)
        if(NULL == (recv_io_mode_info = (uint8_t *)H5MM_malloc(total_chunks * mpi_size)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate recv I/O mode info buffer")

    /* Obtain the regularity and selection information for all chunks in this process. */
    chunk_node        = H5SL_first(fm->sel_chunks);
    while(chunk_node) {
        chunk_info    = H5SL_item(chunk_node);

            io_mode_info[chunk_info->index] = H5D_CHUNK_SELECT_REG; /* this chunk is selected and is "regular" */
        chunk_node = H5SL_next(chunk_node);
    } /* end while */

    /* Gather all the information */
    if(MPI_SUCCESS != (mpi_code = MPI_Gather(io_mode_info, total_chunks, MPI_BYTE, recv_io_mode_info, total_chunks, MPI_BYTE, root, comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Gather failed", mpi_code)

    /* Calculate the mode for IO(collective, independent or none) at root process */
    if(mpi_rank == root) {
        int               nproc;
        int*              nproc_per_chunk;

        /* pre-computing: calculate number of processes and
            regularity of the selection occupied in each chunk */
        if(NULL == (nproc_per_chunk = (int*)H5MM_calloc(total_chunks * sizeof(int))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate nproc_per_chunk buffer")

        /* calculating the chunk address */
        if(H5D__chunk_addrmap(io_info, chunk_addr) < 0) {
            H5MM_free(nproc_per_chunk);
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")
        } /* end if */

        /* checking for number of process per chunk and regularity of the selection*/
        for(nproc = 0; nproc < mpi_size; nproc++) {
            uint8_t *tmp_recv_io_mode_info = recv_io_mode_info + (nproc * total_chunks);

            /* Calculate the number of process per chunk and adding irregular selection option */
            for(ic = 0; ic < total_chunks; ic++, tmp_recv_io_mode_info++) {
                if(*tmp_recv_io_mode_info != 0) {
                    nproc_per_chunk[ic]++;
                } /* end if */
            } /* end for */
        } /* end for */

        /* Calculating MPIO mode for each chunk (collective, independent, none) */
        for(ic = 0; ic < total_chunks; ic++) {
            if(nproc_per_chunk[ic] > MAX(1, threshold_nproc_per_chunk)) {
                assign_io_mode[ic] = H5D_CHUNK_IO_MODE_COL;
            } /* end if */
        } /* end for */


        /* merge buffer io_mode info and chunk addr into one */
        HDmemcpy(mergebuf, assign_io_mode, total_chunks);
        HDmemcpy(tempbuf, chunk_addr, sizeof(haddr_t) * total_chunks);

        H5MM_free(nproc_per_chunk);
    } /* end if */

    /* Broadcasting the MPI_IO option info. and chunk address info. */
    if(MPI_SUCCESS != (mpi_code = MPI_Bcast(mergebuf, ((sizeof(haddr_t) + 1) * total_chunks), MPI_BYTE, root, comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_BCast failed", mpi_code)

    HDmemcpy(assign_io_mode, mergebuf, total_chunks);
    HDmemcpy(chunk_addr, tempbuf, sizeof(haddr_t) * total_chunks);

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
{
    H5P_genplist_t    *plist;           /* Property list pointer */

    /* Get the dataset transfer property list */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(io_info->raw_dxpl_id)))
        HGOTO_ERROR(H5E_IO, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    check_prop = H5P_exist_plist(plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_COLL_NAME);
    if(check_prop > 0) {
        for(ic = 0; ic < total_chunks; ic++) {
            if(assign_io_mode[ic] == H5D_CHUNK_IO_MODE_COL) {
                new_value = 0;
                if(H5P_set(plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_COLL_NAME, &new_value) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_UNSUPPORTED, FAIL, "unable to set property value")
                break;
            } /* end if */
        } /* end for */
    } /* end if */

    check_prop = H5P_exist_plist(plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_IND_NAME);
    if(check_prop > 0) {
        int temp_count = 0;

        for(ic = 0; ic < total_chunks; ic++) {
            if(assign_io_mode[ic] == H5D_CHUNK_IO_MODE_COL) {
                temp_count++;
                break;
            } /* end if */
        } /* end for */
        if(temp_count == 0) {
            new_value = 0;
            if(H5P_set(plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_IND_NAME, &new_value) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_UNSUPPORTED, FAIL, "unable to set property value")
        } /* end if */
    } /* end if */
}
#endif

done:
    if(io_mode_info)
        H5MM_free(io_mode_info);
    if(mergebuf)
        H5MM_free(mergebuf);
    if(recv_io_mode_info) {
        HDassert(mpi_rank == root);
        H5MM_free(recv_io_mode_info);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__obtain_mpio_mode() */


/*-------------------------------------------------------------------------
 * Function:    H5D__construct_filtered_io_info_list
 *
 * XXX: Revise description
 * Purpose:     Constructs a list of entries which contain the necessary
 *              information for inter-process communication when performing
 *              collective io on filtered chunks. This list is used by
 *              every process in operations that must be collectively done
 *              on every chunk, such as chunk re-allocation, insertion of
 *              chunks into the chunk index, etc.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jordan Henderson
 *              Tuesday, January 10th, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__construct_filtered_io_info_list(const H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    const H5D_chunk_map_t *fm, H5D_filtered_collective_io_info_t **chunk_list, size_t *num_entries,
    size_t **_num_chunks_selected_array)
{
    H5D_filtered_collective_io_info_t *local_info_array = NULL;
    H5D_filtered_collective_io_info_t *overlap_info_array = NULL;
    H5SL_node_t                       *chunk_node;
    MPI_Request                       *send_requests = NULL;
    MPI_Status                        *send_statuses = NULL;
    hbool_t                            no_overlap = FALSE;
    size_t                             num_send_requests;
    size_t                             num_chunks_selected;
    size_t                             overlap_info_array_num_entries;
    size_t                            *num_chunks_selected_array = NULL;
    int                                mpi_rank, mpi_size, mpi_code;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(io_info);
    HDassert(type_info);
    HDassert(fm);
    HDassert(chunk_list);

    if ((mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi rank")
    if ((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain mpi size")

    /* Get the no overlap property */


    if (NULL == (local_info_array = (H5D_filtered_collective_io_info_t *) H5MM_malloc(H5SL_count(fm->sel_chunks) * sizeof(*local_info_array))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate local io info array buffer")

    chunk_node = H5SL_first(fm->sel_chunks);
    for (num_chunks_selected = 0; chunk_node; num_chunks_selected++) {
        H5D_chunk_info_t *chunk_info = (H5D_chunk_info_t *) H5SL_item(chunk_node);
        H5D_chunk_ud_t    udata;

        /* Obtain this chunk's address */
        if (H5D__chunk_lookup(io_info->dset, io_info->md_dxpl_id, chunk_info->scaled, &udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "error looking up chunk address")

        local_info_array[num_chunks_selected].chunk_info = *chunk_info;
        local_info_array[num_chunks_selected].old_chunk = local_info_array[num_chunks_selected].new_chunk = udata.chunk_block;
        local_info_array[num_chunks_selected].io_size = H5S_GET_SELECT_NPOINTS(chunk_info->mspace) * type_info->src_type_size;
        local_info_array[num_chunks_selected].num_writers = 0;
        local_info_array[num_chunks_selected].owner = mpi_rank;
        local_info_array[num_chunks_selected].buf = NULL;

        chunk_node = H5SL_next(chunk_node);
    } /* end for */

#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, " Contents of local info array\n");
    HDfprintf(debug_file, "------------------------------\n");
    for (size_t j = 0; j < (size_t) num_chunks_selected; j++) {
        HDfprintf(debug_file, "| Chunk Entry %zd:\n", j);
        HDfprintf(debug_file, "|  - Chunk Address: %a\n", local_info_array[j].old_chunk.offset);
        HDfprintf(debug_file, "|  - Chunk Length: %zd\n", local_info_array[j].old_chunk.length);
        HDfprintf(debug_file, "|  - Address of mspace: %x\n", local_info_array[j].chunk_info.mspace);
        HDfprintf(debug_file, "|  - Chunk Selection Type: %d\n", H5S_GET_SELECT_TYPE(local_info_array[j].chunk_info.mspace));
        HDfprintf(debug_file, "|  - Chunk Num Elmts Sel.: %zd\n", H5S_GET_SELECT_NPOINTS(local_info_array[j].chunk_info.mspace));
    }
    HDfprintf(debug_file, "------------------------------\n\n");
#endif


#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, "Testing mem/file space addresses:\n");
    HDfprintf(debug_file, "-----------------------------------\n");

    for (size_t j = 0; j < num_chunks_selected; j++) {
        HDfprintf(debug_file, "| Testing chunk at address %a.\n", local_info_array[j].old_chunk.offset);
        HDfprintf(debug_file, "| Mem Space:\n");
        HDfprintf(debug_file, "|  - Extent Num elements: %zd\n", H5S_GET_EXTENT_NPOINTS(local_info_array[j].chunk_info.mspace));
        HDfprintf(debug_file, "|  - Extent type: %d\n", H5S_GET_EXTENT_TYPE(local_info_array[j].chunk_info.mspace));
        HDfprintf(debug_file, "|  - Selection Num elements: %zd\n", H5S_GET_SELECT_NPOINTS(local_info_array[j].chunk_info.mspace));
        HDfprintf(debug_file, "|  - Selection type: %d\n", H5S_GET_SELECT_TYPE(local_info_array[j].chunk_info.mspace));
        HDfprintf(debug_file, "| File Space:\n");
        HDfprintf(debug_file, "|  - Extent Num elements: %zd\n", H5S_GET_EXTENT_NPOINTS(local_info_array[j].chunk_info.fspace));
        HDfprintf(debug_file, "|  - Extent type: %d\n", H5S_GET_EXTENT_TYPE(local_info_array[j].chunk_info.fspace));
        HDfprintf(debug_file, "|  - Selection Num elements: %zd\n", H5S_GET_SELECT_NPOINTS(local_info_array[j].chunk_info.fspace));
        HDfprintf(debug_file, "|  - Selection type: %d\n|\n", H5S_GET_SELECT_TYPE(local_info_array[j].chunk_info.fspace));
    }

    HDfprintf(debug_file, "-----------------------------------\n\n");
#endif

    /* Redistribute chunks to new owners as necessary */
    if (!no_overlap && (io_info->op_type == H5D_IO_OP_WRITE)) {
        size_t i;

        if (NULL == (send_requests = (MPI_Request *) H5MM_malloc(num_chunks_selected * sizeof(*send_requests))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate send requests buffer")

        /* XXX: Change minor error code */
        if (H5D__mpio_array_gather(io_info, local_info_array, num_chunks_selected,
                sizeof(*local_info_array), (void **) &overlap_info_array, &overlap_info_array_num_entries,
                H5D__cmp_filtered_collective_io_entry) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "couldn't ")

        for (i = 0, num_chunks_selected = 0, num_send_requests = 0; i < overlap_info_array_num_entries;) {
            H5D_filtered_collective_io_info_t chunk_entry;
            haddr_t chunk_addr = overlap_info_array[i].old_chunk.offset;
            size_t  num_writers = 0;
            size_t  max_bytes = 0;
            int     new_owner = 0;

            /* Process duplicate entries caused by another process writing
             * to the same chunk */
            do {
                /* Store the correct chunk entry information in case this process
                 * becomes the new chunk's owner. The chunk entry that this process
                 * contributed will be the only one with a valid dataspace selection
                 * on this particular process */
                if (mpi_rank == overlap_info_array[i].owner)
                    chunk_entry = overlap_info_array[i];

                /* New owner of the chunk is determined by the process
                 * which is writing the most data to the chunk */
                if (overlap_info_array[i].io_size > max_bytes) {
                    max_bytes = overlap_info_array[i].io_size;
                    new_owner = overlap_info_array[i].owner;
                }

                num_writers++;
                i++;

                if (i == overlap_info_array_num_entries) break;
            } while (overlap_info_array[i].old_chunk.offset == chunk_addr);

            if (mpi_rank == new_owner) {
                /* Make sure the new owner will know how many other processes will
                 * be sending chunk modification data to it */
                chunk_entry.num_writers = num_writers;

                /* New owner takes possession of the chunk */
                overlap_info_array[num_chunks_selected++] = chunk_entry;
            } /* end if */
            else {
                /* Send modification data to new owner */

                /* if (MPI_SUCCESS != (mpi_code= MPI_Isend(, , , new_owner,
                        chunk_entry.old_chunk.offset, io_info->comm, &send_requests[num_send_requests++])))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Isend failed", mpi_code) */
            } /* end else */

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "Chunk at address %a re-assigned to process %d.\n", chunk_addr, new_owner);
#endif
        } /* end for */

        /* Release old list */
        if (local_info_array)
            H5MM_free(local_info_array);

        /* Local info list becomes modified (redistributed) chunk list */
        local_info_array = overlap_info_array;

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "This process now has %d chunks selected after redistribution.\n\n", num_chunks_selected);

        HDfprintf(debug_file, " Contents of local info array (after redistribution)\n");
        HDfprintf(debug_file, "------------------------------\n");
        for (size_t j = 0; j < (size_t) num_chunks_selected; j++) {
            HDfprintf(debug_file, "| Chunk Entry %zd:\n", j);
            HDfprintf(debug_file, "|  - Chunk Address: %a\n", local_info_array[j].old_chunk.offset);
            HDfprintf(debug_file, "|  - Chunk Length: %zd\n", local_info_array[j].old_chunk.length);
            HDfprintf(debug_file, "|  - Address of mspace: %x\n", local_info_array[j].chunk_info.mspace);
            HDfprintf(debug_file, "|  - Chunk Selection Type: %d\n", H5S_GET_SELECT_TYPE(local_info_array[j].chunk_info.mspace));
            HDfprintf(debug_file, "|  - Chunk Num Elmts Sel.: %zd\n", H5S_GET_SELECT_NPOINTS(local_info_array[j].chunk_info.mspace));
        }
        HDfprintf(debug_file, "------------------------------\n\n");
#endif
    } /* end if */

    /* Gather the number of chunks each process is writing to all processes */
    if (NULL == (num_chunks_selected_array = (size_t *) H5MM_malloc((size_t) mpi_size * sizeof(*num_chunks_selected_array))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate num chunks selected array")

    if (MPI_SUCCESS != (mpi_code = MPI_Allgather(&num_chunks_selected, 1, MPI_UNSIGNED_LONG_LONG, num_chunks_selected_array, 1, MPI_UNSIGNED_LONG_LONG, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code)

#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, " Num Chunks Selected Array\n");
    HDfprintf(debug_file, "------------------------------------\n");
    for (size_t j = 0; j < (size_t) mpi_size; j++) {
        HDfprintf(debug_file, "| Process %d has %zd chunks selected.\n", j, num_chunks_selected_array[j]);
    }
    HDfprintf(debug_file, "------------------------------------\n\n");
#endif

    *chunk_list = local_info_array;
    *num_entries = num_chunks_selected;
    *_num_chunks_selected_array = num_chunks_selected_array;

    /* Wait for all async send requests to complete before returning */
    if (!no_overlap && num_send_requests) {
        if (NULL == (send_statuses = (MPI_Status *) H5MM_malloc(num_send_requests * sizeof(*send_statuses))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate send statuses buffer")

        if (MPI_SUCCESS != (mpi_code = MPI_Waitall(num_send_requests, send_requests, send_statuses)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Waitall failed", mpi_code)
    }


done:
    if (send_requests)
        H5MM_free(send_requests);
    if (send_statuses)
        H5MM_free(send_statuses);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__construct_filtered_io_info_list() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_filtered_collective_write_type
 *
 * Purpose:     Constructs a MPI derived datatype for both the memory and
 *              the file for a collective write of filtered chunks. The
 *              datatype contains the offsets in the file and the locations
 *              of the filtered chunk data buffers
 *
 *              XXX: Same type may be reusable for filtered collective read
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jordan Henderson
 *              Tuesday, November 22, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_filtered_collective_write_type(H5D_filtered_collective_io_info_t *chunk_list,
    size_t num_entries, MPI_Datatype *new_mem_type, hbool_t *mem_type_derived,
    MPI_Datatype *new_file_type, hbool_t *file_type_derived)
{
    MPI_Aint *write_buf_array = NULL;   /* Relative displacements of filtered chunk data buffers */
    MPI_Aint *file_offset_array = NULL; /* Chunk offsets in the file */
    int      *length_array = NULL;      /* Filtered Chunk lengths */
    herr_t    ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list);
    HDassert(new_mem_type);
    HDassert(mem_type_derived);
    HDassert(new_file_type);
    HDassert(file_type_derived);

    if (num_entries > 0) {
        size_t i;
        int    mpi_code;
        void  *base_buf;

        H5_CHECK_OVERFLOW(num_entries, size_t, int);

        /* Allocate arrays */
        if (NULL == (length_array = (int *) H5MM_malloc((size_t) num_entries * sizeof(int))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for filtered collective write length array")
        if (NULL == (write_buf_array = (MPI_Aint *) H5MM_malloc((size_t) num_entries * sizeof(MPI_Aint))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for filtered collective write buf length array")
        if (NULL == (file_offset_array = (MPI_Aint *) H5MM_malloc((size_t) num_entries * sizeof(MPI_Aint))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for collective write offset array")

        /* Ensure the list is sorted in ascending order of offset in the file */
        HDqsort(chunk_list, num_entries, sizeof(*chunk_list), H5D__cmp_filtered_collective_io_entry);

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "MPI Write type entries:\n");
        HDfprintf(debug_file, "---------------------------------\n");
#endif

        base_buf = chunk_list[0].buf;
        for (i = 0; i < num_entries; i++) {
            /* XXX: Revise description */
            /* Set up array position */
            file_offset_array[i] = (MPI_Aint) chunk_list[i].new_chunk.offset;
            length_array[i] = (int) chunk_list[i].new_chunk.length;
            write_buf_array[i] = (MPI_Aint) chunk_list[i].buf - (MPI_Aint) base_buf;

#ifdef PARALLEL_COMPRESS_DEBUG
            HDfprintf(debug_file, "| Type Entry %zd:\n", i);
            HDfprintf(debug_file, "|  - Offset: %a; Length: %zd\n", file_offset_array[i], length_array[i]);
            HDfprintf(debug_file, "|  - Write buffer:\n|    [");
            for (size_t j = 0; j < (size_t) length_array[i]; j++) {
                HDfprintf(debug_file, "%c, ", ((char *) chunk_list[i].buf)[j]);
            }
            HDfprintf(debug_file, "]\n|\n");
#endif
        } /* end while */

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "---------------------------------\n\n");
#endif

        /* Create memory MPI type */
        if (MPI_SUCCESS != (mpi_code = MPI_Type_create_hindexed((int) num_entries, length_array, write_buf_array, MPI_BYTE, new_mem_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hindexed failed", mpi_code)
        *mem_type_derived = TRUE;
        if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_mem_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

        /* Create file MPI type */
        if (MPI_SUCCESS != (mpi_code = MPI_Type_create_hindexed((int) num_entries, length_array, file_offset_array, MPI_BYTE, new_file_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hindexed failed", mpi_code)
        *file_type_derived = TRUE;
        if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_file_type)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
    } /* end if */

done:
    length_array = (int *) H5MM_free(length_array);
    write_buf_array = (MPI_Aint *) H5MM_free(write_buf_array);
    file_offset_array = (MPI_Aint *) H5MM_free(file_offset_array);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_filtered_collective_write_type() */


static herr_t
H5D__update_filtered_collective_chunk_entry(H5D_filtered_collective_io_info_t *chunk_entry,
    const H5D_io_info_t *io_info, const H5D_type_info_t *type_info)
{
    H5S_sel_iter_t *mem_iter = NULL;
    unsigned        filter_mask;
    hssize_t        iter_nelmts;
    hbool_t         full_overwrite = TRUE;
    hbool_t         mem_iter_init = FALSE;
    size_t          buf_size;
    herr_t          ret_value = SUCCEED;
    
    FUNC_ENTER_STATIC

    HDassert(chunk_entry);

#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, "| Chunk at address %a:\n", chunk_entry->old_chunk.offset);
#endif

    /* XXX: Determine if a chunk is being fully overwritten by looking at the total selection
     * in the dataspace */

    /* If this is a full overwrite of this chunk, enough memory must be allocated for
     * the size of the unfiltered chunk. Otherwise, enough memory must be allocated
     * to read the filtered chunk from the file. */
    /* XXX: Return value of macro should be checked instead */
    buf_size = (full_overwrite) ? (hsize_t) H5S_GET_EXTENT_NPOINTS(chunk_entry->chunk_info.fspace) * type_info->src_type_size
            : chunk_entry->old_chunk.length;
    chunk_entry->new_chunk.length = buf_size;

#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, "| - Allocing %zd bytes for %s chunk overwrite buffer.\n", buf_size,
            full_overwrite ? "full" : "non-full");
#endif

    if (NULL == (chunk_entry->buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk modification buffer")

    if (!full_overwrite) {
        /* Read the chunk from the file */
        if (H5F_block_read(io_info->dset->oloc.file, H5FD_MEM_DRAW, chunk_entry->old_chunk.offset,
                buf_size, H5AC_rawdata_dxpl_id, chunk_entry->buf) < 0)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, H5_ITER_ERROR, "unable to read raw data chunk")

        /* Unfilter the chunk before modifying it */
        if (H5Z_pipeline(&io_info->dset->shared->dcpl_cache.pline, H5Z_FLAG_REVERSE, &filter_mask,
                io_info->dxpl_cache->err_detect, io_info->dxpl_cache->filter_cb,
                (size_t *) &chunk_entry->new_chunk.length, &buf_size, &chunk_entry->buf) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTFILTER, H5_ITER_ERROR, "couldn't unfilter chunk for modifying")

#ifdef PARALLEL_COMPRESS_DEBUG
        HDfprintf(debug_file, "| - After decompression: Nbytes=%zd; Buf_size=%zd.\n", chunk_entry->new_chunk.length, buf_size);

        HDfprintf(debug_file, "| - Read buf:\n| - [");
        for (size_t j = 0; j < chunk_entry->new_chunk.length / type_info->src_type_size; j++) {
            if (j > 0) HDfprintf(debug_file, ", ");
            HDfprintf(debug_file, "%lld", ((long *) chunk_entry->buf)[j]);
        }
        HDfprintf(debug_file, "]\n|\n");
#endif
    } /* end if */

    /* Owner of this chunk, receive modification data from other processes */

    /* Update the chunk data with the modifications from the current (owning) process */

    if (NULL == (mem_iter = (H5S_sel_iter_t *) H5MM_malloc(sizeof(*mem_iter))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate memory iterator")

    /* Initialize iterator for memory selection */
    if (H5S_select_iter_init(mem_iter, chunk_entry->chunk_info.mspace, type_info->dst_type_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information")
    mem_iter_init = TRUE;

    if ((iter_nelmts = H5S_GET_SELECT_NPOINTS(chunk_entry->chunk_info.mspace)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOUNT, FAIL, "dataspace is invalid")

    if (!H5D__gather_mem(io_info->u.wbuf, chunk_entry->chunk_info.mspace, mem_iter,
            (size_t) iter_nelmts, io_info->dxpl_cache, chunk_entry->buf))
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't gather from write buffer")

    /* Update the chunk data with any modifications from other processes */


#ifdef PARALLEL_COMPRESS_DEBUG
    HDfprintf(debug_file, "| - Write Buffer:\n");
    HDfprintf(debug_file, "| - [");
    for (size_t j = 0; j < chunk_entry->new_chunk.length / type_info->src_type_size; j++) {
        if (j > 0) HDfprintf(debug_file, ", ");
        HDfprintf(debug_file, "%lld", ((long *) chunk_entry->buf)[j]);
    }
    HDfprintf(debug_file, "]\n|\n");

    HDfprintf(debug_file, "| - Write buf is at address %a.\n|\n", chunk_entry->buf);

    HDfprintf(debug_file, "| - About to filter %zd bytes in buffer of size %zd.\n|\n", chunk_entry->new_chunk.length, buf_size);
#endif

    /* Filter the chunk */
    filter_mask = 0;
    if (H5Z_pipeline(&io_info->dset->shared->dcpl_cache.pline, 0, &filter_mask,
            io_info->dxpl_cache->err_detect, io_info->dxpl_cache->filter_cb,
            (size_t *) &chunk_entry->new_chunk.length, &buf_size, &chunk_entry->buf) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTFILTER, H5_ITER_ERROR, "output pipeline failed")

#if H5_SIZEOF_SIZE_T > 4
    /* Check for the chunk expanding too much to encode in a 32-bit value */
    if (chunk_entry->new_chunk.length > ((size_t) 0xffffffff))
        HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "chunk too large for 32-bit length")
#endif

done:
    if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "couldn't release selection iterator")
    if (mem_iter)
        H5MM_free(mem_iter)

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__update_filtered_collective_chunk_entry() */
#endif  /* H5_HAVE_PARALLEL */

