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

/***** Macros for One linked collective IO case. *****/
/* The default value to do one linked collective IO for all chunks.
 * If the average number of chunks per process is greater than this
 * value, the library will create an MPI derived datatype to link all
 * chunks to do collective IO.  The user can set this value through an
 * API. 
 */

/* Macros to represent options on how to obtain chunk address for one linked-chunk IO case */
#define H5D_OBTAIN_ONE_CHUNK_ADDR_IND 0
#define H5D_OBTAIN_ALL_CHUNK_ADDR_COL 2

/* Macros to define the default ratio of obtaining all chunk addresses for one linked-chunk IO case */
#define H5D_ALL_CHUNK_ADDR_THRES_COL  30
#define H5D_ALL_CHUNK_ADDR_THRES_COL_NUM 10000

/***** Macros for multi-chunk collective IO case. *****/
/* The default value of the threshold to do collective IO for this
 *  chunk.  If the average number of processes per chunk is greater
 *  than the default value, collective IO is done for this chunk.
 */


/* Macros to represent the regularity of the selection for multiple chunk IO case. */
#define H5D_CHUNK_SELECT_REG          1
#define H5D_CHUNK_SELECT_IRREG        2
#define H5D_CHUNK_SELECT_NONE         0


/******************/
/* Local Typedefs */
/******************/
/* Combine chunk/piece address and chunk/piece info into a struct for 
 * better performance. */
typedef struct H5D_chunk_addr_info_t {
  /* piece for multi-dset */
  haddr_t piece_addr;
  H5D_piece_info_t piece_info;
} H5D_chunk_addr_info_t;


/********************/
/* Local Prototypes */
/********************/
/* multi-dset IO */
static herr_t H5D__piece_io(const hid_t file_id, const size_t count, 
    H5D_io_info_t *io_info);
static herr_t H5D__final_collective_io(H5D_io_info_t *io_info,
    hsize_t mpi_buf_count, MPI_Datatype *mpi_file_type, MPI_Datatype *mpi_buf_type);

static herr_t H5D__all_piece_collective_io(const hid_t file_id, const size_t count, 
    H5D_io_info_t *io_info, H5P_genplist_t *dx_plist);


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
 *              the file.
 *
 *              This was derived from H5D__mpio_opt_possible for 
 *              multi-dset work.
 *
 * Return:      Sauccess:   Non-negative: TRUE or FALSE
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5D__mpio_opt_possible(const size_t count, H5D_io_info_t *io_info, H5P_genplist_t *dx_plist)
{
    int i;
    H5D_t *dset;
    H5S_t *file_space;
    const H5S_t *mem_space;
    H5D_type_info_t type_info;
    /* variables to set cause of broken collective I/O */
    int local_cause = 0;
    int global_cause = 0;

    int mpi_code;               /* MPI error code */
    htri_t ret_value = TRUE;

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(io_info);
    HDassert(dx_plist);

    for (i = 0; i < count; i++) {
        HDassert(io_info->dsets_info[i].file_space);
        HDassert(io_info->dsets_info[i].mem_space);
    }

    /* For independent I/O, get out quickly and don't try to form consensus */
    if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_INDEPENDENT)
        local_cause |= H5D_MPIO_SET_INDEPENDENT;

    for (i = 0; i < count; i++)
    {
        dset = io_info->dsets_info[i].dset;
        file_space = io_info->dsets_info[i].file_space;
        mem_space = io_info->dsets_info[i].mem_space;
        type_info = io_info->dsets_info[i].type_info;

        /* Optimized MPI types flag must be set */
        /* (based on 'HDF5_MPI_OPT_TYPES' environment variable) */
        if(!H5FD_mpi_opt_types_g)
            local_cause |= H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED;

        /* Don't allow collective operations if datatype conversions need to happen */
        if(!type_info.is_conv_noop)
            local_cause |= H5D_MPIO_DATATYPE_CONVERSION;

        /* Don't allow collective operations if data transform operations should occur */
        if(!type_info.is_xform_noop)
            local_cause |= H5D_MPIO_DATA_TRANSFORMS;

        /* Check whether these are both simple or scalar dataspaces */
        if(!((H5S_SIMPLE == H5S_GET_EXTENT_TYPE(mem_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(mem_space))
             && (H5S_SIMPLE == H5S_GET_EXTENT_TYPE(file_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(file_space))))
            local_cause |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;

        /* Dataset storage must be contiguous or chunked */
        if(!(dset->shared->layout.type == H5D_CONTIGUOUS ||
                dset->shared->layout.type == H5D_CHUNKED))
            local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;

        /* check if external-file storage is used */
        if (dset->shared->dcpl_cache.efl.nused > 0)
            local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;

        /* The handling of memory space is different for chunking and contiguous
         *  storage.  For contiguous storage, mem_space and file_space won't change
         *  when it it is doing disk IO.  For chunking storage, mem_space will
         *  change for different chunks. So for chunking storage, whether we can
         *  use collective IO will defer until each chunk IO is reached.
         */

        /* Don't allow collective operations if filters need to be applied */
        if(dset->shared->layout.type == H5D_CHUNKED &&
                dset->shared->dcpl_cache.pline.nused > 0)
                local_cause |= H5D_MPIO_FILTERS;
    } /* end for loop */

    /* Check for independent I/O */
    if(local_cause & H5D_MPIO_SET_INDEPENDENT)
        global_cause = local_cause;
    else {
        /* Form consensus opinion among all processes about whether to perform
         * collective I/O
         */
        if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&local_cause, &global_cause, 1, MPI_INT, 
                                                    MPI_BOR, io_info->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)
    } /* end else */

    ret_value = global_cause > 0 ? FALSE : TRUE;

    /* Write the local value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_LOCAL_NO_COLLECTIVE_CAUSE_NAME, &local_cause) < 0)
       HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set local no collective cause property")

    /* Write the global value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_GLOBAL_NO_COLLECTIVE_CAUSE_NAME, &global_cause) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set global no collective cause property")

    ret_value = global_cause > 0 ? FALSE : TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__mpio_opt_possible() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_read
 *
 * Purpose:     MPI-IO function to read directly from app buffer to file.
 *
 *              This was referred from H5D__mpio_select_read for 
 *              multi-dset work.
 *
 * Return:      non-negative on success, negative on failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_read(const H5D_io_info_t *io_info, hsize_t mpi_buf_count, 
    const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space)
{
    /* all dsets are in the same file, so just get it from the first dset */
    const H5F_t *file = io_info->dsets_info[0].dset->oloc.file;
    void *rbuf = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* memory addr from a piece with lowest file addr */
    rbuf = io_info->base_maddr_r;

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if(H5F_block_read(file, H5FD_MEM_DRAW, io_info->store_faddr, (size_t)mpi_buf_count, 
                      io_info->dxpl_id, rbuf) < 0)
       HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't finish collective parallel write")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_read() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_write
 *
 * Purpose:     MPI-IO function to write directly from app buffer to file.
 *
 *              This was referred from H5D__mpio_select_write for 
 *              multi-dset work.
 *
 * Return:      non-negative on success, negative on failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_write(const H5D_io_info_t *io_info, hsize_t mpi_buf_count, 
    const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space)
{
    /* all dsets are in the same file, so just get it from the first dset */
    const H5F_t *file = io_info->dsets_info[0].dset->oloc.file;
    const void *wbuf = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* memory addr from a piece with lowest file addr */
    wbuf = io_info->base_maddr_w;

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if(H5F_block_write(file, H5FD_MEM_DRAW, io_info->store_faddr, (size_t)mpi_buf_count, 
                       io_info->dxpl_id, wbuf) < 0)
       HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't finish collective parallel write")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__piece_io
 *
 * Purpose:     Routine for choosing an IO option:
 *              a) Single collective IO defined by one MPI derived datatype 
 *                 to link through all pieces (chunks/contigs). Default.
 *              Note: previously there were other options, but cutoff as part of multi-dset work.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__piece_io(const hid_t file_id, const size_t count, H5D_io_info_t *io_info)
{
    H5P_genplist_t *dx_plist;           /* Pointer to DXPL */
    H5FD_mpio_chunk_opt_t chunk_opt_mode;
    int         io_option = H5D_ONE_LINK_CHUNK_IO;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(io_info);
    HDassert(io_info->using_mpi_vfd);

    /* Obtain the data transfer properties */
    if(NULL == (dx_plist = H5I_object(io_info->dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* Check the optional property list on what to do with collective chunk IO. */
    if(H5P_get(dx_plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME, &chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't get chunk optimization option")
    if(H5FD_MPIO_CHUNK_ONE_IO == chunk_opt_mode)
        io_option = H5D_ONE_LINK_CHUNK_IO;      /*no opt*/

/* MSC - From merge.. remove probably */
#if 0
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
#endif

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    {
        htri_t            check_prop;
        int               new_value;

        /*** Test collective chunk user-input optimization APIs. ***/
        check_prop = H5Pexist(io_info->dxpl_id, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME);
        if(check_prop > 0) {
            if(H5D_ONE_LINK_CHUNK_IO == io_option) {
                new_value = 0;
                if(H5Pset(io_info->dxpl_id, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME, &new_value) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTSET, FAIL, "unable to set property value")
            } /* end if */
        } /* end if */
    } /* end block */
#endif

    /* step 2:  Go ahead to do IO.*/
    if(H5D_ONE_LINK_CHUNK_IO == io_option)  {
        if(H5D__all_piece_collective_io(file_id, count, io_info, dx_plist) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish linked chunk MPI-IO")
    } /* end if */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__piece_io */


/*-------------------------------------------------------------------------
 * Function:    H5D__collective_read
 *
 * Purpose:     Read directly from pieces (chunks/contig) in file into 
 *              application memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__collective_read(const hid_t file_id, const size_t count, H5D_io_info_t *io_info)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if(H5D__piece_io(file_id, count, io_info) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__collective_read() */


/*-------------------------------------------------------------------------
 * Function:    H5D__collective_write
 *
 * Purpose:     Write directly to pieces (chunks/contig) in file into 
 *              application memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__collective_write(const hid_t file_id, const size_t count, H5D_io_info_t *io_info)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if(H5D__piece_io(file_id, count, io_info) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__collective_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__all_piece_collective_io
 *
 * Purpose:     Routine for single collective IO with one MPI derived datatype 
 *              to link with all pieces (chunks + contig)
 *
 *              1. Use the piece addresses and piece info sorted in skiplist
 *              2. Build up MPI derived datatype for each chunk
 *              3. Build up the final MPI derived datatype
 *              4. Use common collective IO routine to do MPI-IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__all_piece_collective_io(H5_ATTR_UNUSED const hid_t file_id, const size_t count, 
    H5D_io_info_t *io_info, H5P_genplist_t *dx_plist)
{
    MPI_Datatype chunk_final_mtype;         /* Final memory MPI datatype for all chunks with seletion */
    hbool_t chunk_final_mtype_is_derived = FALSE;
    MPI_Datatype chunk_final_ftype;         /* Final file MPI datatype for all chunks with seletion */
    hbool_t chunk_final_ftype_is_derived = FALSE;
    H5D_storage_t ctg_store;                /* Storage info for "fake" contiguous dataset */
    size_t              i;
    MPI_Datatype       *chunk_mtype = NULL;
    MPI_Datatype       *chunk_ftype = NULL;
    MPI_Aint           *chunk_file_disp_array = NULL;
    MPI_Aint           *chunk_mem_disp_array = NULL;
    hbool_t            *chunk_mft_is_derived_array = NULL;      /* Flags to indicate each chunk's MPI file datatype is derived */
    hbool_t            *chunk_mbt_is_derived_array = NULL;      /* Flags to indicate each chunk's MPI memory datatype is derived */
    int                *chunk_mpi_file_counts = NULL;   /* Count of MPI file datatype for each chunk */
    int                *chunk_mpi_mem_counts = NULL;    /* Count of MPI memory datatype for each chunk */
    int                 mpi_code;           /* MPI return code */
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode = H5D_MPIO_LINK_CHUNK;
    H5D_mpio_actual_io_mode_t actual_io_mode = 0;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* set actual_io_mode */ 
    for (i=0; i < count; i++) {
        if (io_info->dsets_info[i].layout->type == H5D_CHUNKED)
            actual_io_mode |= H5D_MPIO_CHUNK_COLLECTIVE;
        else if (io_info->dsets_info[i].layout->type == H5D_CONTIGUOUS) {
            actual_io_mode |= H5D_MPIO_CONTIGUOUS_COLLECTIVE;

            /* if only single-dset */
            if (1 == count)
                actual_chunk_opt_mode = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
        }
        else
            HGOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "unsupported storage layout")
    }

    /* Set the actual-chunk-opt-mode property. */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")

    /* Set the actual-io-mode property.
     * Link chunk I/O does not break to independent, so can set right away */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Code block for actual actions (Build a MPI Type, IO) */
    {
        hsize_t mpi_buf_count;  /* Number of MPI types */
        size_t num_chunk;       /* Number of chunks for this process */
        size_t u=0;               /* Local index variable */

        H5SL_node_t    *piece_node;         /* Current node in chunk skip list */
        H5D_piece_info_t *piece_info;

        /* local variable for base address for write buffer */
        const void * base_wbuf_addr = NULL;
        void * base_rbuf_addr = NULL;

        /* Get the number of chunks with a selection */
        num_chunk = H5SL_count(io_info->sel_pieces);
        H5_CHECK_OVERFLOW(num_chunk, size_t, int);

        /* Set up MPI datatype for chunks selected */
        if(num_chunk) {
            /* Allocate chunking information */
            if(NULL == (chunk_mtype = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory datatype buffer")
            if(NULL == (chunk_ftype = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file datatype buffer")
            if(NULL == (chunk_file_disp_array = (MPI_Aint *)H5MM_malloc(num_chunk * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file displacement buffer")
            if(NULL == (chunk_mem_disp_array = (MPI_Aint *)H5MM_calloc(num_chunk * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory displacement buffer")
            if(NULL == (chunk_mpi_mem_counts = (int *)H5MM_calloc(num_chunk * sizeof(int))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory counts buffer")
            if(NULL == (chunk_mpi_file_counts = (int *)H5MM_calloc(num_chunk * sizeof(int))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file counts buffer")
            if(NULL == (chunk_mbt_is_derived_array = (hbool_t *)H5MM_calloc(num_chunk * sizeof(hbool_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory is derived datatype flags buffer")
            if(NULL == (chunk_mft_is_derived_array = (hbool_t *)H5MM_calloc(num_chunk * sizeof(hbool_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file is derived datatype flags buffer")

            /* get first piece, which is sorted in skiplist */
            if(NULL == (piece_node = H5SL_first(io_info->sel_pieces)))
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get piece node from skipped list")
            if(NULL == (piece_info = (H5D_piece_info_t *)H5SL_item(piece_node)))
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get piece info from skipped list")
            /* save lowest file address */
            ctg_store.contig.dset_addr = piece_info->faddr;

            /* save base mem addr of piece for read/write */
            base_wbuf_addr = piece_info->dset_info->u.wbuf;
            base_rbuf_addr = piece_info->dset_info->u.rbuf;

#ifdef H5D_DEBUG
            if(H5DEBUG(D))
                HDfprintf(H5DEBUG(D),"before iterate over selected pieces\n");
#endif

            /* Obtain MPI derived datatype from all individual pieces */
            /* Iterate over selected pieces for this process */
            while(piece_node) {
                hsize_t *permute_map = NULL; /* array that holds the mapping from the old, 
                                                out-of-order displacements to the in-order 
                                                displacements of the MPI datatypes of the 
                                                point selection of the file space */
                hbool_t is_permuted = FALSE;

                if(NULL == (piece_info = (H5D_piece_info_t *)H5SL_item(piece_node)))
                    HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get piece info from skipped list")

                /* Obtain disk and memory MPI derived datatype */
                /* NOTE: The permute_map array can be allocated within H5S_mpio_space_type
                 *              and will be fed into the next call to H5S_mpio_space_type
                 *              where it will be freed.
                 */
                if(H5S_mpio_space_type(piece_info->fspace,
                                       piece_info->dset_info->type_info.src_type_size, 
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
                if(H5S_mpio_space_type(piece_info->mspace,
                                       piece_info->dset_info->type_info.dst_type_size,
                                       &chunk_mtype[u], 
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

                /* Piece address relative to the first piece addr 
                 * Assign piece address to MPI displacement 
                 * (assume MPI_Aint big enough to hold it) */
                chunk_file_disp_array[u] = (MPI_Aint)piece_info->faddr - (MPI_Aint)ctg_store.contig.dset_addr;

                if(io_info->op_type == H5D_IO_OP_WRITE) {
                    chunk_mem_disp_array[u] = (MPI_Aint)piece_info->dset_info->u.wbuf - (MPI_Aint)base_wbuf_addr;
                } 
                else if (io_info->op_type == H5D_IO_OP_READ) {
                    chunk_mem_disp_array[u] = (MPI_Aint)piece_info->dset_info->u.rbuf - (MPI_Aint)base_rbuf_addr;
                }

                /* Advance to next piece in list */
                u++;
                piece_node = H5SL_next(piece_node);
            } /* end while */

            if(MPI_SUCCESS != (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_file_counts, 
                                                                 chunk_file_disp_array, chunk_ftype, 
                                                                 &chunk_final_ftype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)

            if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&chunk_final_ftype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            chunk_final_ftype_is_derived = TRUE;

            /* Create final MPI derived datatype for memory */
            if(MPI_SUCCESS != (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_mem_counts, 
                                                                 chunk_mem_disp_array, chunk_mtype, 
                                                                 &chunk_final_mtype)))
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

            /* since this process doesn't do any io, just pass a valid addr.
             * at this point dset object hear address is availbe to any 
             * process, so just pass it. 0x0 also work fine */
            ctg_store.contig.dset_addr = 0x0;
            /* or ctg_store.contig.dset_addr = io_info->dsets_info[0].dset->oloc.addr; */
        
            /* just provide a valid mem address. no actual IO occur */
            base_wbuf_addr = io_info->dsets_info[0].u.wbuf;
            base_rbuf_addr = io_info->dsets_info[0].u.rbuf;

            /* Set the MPI datatype to just participate */
            chunk_final_ftype = MPI_BYTE;
            chunk_final_mtype = MPI_BYTE;

            mpi_buf_count  = (hsize_t)0;
        } /* end else */
#ifdef H5D_DEBUG
        if(H5DEBUG(D))
            HDfprintf(H5DEBUG(D),"before coming to final collective IO\n");
#endif
        /* Set up the base storage address for this piece */
        io_info->store_faddr = ctg_store.contig.dset_addr;
        io_info->base_maddr_w = base_wbuf_addr;
        io_info->base_maddr_r = base_rbuf_addr;

        /* Perform final collective I/O operation */
        if(H5D__final_collective_io(io_info, mpi_buf_count, &chunk_final_ftype, &chunk_final_mtype) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")
    }

done:
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"before freeing memory inside H5D_link_collective_io ret_value = %d\n", ret_value);
#endif
    /* Release resources */
    if(chunk_mtype)
        H5MM_xfree(chunk_mtype);
    if(chunk_ftype)
        H5MM_xfree(chunk_ftype);
    if(chunk_file_disp_array)
        H5MM_xfree(chunk_file_disp_array);
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
} /* end H5D__all_piece_collective_io */


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
H5D__final_collective_io(H5D_io_info_t *io_info,
    hsize_t mpi_buf_count, MPI_Datatype *mpi_file_type, MPI_Datatype *mpi_buf_type)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Pass buf type, file type to the file driver.  */
    if(H5FD_mpi_setup_collective(io_info->dxpl_id, mpi_buf_type, mpi_file_type) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O properties")

    if(io_info->op_type == H5D_IO_OP_WRITE) {
        if((io_info->io_ops.single_write_md)(io_info, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed")
    } /* end if */
    else {
        if((io_info->io_ops.single_read_md)(io_info, mpi_buf_count, NULL, NULL) < 0)
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

   addr1 = ((const H5D_chunk_addr_info_t *)chunk_addr_info1)->piece_addr;
   addr2 = ((const H5D_chunk_addr_info_t *)chunk_addr_info2)->piece_addr;

   FUNC_LEAVE_NOAPI(H5F_addr_cmp(addr1, addr2))
} /* end H5D__cmp_chunk_addr() */


/*-------------------------------------------------------------------------
 * Function:    H5D_match_coll_calls
 *
 * Purpose: For processes that are not reading/writing to a particular
 * datasets through the multi-dataset interface, but are participating
 * in the collective call, match the collective I/O calls from the
 * other processes.
 *
 * Return:      non-negative on success, negative on failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5D__match_coll_calls(hid_t file_id, H5P_genplist_t *plist, hbool_t do_read)
{
    int local_cause = 0;
    int global_cause = 0;
    int mpi_code;
    H5F_t *file;
    H5FD_mpio_collective_opt_t para_io_mode;
    H5FD_mpio_xfer_t xfer_mode;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    HDassert(file_id > 0);

    /* Get the transfer mode */
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
    HDassert(xfer_mode == H5FD_MPIO_COLLECTIVE);

    /* get parallel io mode */
    if(H5P_get(plist, H5D_XFER_MPIO_COLLECTIVE_OPT_NAME, &para_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* just to match up with MPI_Allreduce from H5D__mpio_opt_possible() */
    if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&local_cause, &global_cause, 1, 
                                                MPI_INT, MPI_BOR, H5F_mpi_get_comm(file))))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

    /* if collective mode is not broken according to the
     * H5D__mpio_opt_possible, since the below MPI funcs will be 
     * called only in collective mode */
    if(!global_cause) {
        MPI_Status mpi_stat;
        MPI_File mpi_fh_p;
        MPI_File mpi_fh;

        if(H5F_get_mpi_handle(file, (MPI_File **)&mpi_fh_p) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get mpi file handle")
        mpi_fh = *(MPI_File*)mpi_fh_p;

        /* just to match up with the 1st MPI_File_set_view from H5FD_mpio_read() */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(mpi_fh, (MPI_Offset)0, MPI_BYTE, 
                                                        MPI_BYTE, "native", MPI_INFO_NULL)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)

        /* just to match up with MPI_File_write_at_all from H5FD_mpio_read() */
        if(para_io_mode == H5FD_MPIO_COLLECTIVE_IO)  {
            HDmemset(&mpi_stat, 0, sizeof(MPI_Status));
            if(do_read) {
                if(MPI_SUCCESS != (mpi_code = MPI_File_read_at_all(mpi_fh, 0, NULL, 0, MPI_BYTE, &mpi_stat)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at_all failed", mpi_code)
            }
            else {
                if(MPI_SUCCESS != (mpi_code = MPI_File_write_at_all(mpi_fh, 0, NULL, 0, MPI_BYTE, &mpi_stat)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at_all failed", mpi_code)
            }
        } /* end if */

        /* just to match up with the 2nd MPI_File_set_view (reset) in H5FD_mpio_read() */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(mpi_fh, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, 
                                                        "native", MPI_INFO_NULL)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)
    } /* end if !global_cause */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_match_coll_calls */

#endif /* H5_HAVE_PARALLEL */

/* MSC - From merge.. remove probably */
#if 0
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
    io_mode_info      = (uint8_t *)H5MM_calloc(total_chunks);
    mergebuf          = H5MM_malloc((sizeof(haddr_t) + 1) * total_chunks);
    tempbuf           = mergebuf + total_chunks;
    if(mpi_rank == root)
        recv_io_mode_info = (uint8_t *)H5MM_malloc(total_chunks * mpi_size);
    mem_cleanup       = TRUE;

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
        nproc_per_chunk = (int*)H5MM_calloc(total_chunks * sizeof(int));

        /* calculating the chunk address */
        if(H5D__chunk_addrmap(io_info, chunk_addr) < 0) {
            HDfree(nproc_per_chunk);
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

        HDfree(nproc_per_chunk);
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
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(io_info->dxpl_id)))
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
    if(mem_cleanup) {
        HDfree(io_mode_info);
        HDfree(mergebuf);
        if(mpi_rank == root)
            HDfree(recv_io_mode_info);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__obtain_mpio_mode() */
#endif
