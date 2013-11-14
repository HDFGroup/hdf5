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

#define H5D_PACKAGE /* suppress error about including H5Dpkg */


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
#include "H5Vprivate.h"       /* Vector            */

#ifdef H5_HAVE_PARALLEL

/****************/
/* Local Macros */
/****************/

/* Macros to represent different IO options */
#define H5D_ONE_LINK_CHUNK_IO          0

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
  /* chunk for single-dset */
  haddr_t chunk_addr;
  H5D_chunk_info_t chunk_info;
  /* piece for multi-dset */
  haddr_t piece_addr;
  H5D_piece_info_t piece_info;
} H5D_chunk_addr_info_t;


/********************/
/* Local Prototypes */
/********************/
/* multi-dset IO */
static herr_t H5D__piece_mdset_io(const hid_t file_id, const size_t count, 
    H5D_io_info_md_t *io_info_md);
static herr_t H5D__final_collective_io_mdset(H5D_io_info_md_t *io_info_md,
    hsize_t mpi_buf_count, MPI_Datatype *mpi_file_type, MPI_Datatype *mpi_buf_type);

static herr_t H5D__all_piece_collective_io(const hid_t file_id, const size_t count, 
    H5D_io_info_md_t *io_info_md, H5P_genplist_t *dx_plist);
static herr_t H5D__mpio_get_min_chunk(const H5D_io_info_t *io_info,
    const H5D_chunk_map_t *fm, int *min_chunkf);


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
    UNUSED const H5D_chunk_map_t *fm, H5P_genplist_t *dx_plist)
{
    /* variables to set cause of broken collective I/O */
    int local_cause = 0;
    int global_cause = 0;

    int mpi_code;               /* MPI error code */
    htri_t ret_value = TRUE;

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(io_info);
    HDassert(mem_space);
    HDassert(file_space);
    HDassert(type_info);


    /* For independent I/O, get out quickly and don't try to form consensus */
    if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_INDEPENDENT) {
        local_cause = H5D_MPIO_SET_INDEPENDENT;
        global_cause = H5D_MPIO_SET_INDEPENDENT;
        HGOTO_DONE(FALSE);
    }

    /* Optimized MPI types flag must be set and it must be collective IO */
    /* (Don't allow parallel I/O for the MPI-posix driver, since it doesn't do real collective I/O) */
    if(!(H5S_mpi_opt_types_g && io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE
            && !IS_H5FD_MPIPOSIX(io_info->dset->oloc.file))) {
        local_cause |= H5D_MPIO_SET_MPIPOSIX;
    } /* end if */

    /* Don't allow collective operations if datatype conversions need to happen */
    if(!type_info->is_conv_noop) {
        local_cause |= H5D_MPIO_DATATYPE_CONVERSION;
    } /* end if */

    /* Don't allow collective operations if data transform operations should occur */
    if(!type_info->is_xform_noop) {
        local_cause |= H5D_MPIO_DATA_TRANSFORMS;
    } /* end if */

    /* Check whether these are both simple or scalar dataspaces */
    if(!((H5S_SIMPLE == H5S_GET_EXTENT_TYPE(mem_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(mem_space))
            && (H5S_SIMPLE == H5S_GET_EXTENT_TYPE(file_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(file_space)))) {
        local_cause |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;
    } /* end if */

    /* Can't currently handle point selections */
    if(H5S_SEL_POINTS == H5S_GET_SELECT_TYPE(mem_space)
            || H5S_SEL_POINTS == H5S_GET_SELECT_TYPE(file_space)) {
        local_cause |= H5D_MPIO_POINT_SELECTIONS;
    } /* end if */

    /* Dataset storage must be contiguous or chunked */
    if(!(io_info->dset->shared->layout.type == H5D_CONTIGUOUS ||
            io_info->dset->shared->layout.type == H5D_CHUNKED)) {
        local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
    } /* end if */

    /* check if external-file storage is used */
    if (io_info->dset->shared->dcpl_cache.efl.nused > 0) {
        local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
    }

    /* The handling of memory space is different for chunking and contiguous
     *  storage.  For contiguous storage, mem_space and file_space won't change
     *  when it it is doing disk IO.  For chunking storage, mem_space will
     *  change for different chunks. So for chunking storage, whether we can
     *  use collective IO will defer until each chunk IO is reached.
     */

    /* Don't allow collective operations if filters need to be applied */
    if(io_info->dset->shared->layout.type == H5D_CHUNKED) {
        if(io_info->dset->shared->dcpl_cache.pline.nused > 0) {
            local_cause |= H5D_MPIO_FILTERS;
        } /* end if */
    } /* end if */

    /* Form consensus opinion among all processes about whether to perform
     * collective I/O
     */
    if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&local_cause, &global_cause, 1, MPI_INT, MPI_BOR, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

    ret_value = global_cause > 0 ? FALSE : TRUE;


done:
    /* Write the local value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_LOCAL_NO_COLLECTIVE_CAUSE_NAME, &local_cause) < 0)
       HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set local no collective cause property")

    /* Write the global value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_GLOBAL_NO_COLLECTIVE_CAUSE_NAME, &global_cause) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set global no collective cause property")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__mpio_opt_possible() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_opt_possible_mdset
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
 * Programmer:  JOnathan Kim
 *-------------------------------------------------------------------------
 */
htri_t
H5D__mpio_opt_possible_mdset(const size_t count, H5D_io_info_md_t *io_info_md, H5P_genplist_t *dx_plist)
   //(const H5D_io_info_md_t *io_info_md, 
   // const H5S_t *file_space, const H5S_t *mem_space, 
   // const H5D_type_info_t *type_info,
   // const H5D_t *dset, H5P_genplist_t *dx_plist)
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
    HDassert(io_info_md);
    HDassert(dx_plist);

    for (i = 0; i < count; i++) {
        HDassert(io_info_md->dsets_info[i].file_space);
        HDassert(io_info_md->dsets_info[i].mem_space);
    }


    /* For independent I/O, get out quickly and don't try to form consensus */
    if(io_info_md->dxpl_cache->xfer_mode == H5FD_MPIO_INDEPENDENT) {
        local_cause = H5D_MPIO_SET_INDEPENDENT;
        global_cause = H5D_MPIO_SET_INDEPENDENT;
        HGOTO_DONE(FALSE);
    }

    for (i = 0; i < count; i++)
    {
        dset = io_info_md->dsets_info[i].dset;
        file_space = io_info_md->dsets_info[i].file_space;
        mem_space = io_info_md->dsets_info[i].mem_space;
        type_info = io_info_md->dsets_info[i].type_info;

        /* Optimized MPI types flag must be set and it must be collective IO */
        /* (Don't allow parallel I/O for the MPI-posix driver, since it doesn't do real collective I/O) */
        if(!(H5S_mpi_opt_types_g && io_info_md->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE
                && !IS_H5FD_MPIPOSIX(dset->oloc.file))) {
            local_cause |= H5D_MPIO_SET_MPIPOSIX;
        } /* end if */

        /* Don't allow collective operations if datatype conversions need to happen */
        if(!type_info.is_conv_noop) {
            local_cause |= H5D_MPIO_DATATYPE_CONVERSION;
        } /* end if */

        /* Don't allow collective operations if data transform operations should occur */
        if(!type_info.is_xform_noop) {
            local_cause |= H5D_MPIO_DATA_TRANSFORMS;
        } /* end if */

        /* Check whether these are both simple or scalar dataspaces */
        if(!((H5S_SIMPLE == H5S_GET_EXTENT_TYPE(mem_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(mem_space))
                && (H5S_SIMPLE == H5S_GET_EXTENT_TYPE(file_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(file_space)))) {
            local_cause |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;
        } /* end if */

        /* Can't currently handle point selections */
        if(H5S_SEL_POINTS == H5S_GET_SELECT_TYPE(mem_space)
                || H5S_SEL_POINTS == H5S_GET_SELECT_TYPE(file_space)) {
            local_cause |= H5D_MPIO_POINT_SELECTIONS;
        } /* end if */

        /* Dataset storage must be contiguous or chunked */
        if(!(dset->shared->layout.type == H5D_CONTIGUOUS ||
                dset->shared->layout.type == H5D_CHUNKED)) {
            local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
        } /* end if */

        /* check if external-file storage is used */
        if (dset->shared->dcpl_cache.efl.nused > 0) {
            local_cause |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
        }

        /* The handling of memory space is different for chunking and contiguous
         *  storage.  For contiguous storage, mem_space and file_space won't change
         *  when it it is doing disk IO.  For chunking storage, mem_space will
         *  change for different chunks. So for chunking storage, whether we can
         *  use collective IO will defer until each chunk IO is reached.
         */

        /* Don't allow collective operations if filters need to be applied */
        if(dset->shared->layout.type == H5D_CHUNKED) {
            if(dset->shared->dcpl_cache.pline.nused > 0) {
                local_cause |= H5D_MPIO_FILTERS;
            } /* end if */
        } /* end if */
    } /* end for loop */

    /* Form consensus opinion among all processes about whether to perform
     * collective I/O
     */
    if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&local_cause, &global_cause, 1, MPI_INT, MPI_BOR, io_info_md->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

    ret_value = global_cause > 0 ? FALSE : TRUE;

    #ifdef JK_DBG
    printf ("JKDBG p:%d %s:%d> local_cause:%d  global_cause:%d\n", getpid(), __FUNCTION__,__LINE__, local_cause, global_cause );
        fflush(stdout);
    #endif


done:
    /* Write the local value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_LOCAL_NO_COLLECTIVE_CAUSE_NAME, &local_cause) < 0)
       HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set local no collective cause property")

    /* Write the global value of no-collective-cause to the DXPL. */
    if(H5P_set(dx_plist, H5D_MPIO_GLOBAL_NO_COLLECTIVE_CAUSE_NAME, &global_cause) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set global no collective cause property")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__mpio_opt_possible_mdset() */



/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_read_mdset
 *
 * Purpose:     MPI-IO function to read directly from app buffer to file.
 *
 *              This was referred from H5D__mpio_select_read for 
 *              multi-dset work.
 *
 * Return:      non-negative on success, negative on failure.
 *
 * Modification: Jonathan Kim  Nov, 2013
 *   Modified from the previous H5D__mpio_select_read.
 *   This is part multi-dset work.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_read_mdset(const H5D_io_info_md_t *io_info_md, hsize_t mpi_buf_count, 
    const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space)
{
    /* all dsets are in the same file, so just get it from the first dset */
    const H5F_t *file = io_info_md->dsets_info[0].dset->oloc.file;

    void *rbuf = NULL;
    /* memory addr from a piece with lowest file addr */
    rbuf = io_info_md->base_maddr_r;

    #ifdef JK_DBG
    printf("JKDBG %s:%d> rbuf: %x\n", __FILE__, __LINE__, rbuf);
    #endif
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if(H5F_block_read(file, H5FD_MEM_DRAW, io_info_md->store_faddr, (size_t)mpi_buf_count, io_info_md->dxpl_id, rbuf) < 0)
       HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't finish collective parallel write")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_read_mdset() */


/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_write_mdset
 *
 * Purpose:     MPI-IO function to write directly from app buffer to file.
 *
 *              This was referred from H5D__mpio_select_write for 
 *              multi-dset work.
 *
 * Return:      non-negative on success, negative on failure.
 *
 * Modification: Jonathan Kim  Nov, 2013
 *   Modified from the previous H5D__mpio_select_write.
 *   This is part multi-dset work.
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_write_mdset(const H5D_io_info_md_t *io_info_md, hsize_t mpi_buf_count, 
    const H5S_t UNUSED *file_space, const H5S_t UNUSED *mem_space)
{
    /* all dsets are in the same file, so just get it from the first dset */
    const H5F_t *file = io_info_md->dsets_info[0].dset->oloc.file;

    const void *wbuf = NULL;
    /* memory addr from a piece with lowest file addr */
    wbuf = io_info_md->base_maddr_w;

    #ifdef JK_DBG
    printf("JKDBG %s:%d> wbuf: %x\n", __FILE__, __LINE__, wbuf);
    #endif
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if(H5F_block_write(file, H5FD_MEM_DRAW, io_info_md->store_faddr, (size_t)mpi_buf_count, io_info_md->dxpl_id, wbuf) < 0)
       HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't finish collective parallel write")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_write_mdset() */



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
 * Function:    H5D__mpio_get_sum_piece
 *
 * Purpose:     Routine for obtaining total number of pieces to cover
 *              hyperslab selection selected by all processors.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 * Modification: Jonathan Kim  Nov, 2013
 *   Modified from the previous H5D__mpio_get_sum_chunk.
 *   This is part multi-dset work.
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_sum_piece(const H5D_io_info_md_t *io_info_md, size_t *sum_chunkf)
{
    size_t num_chunkf;             /* Number of chunks to iterate over */
    size_t ori_num_chunkf;
    int mpi_code;               /* MPI return code */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Get the number of chunks to perform I/O on */
    num_chunkf = 0;
    ori_num_chunkf = H5SL_count(io_info_md->sel_pieces);
    H5_ASSIGN_OVERFLOW(num_chunkf, ori_num_chunkf, size_t, int);

    /* Determine the summation of number of chunks for all processes */
    if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&num_chunkf, sum_chunkf, 1, MPI_UNSIGNED, MPI_SUM, io_info_md->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_sum_piece() */


/*-------------------------------------------------------------------------
 * Function:    H5D__piece_mdset_io
 *
 * Purpose:     Routine for
 *               choose an IO option:
 *                 a) Single collective IO defined by one MPI derived datatype to link through all pieces (chunks/contigs). Default.
 *               Note: previously there were other options, but cutoff as part of multi-dset work.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * programmer: Jonathan Kim  Nov, 2013
 *   Refactored from the previous H5D__chunk_collective_io.
 *   This is part multi-dset work.
 *
 * Modification:
 *  - Refctore to remove multi-chunk-without-opimization feature and update for
 *    multi-chunk-io accordingly
 * Programmer: Jonathan Kim
 * Date: 2012-10-10
 *
 * Original Programmer:  Muqun Yang Monday, Feb. 13th, 2006
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__piece_mdset_io(const hid_t file_id, const size_t count, H5D_io_info_md_t *io_info_md)
{
    H5P_genplist_t *dx_plist;           /* Pointer to DXPL */
    H5FD_mpio_chunk_opt_t chunk_opt_mode;
    int         io_option = H5D_ONE_LINK_CHUNK_IO;
    int         sum_chunk = -1;
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    htri_t      temp_not_link_io = FALSE;
#endif
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(io_info_md);
    HDassert(io_info_md->using_mpi_vfd);

    /* Obtain the data transfer properties */
    if(NULL == (dx_plist = H5I_object(io_info_md->dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* Check the optional property list on what to do with collective chunk IO. */
    chunk_opt_mode = (H5FD_mpio_chunk_opt_t)H5P_peek_unsigned(dx_plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME);
    if(H5FD_MPIO_CHUNK_ONE_IO == chunk_opt_mode)
        io_option = H5D_ONE_LINK_CHUNK_IO;      /*no opt*/

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    htri_t            check_prop;
    int               new_value;

    /*** Test collective chunk user-input optimization APIs. ***/
    check_prop = H5Pexist(io_info_md->dxpl_id, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME);
    if(check_prop > 0) {
        if(H5D_ONE_LINK_CHUNK_IO == io_option) {
            new_value = 0;
            if(H5Pset(io_info_md->dxpl_id, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME, &new_value) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
    } /* end if */
#endif

    /* step 2:  Go ahead to do IO.*/
    if(H5D_ONE_LINK_CHUNK_IO == io_option)  {
        if(H5D__all_piece_collective_io(file_id, count, io_info_md, dx_plist) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish linked chunk MPI-IO")
    } /* end if */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__piece_mdset_io */


/*-------------------------------------------------------------------------
 * Function:    H5D__mdset_collective_read
 *
 * Purpose:     Read directly from pieces (chunks/contig) in file into 
 *              application memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 * Modification: Jonathan Kim  Nov, 2013
 *   Modified from the previous H5D__chunk/contig_collective_read.
 *   This is part multi-dset work.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mdset_collective_read(const hid_t file_id, const size_t count, H5D_io_info_md_t *io_info_md)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if(H5D__piece_mdset_io(file_id, count, io_info_md) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mdset_collective_read() */



/*-------------------------------------------------------------------------
 * Function:    H5D__mdset_collective_write
 *
 * Purpose:     Write directly to pieces (chunks/contig) in file into 
 *              application memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 * Modification: Jonathan Kim  Nov, 2013
 *   Modified from the previous H5D__chunk/contig_collective_write.
 *   This is part multi-dset work.
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mdset_collective_write(const hid_t file_id, const size_t count, H5D_io_info_md_t *io_info_md)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if(H5D__piece_mdset_io(file_id, count, io_info_md) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mdset_collective_write() */


/*-------------------------------------------------------------------------
 * Function:    H5D__all_piece_collective_io
 *
 * Purpose:     Routine for single collective IO with one MPI derived datatype to link with all pieces (chunks + contig)
 *
 *              1. Use the piece addresses and piece info sorted in skiplist
 *              2. Build up MPI derived datatype for each chunk
 *              3. Build up the final MPI derived datatype
 *              4. Use common collective IO routine to do MPI-IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer: Jonathan Kim  Nov, 2013
 *   Refactored from the previous H5D__link_chunk_collective_io (Muqun Yang Feb. 2006).
 *   This is part multi-dset work.
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__all_piece_collective_io(UNUSED const hid_t file_id, const size_t count, 
    H5D_io_info_md_t *io_info_md, H5P_genplist_t *dx_plist)
{
    MPI_Datatype chunk_final_mtype;         /* Final memory MPI datatype for all chunks with seletion */
    hbool_t chunk_final_mtype_is_derived = FALSE;
    MPI_Datatype chunk_final_ftype;         /* Final file MPI datatype for all chunks with seletion */
    hbool_t chunk_final_ftype_is_derived = FALSE;
    H5D_storage_t ctg_store;                /* Storage info for "fake" contiguous dataset */
    size_t              sum_chunk_allproc=0; /* sum of selected chunk from all process */
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
    #ifdef JK_DBG
    int mpi_rank;
    #endif

    FUNC_ENTER_STATIC

    /* set actual_io_mode */ 
    for (i=0; i < count; i++) {
        if (io_info_md->dsets_info[i].layout->type == H5D_CHUNKED)
            actual_io_mode |= H5D_MPIO_CHUNK_COLLECTIVE;
        else if (io_info_md->dsets_info[i].layout->type == H5D_CONTIGUOUS) {
            actual_io_mode |= H5D_MPIO_CONTIGUOUS_COLLECTIVE;

            /* if only single-dset */
            if (1 == count)
                actual_chunk_opt_mode = H5D_MPIO_NO_CHUNK_OPTIMIZATION; 
        }
        else
            HGOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "unsupported storage layout")
    }

    #ifdef JK_DBG
    mpi_rank = H5F_mpi_get_rank(io_info_md->dsets_info[0].dset->oloc.file);
    #endif

    /* Set the actual-chunk-opt-mode property. */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")

    /* Set the actual-io-mode property.
     * Link chunk I/O does not break to independent, so can set right away */
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Get the sum # of chunks */
    if(H5D__mpio_get_sum_piece(io_info_md, &sum_chunk_allproc) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL, "unable to obtain the total chunk number of all processes");

    #ifdef JK_DBG
    printf("JKDBG %s|%d P%d> sum_chunk_allproc: %d\n", __FILE__, __LINE__, mpi_rank, sum_chunk_allproc);
    #endif

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
    num_chunk = H5SL_count(io_info_md->sel_pieces);
    H5_CHECK_OVERFLOW(num_chunk, size_t, int);

    #ifdef JK_DBG
    printf("JKDBG %s|%d P%d> num_chunk (this proc skiplist): %d\n", __FILE__, __LINE__, mpi_rank, num_chunk);
    #endif

    /* Set up MPI datatype for chunks selected */
    if(num_chunk) {
        #ifdef JK_DBG
        printf("JKDBG %s:%d P%d> SELECTION for this process\n", __FILE__, __LINE__, mpi_rank);
        #endif
        /* Allocate chunking information */
        if(NULL == (chunk_mtype           = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory datatype buffer")
        if(NULL == (chunk_ftype           = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file datatype buffer")
        if(NULL == (chunk_file_disp_array      = (MPI_Aint *)H5MM_malloc(num_chunk * sizeof(MPI_Aint))))
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

        #ifdef JK_DBG2
        /* print dset_info addr from each piece.
         * This is to verify if each piece has correct dset_info of its own
         */
        {
        /* Start at first node in chunk skip list */
        int i = 0;
        H5SL_node_t    *piece_node;         /* Current node in chunk skip list */
        H5D_piece_info_t *piece_info;
        if(NULL == (piece_node = H5SL_first(io_info_md->sel_pieces)))
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get piece node from skipped list")

        /* Iterate over all pieces for this process */
        while(piece_node) {
            if(NULL == (piece_info = H5SL_item(piece_node)))
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get piece info from skipped list")

                printf("JKDBG %s|%d P%d> piece%d dset_info addr: %x\n", __FILE__, __LINE__, mpi_rank,i, piece_info->dset_info);
                //printf("JKDBG %s|%d P%d> piece%d addr: %llu\n", __FILE__, __LINE__, mpi_rank,i, piece_info->piece_addr);
                printf("JKDBG %s|%d P%d> piece%d fspace: %x\n", __FILE__, __LINE__, mpi_rank,i, piece_info->fspace);
                printf("JKDBG %s|%d P%d> piece%d mspace: %x\n", __FILE__, __LINE__, mpi_rank,i, piece_info->mspace);


            /* Advance to next piece in list */
            i++;
            piece_node = H5SL_next(piece_node);
        } /* end while */
        }
        #endif // JK_DBG

        /* get first piece, which is sorted in skiplist */
        if(NULL == (piece_node = H5SL_first(io_info_md->sel_pieces)))
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
            if(NULL == (piece_info = (H5D_piece_info_t *)H5SL_item(piece_node)))
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL,"couldn't get piece info from skipped list")
            
                #ifdef JK_DBG2
                //printf("JKDBG %s|%d P%d> piece%d dset_info addr: %x\n", __FILE__, __LINE__, mpi_rank,u, piece_info->dset_info);
                //printf("JKDBG %s|%d P%d> piece%d addr: %llu\n", __FILE__, __LINE__, mpi_rank,u, piece_info->faddr);
                printf("JKDBG %s|%d P%d> piece%d fspace: %x\n", __FILE__, __LINE__, mpi_rank,u, piece_info->fspace);
                printf("JKDBG %s|%d P%d> piece%d mspace: %x\n", __FILE__, __LINE__, mpi_rank,u, piece_info->mspace);
                #endif

            /* Disk MPI derived datatype */
            if(H5S_mpio_space_type(piece_info->fspace,
                    piece_info->dset_info->type_info.src_type_size, &chunk_ftype[u], &chunk_mpi_file_counts[u], &(chunk_mft_is_derived_array[u])) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI file type")

            /* Buffer MPI derived datatype */
            if(H5S_mpio_space_type(piece_info->mspace,
                    piece_info->dset_info->type_info.dst_type_size, &chunk_mtype[u], &chunk_mpi_mem_counts[u], &(chunk_mbt_is_derived_array[u])) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI buf type")

            /* Piece address relative to the first piece addr 
             * Assign piece address to MPI displacement 
             * (assume MPI_Aint big enough to hold it) */
            chunk_file_disp_array[u] = (MPI_Aint)piece_info->faddr - (MPI_Aint)ctg_store.contig.dset_addr;

            if(io_info_md->op_type == H5D_IO_OP_WRITE) {
                chunk_mem_disp_array[u] = (MPI_Aint)piece_info->dset_info->u.wbuf - (MPI_Aint)base_wbuf_addr;
                #if 0 // JK_DBG
                printf("dset_info->u.wbuf: %x\n", piece_info->dset_info->u.wbuf);
                printf("base_wbuf_addr: %x\n", base_wbuf_addr);
                #endif
            } 
            else if (io_info_md->op_type == H5D_IO_OP_READ) {
                chunk_mem_disp_array[u] = (MPI_Aint)piece_info->dset_info->u.rbuf - (MPI_Aint)base_rbuf_addr;
                #if 0 // JK_DBG
                printf("dset_info->u.rbuf: %x\n", piece_info->dset_info->u.rbuf);
                printf("base_rbuf_addr: %x\n", base_rbuf_addr);
                #endif
            }
             #if 0 // JK_DBG
             printf("JKDBG %s:%d P%d> mem_disp[%d]: ", __FILE__, __LINE__, mpi_rank, u);
             printf("%x\n", chunk_mem_disp_array[u]);
             #endif

            /* Advance to next piece in list */
            u++;
            piece_node = H5SL_next(piece_node);
        } /* end while */

        /* Create final MPI derived datatype for the file */
        /* Note: consider to use 'MPI_Type_create_struct' instead of
         * 'MPI_Type_struct' which is deprecated as of MPI-2 - JKM */
        #if defined(MPI_VERSION) && MPI_VERSION >= 2
        if(MPI_SUCCESS != (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_file_counts, chunk_file_disp_array, chunk_ftype, &chunk_final_ftype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
        #else
        if(MPI_SUCCESS != (mpi_code = MPI_Type_struct((int)num_chunk, chunk_mpi_file_counts, chunk_file_disp_array, chunk_ftype, &chunk_final_ftype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_struct failed", mpi_code)
        #endif
        if(MPI_SUCCESS != (mpi_code = MPI_Type_commit(&chunk_final_ftype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
        chunk_final_ftype_is_derived = TRUE;

        /* Create final MPI derived datatype for memory */
        /* Note: consider to use 'MPI_Type_create_struct' instead of
         * 'MPI_Type_struct' which is deprecated as of MPI-2 - JKM */
        #if defined(MPI_VERSION) && MPI_VERSION >= 2
        if(MPI_SUCCESS != (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_mem_counts, chunk_mem_disp_array, chunk_mtype, &chunk_final_mtype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
        #else
        if(MPI_SUCCESS != (mpi_code = MPI_Type_struct((int)num_chunk, chunk_mpi_mem_counts, chunk_mem_disp_array, chunk_mtype, &chunk_final_mtype)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_struct failed", mpi_code)
        #endif
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
        #ifdef JK_DBG
        printf("JKDBG %s:%d P%d> NO Selection for this process\n", __FILE__, __LINE__, mpi_rank);
        #endif

        /* since this process doesn't do any io, just pass a valid addr.
         * at this point dset object hear address is availbe to any 
         * process, so just pass it. 0x0 also work fine */
        ctg_store.contig.dset_addr = 0x0;
        /* or ctg_store.contig.dset_addr = io_info_md->dsets_info[0].dset->oloc.addr; */
        
        /* just provide a valid mem address. no actual IO occur */
        base_wbuf_addr = io_info_md->dsets_info[0].u.wbuf;
        base_rbuf_addr = io_info_md->dsets_info[0].u.rbuf;

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
    io_info_md->store_faddr = ctg_store.contig.dset_addr;
    io_info_md->base_maddr_w = base_wbuf_addr;
    io_info_md->base_maddr_r = base_rbuf_addr;

    /* Perform final collective I/O operation */
    if(H5D__final_collective_io_mdset(io_info_md, mpi_buf_count, &chunk_final_ftype, &chunk_final_mtype) < 0)
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
 * Function:    H5D__final_collective_io_mdset
 *
 * Purpose:     Routine for the common part of collective IO with different storages.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 * Modification: Jonathan Kim  Nov, 2013
 *   Modified from the previous H5D__final_collective_io.
 *   This is part multi-dset work.
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__final_collective_io_mdset(H5D_io_info_md_t *io_info_md,
    hsize_t mpi_buf_count, MPI_Datatype *mpi_file_type, MPI_Datatype *mpi_buf_type)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Pass buf type, file type to the file driver.  */
    if(H5FD_mpi_setup_collective(io_info_md->dxpl_id, mpi_buf_type, mpi_file_type) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O properties")

    if(io_info_md->op_type == H5D_IO_OP_WRITE) {
        if((io_info_md->io_ops.single_write_md)(io_info_md, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed")
    } /* end if */
    else {
        if((io_info_md->io_ops.single_read_md)(io_info_md, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed")
    } /* end else */

done:
#ifdef H5D_DEBUG
if(H5DEBUG(D))
    HDfprintf(H5DEBUG(D),"ret_value before leaving final_collective_io=%d\n",ret_value);
#endif
      FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__final_collective_io_mdset */


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

#endif /* H5_HAVE_PARALLEL */

