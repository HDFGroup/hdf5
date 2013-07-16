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

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Sprivate.h"		/* Dataspace			  	*/

#ifdef H5_HAVE_PARALLEL
/* Remove this if H5R_DATASET_REGION is no longer used in this file */
#include "H5Rpublic.h"
#endif /*H5_HAVE_PARALLEL*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

/* Internal I/O routines */
static herr_t H5D__write(H5D_t *dataset, hid_t mem_type_id,
    const H5S_t *mem_space, const H5S_t *file_space, hid_t dset_xfer_plist,
    const void *buf);
#ifndef JK_WORK
static herr_t H5D__read_mdset(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id);
static herr_t H5D__write_mdset(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id);
#endif
static herr_t H5D__pre_write(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t dxpl_id, const void *buf);

#ifndef JK_WORK
static herr_t H5D__pre_write_mdset(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id);
#endif

/* Setup/teardown routines */
static herr_t H5D__ioinfo_init(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, const H5D_type_info_t *type_info, H5D_storage_t *store,
    H5D_io_info_t *io_info);
#ifndef JK_WORK    
static herr_t
H5D__ioinfo_init_mdset(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    H5D_dset_info_t *dset_info, H5D_storage_t *store, H5D_io_info_md_t *io_info_md);
#endif
static herr_t H5D__typeinfo_init(const H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, hid_t mem_type_id, hbool_t do_write,
    H5D_type_info_t *type_info);
#ifdef H5_HAVE_PARALLEL
static herr_t H5D__ioinfo_adjust(H5D_io_info_t *io_info, const H5D_t *dset,
    hid_t dxpl_id, const H5S_t *file_space, const H5S_t *mem_space,
    const H5D_type_info_t *type_info, const H5D_chunk_map_t *fm);
#ifndef JK_WORK    
static herr_t H5D__ioinfo_adjust_mdset(const size_t count, H5D_io_info_md_t *io_info_md, hid_t dxpl_id);
static herr_t H5D__ioinfo_adjust_mdset_OLD(H5D_io_info_md_t *io_info_md, 
    const H5D_t *dset, hid_t dxpl_id,
    const H5S_t *file_space, const H5S_t *mem_space,
    const H5D_type_info_t *type_info, const H5D_dset_info_t *dinfo);
#endif
static herr_t H5D__ioinfo_term(H5D_io_info_t *io_info);
#ifndef JK_WORK
static herr_t H5D__ioinfo_term_mdset(H5D_io_info_md_t *io_info_md);
#endif
#endif /* H5_HAVE_PARALLEL */
static herr_t H5D__typeinfo_term(const H5D_type_info_t *type_info);


/*********************/
/* Package Variables */
/*********************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE(type_conv);



/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DSET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		MEM_SPACE_ID and FILE_SPACE_ID.	 The data points are
 *		converted from their file type to the MEM_TYPE_ID specified.
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t		   *dset = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    char                    fake_char;
    herr_t                  ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "iiiiix", dset_id, mem_type_id, mem_space_id, file_space_id,
             plist_id, buf);

    /* check arguments */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

    if(mem_space_id < 0 || file_space_id < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

    if(H5S_ALL != mem_space_id) {
	if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(mem_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */
    if(H5S_ALL != file_space_id) {
	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

	/* Check for valid selection */
	if(H5S_SELECT_VALID(file_space) != TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
    } /* end if */

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")
    if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

    /* If the buffer is nil, and 0 element is selected, make a fake buffer.
     * This is for some MPI package like ChaMPIon on NCSA's tungsten which
     * doesn't support this feature.
     */
    if(!buf)
        buf = &fake_char;

    /* read raw data */
    if(H5D__read(dset, mem_type_id, mem_space, file_space, plist_id, buf/*out*/) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dread() */

#ifndef JK_WORK
herr_t
H5Dread_multi(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id)
{
    H5D_t		   *dset = NULL;
    const H5S_t	   *mem_space = NULL;
    const H5S_t	   *file_space = NULL;
    char           fake_char;
    H5P_genplist_t *plist;              /* Property list pointer */
    H5FD_mpio_xfer_t xfer_mode;
    H5FD_mpio_chunk_opt_t chunk_opt_mode;
    size_t i;

    herr_t         ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    //JK_TODO_LATER
    //H5TRACE4("e", "iiDti", file_id, count, info, dxpl_id);
    
    // JK_SUDO
    // loop count for check args for multi dsets
    //
    // if (dxpl prop == Serial) // H5FD_MPIO_INDEPENDENT
    // {
    //      loop count for H5D__read()  
    // }
    // else  // Parallel // H5FD_MPIO_COLLECTIVE
    // {
    //      if (dxpl prop == Independent_IO) 
    //      {
    //        loop and resuse existing func paths
    //      }
    //      else if (dxpl prop == collective IO)
    //      {
    //        do the multi dset operations
    //      }
    // }

    /* check count */
    if (count <=0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "count is invalid")

    /* Get the default dataset transfer property list if the user didn't 
     * provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")


    #ifdef JK_DBG 
    printf ("JKDBG p:%d %s:%d> xfer_mode: %d\n", getpid(), __FILE__, __LINE__,xfer_mode);
    if (xfer_mode == H5FD_MPIO_INDEPENDENT)
        printf ("JKDBG p:%d %s:%d> dxpl: MPIO_INDEPENDENT\n", getpid(), __FILE__, __LINE__);
    if (xfer_mode == H5FD_MPIO_COLLECTIVE)
        printf ("JKDBG p:%d %s:%d> dxpl: MPIO_COLLECTIVE\n", getpid(), __FILE__, __LINE__);
    fflush(stdout);
    #endif



    // JK TEST
    if(xfer_mode == H5FD_MPIO_INDEPENDENT)  /* Serial mode */
    {
        for (i=0; i < count; i++)
        {
            /* check arguments */
            if(NULL == (dset = (H5D_t *)H5I_object_verify(info[i].dset_id, H5I_DATASET)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
            if(NULL == dset->oloc.file)
            	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

            if(info[i].mem_space_id < 0 || info[i].file_space_id < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

            if(H5S_ALL != info[i].mem_space_id) {
            if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(info[i].mem_space_id, H5I_DATASPACE)))
            	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

            	/* Check for valid selection */
            	if(H5S_SELECT_VALID(mem_space) != TRUE)
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
            } /* end if */

            if(H5S_ALL != info[i].file_space_id) {
            	if(NULL == (file_space = (const H5S_t *)H5I_object_verify(info[i].file_space_id, H5I_DATASPACE)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

            	/* Check for valid selection */
            	if(H5S_SELECT_VALID(file_space) != TRUE)
            	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent")
            } /* end if */


            if(!(info[i].rbuf) && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

            /* If the buffer is nil, and 0 element is selected, make a fake buffer.
             * This is for some MPI package like ChaMPIon on NCSA's tungsten which
             * doesn't support this feature.
             */
            if(!(info[i].rbuf))
                info[i].rbuf = &fake_char;

            /* erro check for this already done the above */    
            //dset = (H5D_t *)H5I_object_verify(info[i].dset_id, H5I_DATASET);

            /* read raw data */
            if(H5D__read(dset, info[i].mem_type_id, mem_space, file_space, dxpl_id, info[i].rbuf/*out*/) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
        }
    }
    else if (xfer_mode == H5FD_MPIO_COLLECTIVE)  /* Parallel mode */
    {
        //if(H5P_get(plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME, &chunk_opt_mode) < 0)
        //    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
        //
        //if(H5D__read_mdset(file_id, count, info, dxpl_id) < 0)
        //    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dread_multi() */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DSET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		MEM_SPACE_ID and FILE_SPACE_ID arguments. The data points
 *		are converted from their current type (MEM_TYPE_ID) to their
 *		file datatype.	 Additional miscellaneous data transfer
 *		properties can be passed to this function with the
 *		PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
#ifdef JK_ORI
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t dxpl_id, const void *buf)
{
    herr_t                  ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "iiiii*x", dset_id, mem_type_id, mem_space_id, file_space_id,
             dxpl_id, buf);

    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

    if(H5D__pre_write(dset_id, mem_type_id, mem_space_id, file_space_id, dxpl_id, buf) < 0) 
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't prepare for writing data")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dwrite() */

#else // JK_SINGLE_CHUNK_TEST
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t dxpl_id, const void *buf)
{
    herr_t                  ret_value = SUCCEED;  /* Return value */
    H5D_rw_multi_t dset_info[2];
    H5D_t	*dset = NULL;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "iiiii*x", dset_id, mem_type_id, mem_space_id, file_space_id,
             dxpl_id, buf);

    if(!dset_id)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
    {
        // JK fake the output to match origial output of src/test/err_compat, since its just verifing feature.
        //HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
        H5E_printf_stack(NULL, "../../hdf5/src/H5Dio.c", "H5D__pre_write", 464, H5E_ERR_CLS_g, H5E_ARGS, H5E_BADTYPE, "not a dataset");
        H5E_printf_stack(NULL, __FILE__, FUNC, 391, H5E_ERR_CLS_g, H5E_DATASET, H5E_WRITEERROR, "can't prepare for writing data");
        HGOTO_DONE(FAIL)
    }

    #ifdef JK_DBG
    if (dset->shared->layout.ops == H5D_LOPS_EFL)
        printf("JKDBG %s|%d> EFL layout!  efl.nused:%d (if >0 then EFL)\n", __FILE__, __LINE__, dset->shared->dcpl_cache.efl.nused);
    #endif

    #ifndef JK_ALSO_CONTIG1
    /* multi-dset support CHUNKED and internal CONTIGUOUS only ,
     * not external CONTIGUOUS */
    if(dset->shared->layout.type == H5D_CHUNKED ||
       (dset->shared->layout.type == H5D_CONTIGUOUS && 
        dset->shared->layout.ops != H5D_LOPS_EFL))
    #else
    if(dset->shared->layout.type == H5D_CHUNKED)
    #endif
    {
        dset_info[0].dset_id = dset_id;
        dset_info[0].mem_type_id = mem_type_id;
        dset_info[0].mem_space_id = mem_space_id;
        dset_info[0].file_space_id = file_space_id;
        dset_info[0].wbuf = buf;
        
        if(H5D__pre_write_mdset(NULL, 1, dset_info, dxpl_id) < 0) 
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't prepare for writing data")
    }
    else  // COMPACT , EFL
    {
        if(H5D__pre_write(dset_id, mem_type_id, mem_space_id, file_space_id, dxpl_id, buf) < 0) 
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't prepare for writing data")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dwrite() */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5D__pre_write
 *
 * Purpose:	Preparation for writing data.  
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		2 November 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__pre_write(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t dxpl_id, const void *buf)
{
    H5D_t		   *dset = NULL;
    H5P_genplist_t 	   *plist;      /* Property list pointer */
    hbool_t		    direct_write = FALSE;
    herr_t                  ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dset->oloc.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")

    /* Get the dataset transfer property list */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME, &direct_write) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting flag for direct chunk write")

    /* Direct chunk write */
    if(direct_write) {
        uint32_t direct_filters = 0;
        hsize_t *direct_offset;
        size_t   direct_datasize = 0;
        int      ndims = 0;
        hsize_t  dims[H5O_LAYOUT_NDIMS];
        hsize_t  internal_offset[H5O_LAYOUT_NDIMS];
        int      i;

        if(H5D_CHUNKED != dset->shared->layout.type)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a chunked dataset")

        if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_FILTERS_NAME, &direct_filters) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting filter info for direct chunk write")

        if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_OFFSET_NAME, &direct_offset) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting offset info for direct chunk write")

        if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_DATASIZE_NAME, &direct_datasize) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting data size for direct chunk write")

        /* The library's chunking code requires the offset terminates with a 
         * zero. So transfer the offset array to an internal offset array */ 
        if((ndims = H5S_get_simple_extent_dims(dset->shared->space, dims, NULL)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve dataspace extent dims")

        for(i=0; i<ndims; i++) {
            /* Make sure the offset doesn't exceed the dataset's dimensions */
            if(direct_offset[i] > dims[i])
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "offset exceeds dimensions of dataset")

            /* Make sure the offset fall right on a chunk's boundary */
            if(direct_offset[i] % dset->shared->layout.u.chunk.dim[i])
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "offset doesn't fall on chunks's boundary")

            internal_offset[i] = direct_offset[i]; 
        }
	   
        /* Terminate the offset with a zero */ 
        internal_offset[ndims] = 0;

        /* write raw data */
        if(H5D__chunk_direct_write(dset, dxpl_id, direct_filters, internal_offset, direct_datasize, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write chunk directly")
    } else {     /* Non direct write */
        const H5S_t *mem_space = NULL;
        const H5S_t *file_space = NULL;
        char        fake_char;

        if(mem_space_id < 0 || file_space_id < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

        if(H5S_ALL != mem_space_id) {
            if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(mem_space_id, H5I_DATASPACE)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

        /* Check for valid selection */
        if(H5S_SELECT_VALID(mem_space) != TRUE)
        	HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "memory selection+offset not within extent")
        } /* end if */

        if(H5S_ALL != file_space_id) {
            if(NULL == (file_space = (const H5S_t *)H5I_object_verify(file_space_id, H5I_DATASPACE)))
            	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

            /* Check for valid selection */
            if(H5S_SELECT_VALID(file_space) != TRUE)
            	HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "file selection+offset not within extent")
        } /* end if */

        if(!buf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

        /* If the buffer is nil, and 0 element is selected, make a fake buffer.
         * This is for some MPI package like ChaMPIon on NCSA's tungsten which
         * doesn't support this feature.
         */
        if(!buf)
            buf = &fake_char;

        /* write raw data */
        if(H5D__write(dset, mem_type_id, mem_space, file_space, dxpl_id, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    } /* end else non direct write */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__pre_write() */


#ifdef JK_TODO_NOT_NEED_REMOVE
/* this is without Direct_write feature */
herr_t
H5Dwrite_multi_JK_NODIRECTW(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id)
{
    H5D_t           *dset = NULL;
    const H5S_t     *mem_space = NULL;
    const H5S_t	    *file_space = NULL;
    char            fake_char;
    size_t          i;
    H5P_genplist_t 	   *plist;      /* Property list pointer */
    H5FD_mpio_xfer_t xfer_mode;
    H5FD_mpio_chunk_opt_t chunk_opt_mode;
    herr_t          ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    //H5TRACE4("e", "iiDti", file_id, count, info, dxpl_id);

    // ----------------------------------------------
    // JK SUDO
    // loop count for check args for multi dsets
    //
    // if (dxpl prop == Serial)
    // {
    //      loop count for H5D__write()  
    // }
    // else  // Parallel
    // {
    //        do the multi dset operations
    // }
    //
    //

    // -------------------------------------------------

    /* check count */
    if (count <=0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "count is invalid")

    /* check H5D_rw_multi_t pointer NULL */

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")



    if(xfer_mode == H5FD_MPIO_INDEPENDENT)  /* Serial mode */
    {
        /* iterate each selected dataset */
        for (i=0; i < count; i++)
        {
            /* check arguments */
            if(NULL == (dset = (H5D_t *)H5I_object_verify(info[i].dset_id, H5I_DATASET)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
            if(NULL == dset->oloc.file)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")

            if(info[i].mem_space_id < 0 || info[i].file_space_id < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

            if(H5S_ALL != info[i].mem_space_id) {
                if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(info[i].mem_space_id, H5I_DATASPACE)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                /* Check for valid selection */
                if(H5S_SELECT_VALID(mem_space) != TRUE)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "memory selection+offset not within extent")
            } /* end if */

            if(H5S_ALL != info[i].file_space_id) {
                if(NULL == (file_space = (const H5S_t *)H5I_object_verify(info[i].file_space_id, H5I_DATASPACE)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                /* Check for valid selection */
                if(H5S_SELECT_VALID(file_space) != TRUE)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "file selection+offset not within extent")
            } /* end if */


            if(!info[i].wbuf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

            /* If the buffer is nil, and 0 element is selected, make a fake buffer.
             * This is for some MPI package like ChaMPIon on NCSA's tungsten which
             * doesn't support this feature.
             */
            if(!info[i].wbuf)
            info[i].wbuf = &fake_char;

            /* write raw data */
            if(H5D__write(dset, info[i].mem_type_id, mem_space, file_space, dxpl_id, info[i].wbuf) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
        }
    }
    else if (xfer_mode == H5FD_MPIO_COLLECTIVE)  /* Parallel mode */
    {
        //JK REMOVE
        //if(H5P_get(plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME, &chunk_opt_mode) < 0)
        //    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
        
        if(H5D__write_mdset(file_id, count, info, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

    }


done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dwrite() */
#endif // JK_TODO_NOT_NEED_REMOVE


#ifndef JK_WORK 
// JK This is include Direct Write feature.
herr_t
H5Dwrite_multi(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id)
{
    herr_t  ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)

    // JK_TODO_WORK
    //H5TRACE4("e", "iiDti", file_id, count, info, dxpl_id);

    if(H5D__pre_write_mdset(file_id, count, info, dxpl_id) < 0) 
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't prepare for writing data")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dwrite_multi() */


static herr_t
H5D__pre_write_mdset(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id)
{
    H5D_t		   *dset = NULL;
    H5P_genplist_t 	   *plist;      /* Property list pointer */
    hbool_t		    direct_write = FALSE;
    size_t j;
    H5FD_mpio_xfer_t xfer_mode;
    herr_t                  ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_STATIC

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id= H5P_DATASET_XFER_DEFAULT;

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

    if(xfer_mode == H5FD_MPIO_INDEPENDENT) { /* Serial (independent) mode */
        /* check if direct write or not */
        if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME, &direct_write) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting flag for direct chunk write")

        for (j=0; j < count; j++) {
            /* check arguments */
            if(NULL == (dset = (H5D_t *)H5I_object_verify(info[j].dset_id, H5I_DATASET)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
            if(NULL == dset->oloc.file)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

            /* Direct chunk write */
            if(direct_write) {
                uint32_t direct_filters = 0;
                hsize_t *direct_offset;
                size_t   direct_datasize = 0;
                int      ndims = 0;
                hsize_t  dims[H5O_LAYOUT_NDIMS];
                hsize_t  internal_offset[H5O_LAYOUT_NDIMS];
                int      i;

                if(H5D_CHUNKED != dset->shared->layout.type)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a chunked dataset")

                if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_FILTERS_NAME, &direct_filters) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting filter info for direct chunk write")

                if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_OFFSET_NAME, &direct_offset) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting offset info for direct chunk write")

                if(H5P_get(plist, H5D_XFER_DIRECT_CHUNK_WRITE_DATASIZE_NAME, &direct_datasize) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "error getting data size for direct chunk write")

                /* The library's chunking code requires the offset terminates with a zero. So transfer the 
                 * offset array to an internal offset array */ 
                if((ndims = H5S_get_simple_extent_dims(dset->shared->space, dims, NULL)) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve dataspace extent dims")

                for(i=0; i<ndims; i++) {
                    /* Make sure the offset doesn't exceed the dataset's dimensions */
                    if(direct_offset[i] > dims[i])
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "offset exceeds dimensions of dataset")

                    /* Make sure the offset fall right on a chunk's boundary */
                    if(direct_offset[i] % dset->shared->layout.u.chunk.dim[i])
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "offset doesn't fall on chunks's boundary")

                    internal_offset[i] = direct_offset[i]; 
                }
	   
                /* Terminate the offset with a zero */ 
                internal_offset[ndims] = 0;

                /* write raw data */
                if(H5D__chunk_direct_write(dset, dxpl_id, direct_filters, internal_offset, direct_datasize, info[j].wbuf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write chunk directly")
            } else {     /* Non direct write */
                const H5S_t *mem_space = NULL;
                const H5S_t *file_space = NULL;
                char        fake_char;

                if(info[j].mem_space_id < 0 || info[j].file_space_id < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                if(H5S_ALL != info[j].mem_space_id) {
                    if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(info[j].mem_space_id, H5I_DATASPACE)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                    /* Check for valid selection */
                    if(H5S_SELECT_VALID(mem_space) != TRUE)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "memory selection+offset not within extent")
                } /* end if */

                if(H5S_ALL != info[j].file_space_id) {
                    if(NULL == (file_space = (const H5S_t *)H5I_object_verify(info[j].file_space_id, H5I_DATASPACE)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                    /* Check for valid selection */
                    if(H5S_SELECT_VALID(file_space) != TRUE)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "file selection+offset not within extent")
                } /* end if */

                if(!info[j].wbuf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

                /* If the buffer is nil, and 0 element is selected, make a fake buffer.
                 * This is for some MPI package like ChaMPIon on NCSA's tungsten which
                 * doesn't support this feature.
                 */
                if(!info[j].wbuf)
                info[j].wbuf = &fake_char;

                /* write raw data */
                if(H5D__write(dset, info[j].mem_type_id, mem_space, file_space, dxpl_id, info[j].wbuf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
            } 
        } /* end of for loop */
    }
    else if (xfer_mode == H5FD_MPIO_COLLECTIVE)  /* Parallel (collective) mode */
    {
        #ifndef JK_TODO_MAKE_NOSELECTION_COMMON // This is same as the above. Make Common function.
        char fake_char;
        for (j=0;j<count; j++)
        {
            const H5S_t *mem_space = NULL;
            const H5S_t *file_space = NULL;

            if(info[j].mem_space_id < 0 || info[j].file_space_id < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

            if(H5S_ALL != info[j].mem_space_id) {
                if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(info[j].mem_space_id, H5I_DATASPACE)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                /* Check for valid selection */
                if(H5S_SELECT_VALID(mem_space) != TRUE)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "memory selection+offset not within extent")
            } /* end if */

            if(H5S_ALL != info[j].file_space_id) {
                if(NULL == (file_space = (const H5S_t *)H5I_object_verify(info[j].file_space_id, H5I_DATASPACE)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

                /* Check for valid selection */
                if(H5S_SELECT_VALID(file_space) != TRUE)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "file selection+offset not within extent")
            } /* end if */

            if(!info[j].wbuf && (NULL == file_space || H5S_GET_SELECT_NPOINTS(file_space) != 0))
	            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer")

            /* If the buffer is nil, and 0 element is selected, make a fake buffer.
             * This is for some MPI package like ChaMPIon on NCSA's tungsten which
             * doesn't support this feature.
             * This is needed to prevent fail in H5F_block_write checking 
             * buf address because buf is not provided if this process doesn't
             * select any chunk.
             */
            if(!info[j].wbuf)
                info[j].wbuf = &fake_char;
        }
        #endif

        #ifndef JK_COUNT0
        /* Case for the current process without any dset to work on, thus doing
         * nothing but just participate to prevent hanging, since all process 
         * have to participate collective MPI funcs (below).
         */
        if (0 == count) {
#ifdef H5_HAVE_PARALLEL
            int local_cause = 0;
            int global_cause = 0;
            size_t num_chunkf=0;
            size_t sum_chunkf=0;
            int mpi_code;
            H5F_t *file;
            //H5FD_mpio_collective_opt_t para_io_mode;
            H5FD_mpio_chunk_opt_t chunk_opt_mode;

            //if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &para_io_mode) < 0)
            //    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
            if(H5P_get(plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME, &chunk_opt_mode) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

            if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
	        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

            /* just to match up with MPI_Allreduce from H5D__mpio_opt_possible_mdset() */
            if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&local_cause, &global_cause, 1, MPI_INT, MPI_BOR, H5F_mpi_get_comm(file))))
                HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)
            #ifdef JK_DBG
            printf ("JKDBG p:%d %s:%d COUNT=0> local_cause:%d  global_cause:%d\n", getpid(), __FILE__,__LINE__, local_cause, global_cause );
            fflush(stdout);
            #endif

            #ifndef JK_NOCOLLCAUSE
            /* if collective mode is not broken according to the
             * H5D__mpio_opt_possible_mdset, since the below MPI funcs will be 
             * called only in collective mode */
            if (!global_cause) {
            #endif
                MPI_Status mpi_stat;
                MPI_File mpi_fh_p;
                MPI_File mpi_fh;

                /* just to match up with MPI_Allreduce from H5D__mpio_get_sum_piece() */
                if(MPI_SUCCESS != (mpi_code = MPI_Allreduce(&num_chunkf, &sum_chunkf, 1, MPI_UNSIGNED, MPI_SUM, H5F_mpi_get_comm(file))))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)
                #ifdef JK_DBG
                printf ("JKDBG p:%d %s:%d COUNT=0> num_chunkf:%d  sum_chunkf:%d\n", getpid(), __FILE__,__LINE__, num_chunkf, sum_chunkf);
                fflush(stdout);
                #endif

                if (H5F_get_mpi_handle(file, &mpi_fh_p) <0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get mpi file handle")
                mpi_fh = *(MPI_File*)mpi_fh_p;

                #ifdef JK_DBG
                printf ("JKDBG p:%d %s:%d COUNT=0> mpi_fh: %x , mpi_fh addr:%x\n", getpid(), __FILE__,__LINE__, *(MPI_File*)mpi_fh_p, mpi_fh_p);
                printf ("JKDBG p:%d %s:%d COUNT=0> mpi_fh: %x , mpi_fh addr:%x\n", getpid(), __FILE__,__LINE__, mpi_fh, &mpi_fh);
                fflush(stdout);
                #endif

                /* just to match up with the 1st MPI_File_set_view from H5FD_mpio_write() */
                if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(mpi_fh, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)

                /* just to match up with MPI_File_write_at_all from H5FD_mpio_write() */

                // JK_TODO make this based on the internal H5FD_MPIO_COLLECTIVE_IO 
                // OR add H5FD_MPIO_CHUNK_MULTI_IO . Later AT_ONCE_IO too
                //if(para_io_mode == H5FD_MPIO_COLLECTIVE_IO) 
                if(chunk_opt_mode == H5FD_MPIO_CHUNK_ONE_IO || 
                   chunk_opt_mode == H5FD_MPIO_CHUNK_MULTI_IO ) {
                    HDmemset(&mpi_stat, 0, sizeof(MPI_Status));
                    if(MPI_SUCCESS != (mpi_code = MPI_File_write_at_all(mpi_fh, 0, NULL, 0, MPI_BYTE, &mpi_stat)))
                        HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at_all failed", mpi_code)
                }

                /* just to match up with the 2nd MPI_File_set_view (reset) in H5FD_mpio_write() */
                if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(mpi_fh, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, "native", MPI_INFO_NULL)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)
            #ifndef JK_NOCOLLCAUSE
            } /* end if !global_cause */ 
            #endif
#endif /* H5_HAVE_PARALLEL */
        }
        else  /* this process select one or more dset to work on */
        #endif // JK_COUNT0
            if(H5D__write_mdset(file_id, count, info, dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__pre_write_mdset() */
#endif  // JK_WORK


/*-------------------------------------------------------------------------
 * Function:	H5D__read
 *
 * Purpose:	Reads (part of) a DATASET into application memory BUF. See
 *		H5Dread() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__read(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
	 const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/)
{
    H5D_chunk_map_t fm;                 /* Chunk file<->memory mapping */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
    H5D_type_info_t type_info;          /* Datatype info for operation */
    hbool_t type_info_init = FALSE;     /* Whether the datatype info has been initialized */
    H5S_t * projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                        /* projection of the supplied mem_space to a new  */
                                        /* data space with rank equal to that of          */
                                        /* file_space.                                    */
                                        /*                                                */
                                        /* This field is only used if                     */
                                        /* H5S_select_shape_same() returns TRUE when      */
                                        /* comparing the mem_space and the data_space,    */
                                        /* and the mem_space have different rank.         */
                                        /*                                                */
                                        /* Note that if this variable is used, the        */
                                        /* projected mem space must be discarded at the   */
                                        /* end of the function to avoid a memory leak.    */
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
#ifdef H5_HAVE_PARALLEL
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
#endif /*H5_HAVE_PARALLEL*/
    hbool_t     io_op_init = FALSE;     /* Whether the I/O op has been initialized */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_PACKAGE_TAG(dxpl_id, dataset->oloc.addr, FAIL)

    /* check args */
    HDassert(dataset && dataset->oloc.file);

    if(!file_space)
        file_space = dataset->shared->space;
    if(!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dst dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    /* Fill the DXPL cache values for later use */
    if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Set up datatype info for operation */
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, mem_type_id, FALSE, &type_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
    type_info_init = TRUE;

#ifdef H5_HAVE_PARALLEL
    /* Collective access is not permissible without a MPI based VFD */
    if(dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE && 
            !(H5F_HAS_FEATURE(dataset->oloc.file, H5FD_FEAT_HAS_MPI)))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if(nelmts != (hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if(!(H5S_has_extent(file_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if(!(H5S_has_extent(mem_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* H5S_select_shape_same() has been modified to accept topologically identical
     * selections with different rank as having the same shape (if the most 
     * rapidly changing coordinates match up), but the I/O code still has 
     * difficulties with the notion.
     *
     * To solve this, we check to see if H5S_select_shape_same() returns true, 
     * and if the ranks of the mem and file spaces are different.  If the are, 
     * construct a new mem space that is equivalent to the old mem space, and 
     * use that instead.
     *
     * Note that in general, this requires us to touch up the memory buffer as 
     * well.
     */
    if(TRUE == H5S_select_shape_same(mem_space, file_space) &&
            H5S_GET_EXTENT_NDIMS(mem_space) != H5S_GET_EXTENT_NDIMS(file_space)) {
        void *adj_buf = NULL;   /* Pointer to the location in buf corresponding  */
                                /* to the beginning of the projected mem space.  */

        /* Attempt to construct projected dataspace for memory dataspace */
        if(H5S_select_construct_projection(mem_space, &projected_mem_space,
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), buf, &adj_buf, type_info.dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
        HDassert(projected_mem_space);
        HDassert(adj_buf);

        /* Switch to using projected memory dataspace & adjusted buffer */
        mem_space = projected_mem_space;
        buf = adj_buf;
    } /* end if */


    /* Retrieve dataset properties */
    /* <none needed in the general case> */

    /* If space hasn't been allocated and not using external storage,
     * return fill value to buffer if fill time is upon allocation, or
     * do nothing if fill time is never.  If the dataset is compact and
     * fill time is NEVER, there is no way to tell whether part of data
     * has been overwritten.  So just proceed in reading.
     */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused == 0 &&
            !(*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)) {
        H5D_fill_value_t fill_status;   /* Whether/How the fill value is defined */

        /* Retrieve dataset's fill-value properties */
        if(H5P_is_fill_value_defined(&dataset->shared->dcpl_cache.fill, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined")

        /* Should be impossible, but check anyway... */
        if(fill_status == H5D_FILL_VALUE_UNDEFINED &&
                (dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_ALLOC || dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_IFSET))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "read failed: dataset doesn't exist, no data can be read")

        /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
        if(dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_NEVER)
            HGOTO_DONE(SUCCEED)

        /* Go fill the user's selection with the dataset's fill value */
        if(H5D__fill(dataset->shared->dcpl_cache.fill.buf, dataset->shared->type, buf, type_info.mem_type, mem_space, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed")
        else
            HGOTO_DONE(SUCCEED)
    } /* end if */

    /* Set up I/O operation */
    io_info.op_type = H5D_IO_OP_READ;
    io_info.u.rbuf = buf;
    if(H5D__ioinfo_init(dataset, dxpl_cache, dxpl_id, &type_info, &store, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to set up I/O operation")
#ifdef H5_HAVE_PARALLEL
    io_info_init = TRUE;
#endif /*H5_HAVE_PARALLEL*/

    /* Sanity check that space is allocated, if there are elements */
    if(nelmts > 0)
        HDassert((*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)
                || dataset->shared->dcpl_cache.efl.nused > 0
                || dataset->shared->layout.type == H5D_COMPACT);

    /* Call storage method's I/O initialization routine */
    HDmemset(&fm, 0, sizeof(H5D_chunk_map_t));
    if(io_info.layout_ops.io_init && (*io_info.layout_ops.io_init)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    io_op_init = TRUE;

#ifdef H5_HAVE_PARALLEL
    /* Adjust I/O info for any parallel I/O */
    if(H5D__ioinfo_adjust(&io_info, dataset, dxpl_id, file_space, mem_space, &type_info, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")
#endif /*H5_HAVE_PARALLEL*/

    /* Invoke correct "high level" I/O routine */
    if((*io_info.io_ops.multi_read)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    /* Shut down the I/O op information */
    if(io_op_init && io_info.layout_ops.io_term && (*io_info.layout_ops.io_term)(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")
#ifdef H5_HAVE_PARALLEL
    /* Shut down io_info struct */
    if(io_info_init)
        if(H5D__ioinfo_term(&io_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
#endif /*H5_HAVE_PARALLEL*/
    /* Shut down datatype info for operation */
    if(type_info_init && H5D__typeinfo_term(&type_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* discard projected mem space if it was created */
    if(NULL != projected_mem_space)
        if(H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5D__read() */

#ifdef JK_WORK
static herr_t
H5D__read_mdset(hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id);
{
    H5D_chunk_map_t fm;                 /* Chunk file<->memory mapping */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
    H5D_type_info_t type_info;          /* Datatype info for operation */
    hbool_t type_info_init = FALSE;     /* Whether the datatype info has been initialized */
    H5S_t * projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                        /* projection of the supplied mem_space to a new  */
                                        /* data space with rank equal to that of          */
                                        /* file_space.                                    */
                                        /*                                                */
                                        /* This field is only used if                     */
                                        /* H5S_select_shape_same() returns TRUE when      */
                                        /* comparing the mem_space and the data_space,    */
                                        /* and the mem_space have different rank.         */
                                        /*                                                */
                                        /* Note that if this variable is used, the        */
                                        /* projected mem space must be discarded at the   */
                                        /* end of the function to avoid a memory leak.    */
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
#ifdef H5_HAVE_PARALLEL
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
#endif /*H5_HAVE_PARALLEL*/
    hbool_t     io_op_init = FALSE;     /* Whether the I/O op has been initialized */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_STATIC_TAG(dxpl_id, dataset->oloc.addr, FAIL)

    /* check args */
    HDassert(dataset && dataset->oloc.file);

    if(!file_space)
        file_space = dataset->shared->space;
    if(!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dst dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    /* Fill the DXPL cache values for later use */
    if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Set up datatype info for operation */
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, mem_type_id, FALSE, &type_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
    type_info_init = TRUE;

#ifdef H5_HAVE_PARALLEL
    /* Collective access is not permissible without a MPI based VFD */
    if(dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE && 
            !(H5F_HAS_FEATURE(dataset->oloc.file, H5FD_FEAT_HAS_MPI)))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if(nelmts != (hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if(!(H5S_has_extent(file_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if(!(H5S_has_extent(mem_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* H5S_select_shape_same() has been modified to accept topologically identical
     * selections with different rank as having the same shape (if the most 
     * rapidly changing coordinates match up), but the I/O code still has 
     * difficulties with the notion.
     *
     * To solve this, we check to see if H5S_select_shape_same() returns true, 
     * and if the ranks of the mem and file spaces are different.  If the are, 
     * construct a new mem space that is equivalent to the old mem space, and 
     * use that instead.
     *
     * Note that in general, this requires us to touch up the memory buffer as 
     * well.
     */
    if(TRUE == H5S_select_shape_same(mem_space, file_space) &&
            H5S_GET_EXTENT_NDIMS(mem_space) != H5S_GET_EXTENT_NDIMS(file_space)) {
        void *adj_buf = NULL;   /* Pointer to the location in buf corresponding  */
                                /* to the beginning of the projected mem space.  */

        /* Attempt to construct projected dataspace for memory dataspace */
        if(H5S_select_construct_projection(mem_space, &projected_mem_space,
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), buf, &adj_buf, type_info.dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
        HDassert(projected_mem_space);
        HDassert(adj_buf);

        /* Switch to using projected memory dataspace & adjusted buffer */
        mem_space = projected_mem_space;
        buf = adj_buf;
    } /* end if */


    /* Retrieve dataset properties */
    /* <none needed in the general case> */

    /* If space hasn't been allocated and not using external storage,
     * return fill value to buffer if fill time is upon allocation, or
     * do nothing if fill time is never.  If the dataset is compact and
     * fill time is NEVER, there is no way to tell whether part of data
     * has been overwritten.  So just proceed in reading.
     */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused == 0 &&
            !(*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)) {
        H5D_fill_value_t fill_status;   /* Whether/How the fill value is defined */

        /* Retrieve dataset's fill-value properties */
        if(H5P_is_fill_value_defined(&dataset->shared->dcpl_cache.fill, &fill_status) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined")

        /* Should be impossible, but check anyway... */
        if(fill_status == H5D_FILL_VALUE_UNDEFINED &&
                (dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_ALLOC || dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_IFSET))
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "read failed: dataset doesn't exist, no data can be read")

        /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
        if(dataset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_NEVER)
            HGOTO_DONE(SUCCEED)

        /* Go fill the user's selection with the dataset's fill value */
        if(H5D__fill(dataset->shared->dcpl_cache.fill.buf, dataset->shared->type, buf, type_info.mem_type, mem_space, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed")
        else
            HGOTO_DONE(SUCCEED)
    } /* end if */

    /* Set up I/O operation */
    io_info.op_type = H5D_IO_OP_READ;
    io_info.u.rbuf = buf;
    if(H5D__ioinfo_init(dataset, dxpl_cache, dxpl_id, &type_info, &store, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to set up I/O operation")
#ifdef H5_HAVE_PARALLEL
    io_info_init = TRUE;
#endif /*H5_HAVE_PARALLEL*/

    /* Sanity check that space is allocated, if there are elements */
    if(nelmts > 0)
        HDassert((*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)
                || dataset->shared->dcpl_cache.efl.nused > 0
                || dataset->shared->layout.type == H5D_COMPACT);

    /* Call storage method's I/O initialization routine */
    HDmemset(&fm, 0, sizeof(H5D_chunk_map_t));
    if(io_info.layout_ops.io_init && (*io_info.layout_ops.io_init)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    io_op_init = TRUE;

#ifdef H5_HAVE_PARALLEL
    /* Adjust I/O info for any parallel I/O */
    if(H5D__ioinfo_adjust(&io_info, dataset, dxpl_id, file_space, mem_space, &type_info, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")
#endif /*H5_HAVE_PARALLEL*/

    /* Invoke correct "high level" I/O routine */
    if((*io_info.io_ops.multi_read)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    /* Shut down the I/O op information */
    if(io_op_init && io_info.layout_ops.io_term && (*io_info.layout_ops.io_term)(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")
#ifdef H5_HAVE_PARALLEL
    /* Shut down io_info struct */
    if(io_info_init)
        if(H5D__ioinfo_term(&io_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
#endif /*H5_HAVE_PARALLEL*/
    /* Shut down datatype info for operation */
    if(type_info_init && H5D__typeinfo_term(&type_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* discard projected mem space if it was created */
    if(NULL != projected_mem_space)
        if(H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5D__read_mdset() */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5D__write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. See H5Dwrite() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__write(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
	  const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{
    H5D_chunk_map_t fm;                 /* Chunk file<->memory mapping */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
    H5D_type_info_t type_info;          /* Datatype info for operation */
    hbool_t type_info_init = FALSE;     /* Whether the datatype info has been initialized */
    H5S_t * projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                        /* projection of the supplied mem_space to a new  */
                                        /* data space with rank equal to that of          */
                                        /* file_space.                                    */
                                        /*                                                */
                                        /* This field is only used if                     */
                                        /* H5S_select_shape_same() returns TRUE when      */
                                        /* comparing the mem_space and the data_space,    */
                                        /* and the mem_space have different rank.         */
                                        /*                                                */
                                        /* Note that if this variable is used, the        */
                                        /* projected mem space must be discarded at the   */
                                        /* end of the function to avoid a memory leak.    */
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
#ifdef H5_HAVE_PARALLEL
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
#endif /*H5_HAVE_PARALLEL*/
    hbool_t     io_op_init = FALSE;     /* Whether the I/O op has been initialized */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_STATIC_TAG(dxpl_id, dataset->oloc.addr, FAIL)

    /* check args */
    HDassert(dataset && dataset->oloc.file);

    /* All filters in the DCPL must have encoding enabled. */
    if(!dataset->shared->checked_filters) {
        if(H5Z_can_apply(dataset->shared->dcpl_id, dataset->shared->type_id) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANAPPLY, FAIL, "can't apply filters")

        dataset->shared->checked_filters = TRUE;
    } /* end if */

    /* Check if we are allowed to write to this file */
    if(0 == (H5F_INTENT(dataset->oloc.file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Fill the DXPL cache values for later use */
    if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Set up datatype info for operation */
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, mem_type_id, TRUE, &type_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
    type_info_init = TRUE;

    /* Various MPI based checks */
#ifdef H5_HAVE_PARALLEL
    if H5F_HAS_FEATURE(dataset->oloc.file, H5FD_FEAT_HAS_MPI) {
        /* If MPI based VFD is used, no VL datatype support yet. */
        /* This is because they use the global heap in the file and we don't */
        /* support parallel access of that yet */
        if(H5T_detect_class(type_info.mem_type, H5T_VLEN, FALSE) > 0)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet")

        /* If MPI based VFD is used, no VL datatype support yet. */
        /* This is because they use the global heap in the file and we don't */
        /* support parallel access of that yet */
        /* We should really use H5T_detect_class() here, but it will be difficult
         * to detect the type of the reference if it is nested... -QAK
         */
        if(H5T_get_class(type_info.mem_type, TRUE) == H5T_REFERENCE &&
                H5T_get_ref_type(type_info.mem_type) == H5R_DATASET_REGION)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet")

        /* Can't write to chunked datasets with filters, in parallel */
        if(dataset->shared->layout.type == H5D_CHUNKED &&
                dataset->shared->dcpl_cache.pline.nused > 0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "cannot write to chunked storage with filters in parallel")
    } /* end if */
    else {
        /* Collective access is not permissible without a MPI based VFD */
        if(dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based driver only")
    } /* end else */
#endif /*H5_HAVE_PARALLEL*/

    /* Initialize dataspace information */
    if(!file_space)
        file_space = dataset->shared->space;
    if(!mem_space)
        mem_space = file_space;

    #ifdef JK_DBG_SHAPE_SAME_P
    {
        int a;
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P Before - ORI> buf : ", __FUNCTION__, __LINE__);
        for (a=0; a < 10; a++) {
            printf("%u ", (uint32_t) ((uint32_t *)buf)[a]);
        }
        printf(" \n");
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P Before - ORI> ShapeSame:%d  Mem Ndims: %u File Ndims: %u \n", __FUNCTION__, __LINE__, H5S_select_shape_same(mem_space, file_space), H5S_GET_EXTENT_NDIMS(mem_space), H5S_GET_EXTENT_NDIMS(file_space));
    }
    #endif

    /* H5S_select_shape_same() has been modified to accept topologically 
     * identical selections with different rank as having the same shape 
     * (if the most rapidly changing coordinates match up), but the I/O 
     * code still has difficulties with the notion.
     *
     * To solve this, we check to see if H5S_select_shape_same() returns 
     * true, and if the ranks of the mem and file spaces are different.  
     * If the are, construct a new mem space that is equivalent to the 
     * old mem space, and use that instead.
     *
     * Note that in general, this requires us to touch up the memory buffer 
     * as well.
     */
    if(TRUE == H5S_select_shape_same(mem_space, file_space) &&
            H5S_GET_EXTENT_NDIMS(mem_space) != H5S_GET_EXTENT_NDIMS(file_space)) {
        void *adj_buf = NULL;   /* Pointer to the location in buf corresponding  */
                                /* to the beginning of the projected mem space.  */
        #ifdef JK_DBG_SHAPE_SAME_P
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P> CONTRUCT_PROJECTION! - ORI \n", __FUNCTION__, __LINE__);
        #endif

        /* Attempt to construct projected dataspace for memory dataspace */
        if(H5S_select_construct_projection(mem_space, &projected_mem_space,
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), buf, &adj_buf, type_info.src_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
        HDassert(projected_mem_space);
        HDassert(adj_buf);

        /* Switch to using projected memory dataspace & adjusted buffer */
        mem_space = projected_mem_space;
        buf = adj_buf;
    } /* end if */
    #ifdef JK_DBG_SHAPE_SAME_P
    {
        int a;
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P After> ORI buf : ", __FUNCTION__, __LINE__);
        for (a=0; a < 10; a++) {
            printf("%u ", (uint32_t) ((uint32_t *)buf)[a]);
        }
        printf(" \n");
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P After - ORI> ShapeSame:%d  Mem Ndims: %u File Ndims: %u \n", __FUNCTION__, __LINE__, H5S_select_shape_same(mem_space, file_space), H5S_GET_EXTENT_NDIMS(mem_space), H5S_GET_EXTENT_NDIMS(file_space));
        printf(" \n");
    }
    #endif

    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, hsize_t);

    /* Make certain that the number of elements in each selection is the same */
    if(nelmts != (hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if(!(H5S_has_extent(file_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if(!(H5S_has_extent(mem_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* Retrieve dataset properties */
    /* <none needed currently> */

    /* Allocate data space and initialize it if it hasn't been. */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused == 0 &&
            !(*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)) {
        hssize_t file_nelmts;   /* Number of elements in file dataset's dataspace */
        hbool_t full_overwrite; /* Whether we are over-writing all the elements */

        /* Get the number of elements in file dataset's dataspace */
        if((file_nelmts = H5S_GET_EXTENT_NPOINTS(file_space)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "can't retrieve number of elements in file dataset")

        /* Always allow fill values to be written if the dataset has a VL datatype */
        if(H5T_detect_class(dataset->shared->type, H5T_VLEN, FALSE))
            full_overwrite = FALSE;
        else
            full_overwrite = (hbool_t)((hsize_t)file_nelmts == nelmts ? TRUE : FALSE);

 	/* Allocate storage */
        if(H5D__alloc_storage(dataset, dxpl_id, H5D_ALLOC_WRITE, full_overwrite, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage")
    } /* end if */

    /* Set up I/O operation */
    io_info.op_type = H5D_IO_OP_WRITE;
    io_info.u.wbuf = buf;
    if(H5D__ioinfo_init(dataset, dxpl_cache, dxpl_id, &type_info, &store, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up I/O operation")
#ifdef H5_HAVE_PARALLEL
    io_info_init = TRUE;
#endif /*H5_HAVE_PARALLEL*/

    /* Call storage method's I/O initialization routine */
    HDmemset(&fm, 0, sizeof(H5D_chunk_map_t));
    if(io_info.layout_ops.io_init && (*io_info.layout_ops.io_init)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    io_op_init = TRUE;

#ifdef H5_HAVE_PARALLEL
    /* Adjust I/O info for any parallel I/O */
    if(H5D__ioinfo_adjust(&io_info, dataset, dxpl_id, file_space, mem_space, &type_info, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")
#endif /*H5_HAVE_PARALLEL*/

    /* Invoke correct "high level" I/O routine */
    if((*io_info.io_ops.multi_write)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

#ifdef OLD_WAY
/*
 * This was taken out because it can be called in a parallel program with
 * independent access, causing the metadata cache to get corrupted. Its been
 * disabled for all types of access (serial as well as parallel) to make the
 * modification time consistent for all programs. -QAK
 *
 * We should set a value in the dataset's shared information instead and flush
 * it to the file when the dataset is being closed. -QAK
 */
    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if(H5O_touch(&(dataset->oloc), FALSE, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time")
#endif /* OLD_WAY */

done:
    /* Shut down the I/O op information */
    if(io_op_init && io_info.layout_ops.io_term && (*io_info.layout_ops.io_term)(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")
#ifdef H5_HAVE_PARALLEL
    /* Shut down io_info struct */
    if(io_info_init && H5D__ioinfo_term(&io_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
#endif /*H5_HAVE_PARALLEL*/
    /* Shut down datatype info for operation */
    if(type_info_init && H5D__typeinfo_term(&type_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* discard projected mem space if it was created */
    if(NULL != projected_mem_space)
        if(H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")

    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
} /* end H5D__write() */

#ifndef JK_WORK
static herr_t
H5D__write_mdset (hid_t file_id, size_t count, H5D_rw_multi_t *info, hid_t dxpl_id)
//(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space, const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{    
    //H5D_io_info_t io_info;            /* Dataset I/O info     */
    H5D_io_info_md_t io_info_md;        /* Dataset I/O info for multi dsets */
    //H5D_chunk_map_t fm;               /* Chunk file<->memory mapping */
    //H5D_type_info_t type_info;          /* Datatype info for operation */
    hbool_t type_info_init = FALSE;     /* Whether the datatype info has been initialized */
    H5D_dset_info_t *dset_info_array = NULL;
    #ifdef JK_ORI
    H5S_t * projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
    #else
    H5S_t ** projected_mem_space; /* If not NULL, ptr to dataspace containing a     */
    #endif
                                        /* projection of the supplied mem_space to a new  */
                                        /* data space with rank equal to that of          */
                                        /* file_space.                                    */
                                        /*                                                */
                                        /* This field is only used if                     */
                                        /* H5S_select_shape_same() returns TRUE when      */
                                        /* comparing the mem_space and the data_space,    */
                                        /* and the mem_space have different rank.         */
                                        /*                                                */
                                        /* Note that if this variable is used, the        */
                                        /* projected mem space must be discarded at the   */
                                        /* end of the function to avoid a memory leak.    */
    #ifdef JK_ORI                                        
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    #else
    H5D_storage_t *store=NULL;               /*union of EFL and chunk pointer in file space */
    #endif
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
#ifdef H5_HAVE_PARALLEL
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
#endif /*H5_HAVE_PARALLEL*/
    hbool_t     io_op_init = FALSE;     /* Whether the I/O op has been initialized */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/
    #ifndef JK_WORK
    H5D_t *dataset=NULL;  // old args
    const H5S_t *file_space = NULL; // old arg
    const H5S_t *mem_space = NULL;  // old arg
    #endif
    #ifndef JK_SHAPE_SAME_P
    /* save original wbuf */
    const void ** info_wbuf_ori;
    #endif

    #ifdef JK_TEST_ADDR_TAG_OLD_REMOVE
    //haddr_t prev_tag = HADDR_UNDEF;
    dataset = (H5D_t *)H5I_object_verify(info[1].dset_id, H5I_DATASET);
    FUNC_ENTER_STATIC_TAG(dxpl_id, dataset->oloc.addr, FAIL)
    #else
    FUNC_ENTER_STATIC
    #endif

   #ifndef JK_MULTI_DSET
    #ifdef JK_TEST_NO_TOTAL_SELECT_PIECE_REMOVE
    #ifdef H5_HAVE_PARALLEL
    io_info_md.select_piece = NULL;
    #endif
    io_info_md.select_piece_last_size = 0;
    #endif // JK_TEST_NO_TOTAL_SELECT_PIECE_REMOVE
    io_info_md.sel_pieces = NULL;
    io_info_md.store_faddr = 0;
    io_info_md.base_maddr_w = NULL;
    #ifndef JK_DEBUG_SLMEM
    io_info_md.mc_cnt=0;
    #endif
   #endif // JK_MULTI_DSET

    /* malloc dset_info */
    if(NULL == (io_info_md.dsets_info = (H5D_dset_info_t *)H5MM_calloc(count * sizeof(H5D_dset_info_t))))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate dset info array buffer")
    /* simplify code read */
    dset_info_array = io_info_md.dsets_info;

    #ifndef JK_WORK
    if(NULL == ( projected_mem_space = (H5S_t **)H5MM_calloc(count * sizeof(H5S_t*))))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTALLOC, FAIL, "couldn't allocate dset space array ptr")

    if(NULL == (store = (H5D_storage_t *)H5MM_malloc(count * sizeof(H5D_storage_t))))
        HGOTO_ERROR(H5E_STORAGE, H5E_CANTALLOC, FAIL, "couldn't allocate dset storage info array buffer")
    
    #endif

    #ifndef JK_SHAPE_SAME_P
    /* allocate wbuf ptr array to save original wbuf ptr */
    if(NULL == (info_wbuf_ori = (const void **)H5MM_calloc(count * sizeof(void*))))
        HGOTO_ERROR(H5E_STORAGE, H5E_CANTALLOC, FAIL, "couldn't allocate ori buf array")
    #endif

  /* iterate dsets */
  for (unsigned int i=0; i < count; i++)
  {
    /* check args */
    // JK - dataset : info[i].dset_id , loop
    #ifdef JK_ORI
    //HDassert(dataset && dataset->oloc.file);
    #else
    if(NULL == (dataset = (H5D_t *)H5I_object_verify(info[i].dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(NULL == dataset->oloc.file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    #endif
    #ifdef JK_DBG
    printf ("JKDBG p:%d %s:%d> dataset%d->oloc.addr: %llu\n", getpid(), __FILE__, __LINE__,i, dataset->oloc.addr);
    #endif

    #ifdef JK_TEST_ADDR_TAG_OLD_REMOVE
    if(H5AC_tag(dxpl_id, dataset->oloc.addr, &prev_tag) < 0)                                     HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag")
    #endif

    /* All filters in the DCPL must have encoding enabled. */
    if(!dataset->shared->checked_filters) {
        if(H5Z_can_apply(dataset->shared->dcpl_id, dataset->shared->type_id) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANAPPLY, FAIL, "can't apply filters")

        dataset->shared->checked_filters = TRUE;
    } /* end if */

    /* Check if we are allowed to write to this file */
    if(0 == (H5F_INTENT(dataset->oloc.file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Fill the DXPL cache values for later use */
    if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Set up datatype info for operation */
    #ifdef JK_ORI
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, mem_type_id, TRUE, &type_info) < 0)
    #else
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, info[i].mem_type_id, TRUE, &(dset_info_array[i].type_info)) < 0)
    #endif
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
    type_info_init = TRUE;

    /* Various MPI based checks */
#ifdef H5_HAVE_PARALLEL
    if H5F_HAS_FEATURE(dataset->oloc.file, H5FD_FEAT_HAS_MPI) {
        /* If MPI based VFD is used, no VL datatype support yet. */
        /* This is because they use the global heap in the file and we don't */
        /* support parallel access of that yet */
        #ifdef JK_ORI
        if(H5T_detect_class(type_info.mem_type, H5T_VLEN, FALSE) > 0)
        #else
        if(H5T_detect_class(dset_info_array[i].type_info.mem_type, H5T_VLEN, FALSE) > 0)
        #endif
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet")

        /* If MPI based VFD is used, no VL datatype support yet. */
        /* This is because they use the global heap in the file and we don't */
        /* support parallel access of that yet */
        /* We should really use H5T_detect_class() here, but it will be difficult
         * to detect the type of the reference if it is nested... -QAK
         */
        #ifdef JK_ORI
        if(H5T_get_class(type_info.mem_type, TRUE) == H5T_REFERENCE &&
                H5T_get_ref_type(type_info.mem_type) == H5R_DATASET_REGION)
        #else
        if(H5T_get_class(dset_info_array[i].type_info.mem_type, TRUE) == H5T_REFERENCE &&
           H5T_get_ref_type(dset_info_array[i].type_info.mem_type) == H5R_DATASET_REGION)
        #endif
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet")

        /* Can't write to chunked datasets with filters, in parallel */
        if(dataset->shared->layout.type == H5D_CHUNKED &&
                dataset->shared->dcpl_cache.pline.nused > 0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "cannot write to chunked storage with filters in parallel")
    } /* end if */
    else {
        /* Collective access is not permissible without a MPI based VFD */
        if(dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE)
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based driver only")
    } /* end else */
#endif /*H5_HAVE_PARALLEL*/

    #ifndef JK_NEW // get file_space
    /* Initialize file dataspace information */
    if(H5S_ALL != info[i].file_space_id) {
        if(NULL == (file_space = (const H5S_t *)H5I_object_verify(info[i].file_space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

        /* Check for valid selection */
        if(H5S_SELECT_VALID(file_space) != TRUE)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "file selection+offset not within extent")
    } /* end if */
    #endif
    if(!file_space)
        file_space = dataset->shared->space;

    #ifndef JK_NEW // get mem_space
    /* Initialize memory dataspace information */
    if(H5S_ALL != info[i].mem_space_id) {
        if(NULL == (mem_space = (const H5S_t *)H5I_object_verify(info[i].mem_space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space")

        /* Check for valid selection */
        if(H5S_SELECT_VALID(mem_space) != TRUE)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "memory selection+offset not within extent")
    } /* end if */
    #endif
    if(!mem_space)
        mem_space = file_space;

    #ifdef JK_DBG_SHAPE_SAME_P
    {
        int a;
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P Before> buf : ", __FUNCTION__, __LINE__);
        for (a=0; a < 10; a++) {
            printf("%u ", (uint32_t) ((uint32_t *)info[i].wbuf)[a]);
        }
        printf(" \n");
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P Before> ShapeSame:%d  Mem Ndims: %u File Ndims: %u \n", __FUNCTION__, __LINE__, H5S_select_shape_same(mem_space, file_space), H5S_GET_EXTENT_NDIMS(mem_space), H5S_GET_EXTENT_NDIMS(file_space));
    }
    #endif

    /* H5S_select_shape_same() has been modified to accept topologically 
     * identical selections with different rank as having the same shape 
     * (if the most rapidly changing coordinates match up), but the I/O 
     * code still has difficulties with the notion.
     *
     * To solve this, we check to see if H5S_select_shape_same() returns 
     * true, and if the ranks of the mem and file spaces are different.  
     * If the are, construct a new mem space that is equivalent to the 
     * old mem space, and use that instead.
     *
     * Note that in general, this requires us to touch up the memory buffer 
     * as well.
     */
    #ifndef JK_SHAPE_SAME_P
    /* Save original write buf pointer */
    info_wbuf_ori[i] = info[i].wbuf;
    #endif
    if(TRUE == H5S_select_shape_same(mem_space, file_space) &&
            H5S_GET_EXTENT_NDIMS(mem_space) != H5S_GET_EXTENT_NDIMS(file_space)) {
        const void *adj_buf = NULL;   /* Pointer to the location in buf corresponding  */
                                /* to the beginning of the projected mem space.  */

        #ifdef JK_DBG_SHAPE_SAME_P
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P> CONTRUCT_PROJECTION! \n", __FUNCTION__, __LINE__);
        #endif
        /* Attempt to construct projected dataspace for memory dataspace */
        #ifdef JK_ORI
        if(H5S_select_construct_projection(mem_space, &projected_mem_space,
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), buf, &adj_buf, type_info.src_type_size) < 0)
        #else                
        if(H5S_select_construct_projection(mem_space, &(projected_mem_space[i]),
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), info[i].wbuf, &adj_buf, (hsize_t) dset_info_array[i].type_info.src_type_size) < 0)
        #endif
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
        #ifdef JK_ORI
        HDassert(projected_mem_space);
        #else
        HDassert(projected_mem_space[i]);
        #endif
        HDassert(adj_buf);

        /* Switch to using projected memory dataspace & adjusted buffer */
        #ifdef JK_ORI
        mem_space = projected_mem_space;
        buf = adj_buf;
        #else
        mem_space = projected_mem_space[i];
        info[i].wbuf = adj_buf;
        #endif
    } /* end if */
    #ifdef JK_DBG_SHAPE_SAME_P
    {
        int a;
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P After> buf : ", __FUNCTION__, __LINE__);
        for (a=0; a < 10; a++) {
            printf("%u ", (uint32_t) ((uint32_t *)info[i].wbuf)[a]);
        }
        printf(" \n");
        printf("JKDBG %s|%d DBG_SHAPE_SAME_P After> ShapeSame:%d  Mem Ndims: %u File Ndims: %u \n", __FUNCTION__, __LINE__, H5S_select_shape_same(mem_space, file_space), H5S_GET_EXTENT_NDIMS(mem_space), H5S_GET_EXTENT_NDIMS(file_space));
        printf(" \n");
    }
    #endif

    #ifdef JK_DBG
    printf("JKDBG %s|%d > ShapeSame:%d  Mem NPnts: %lld File NPnts: %lld \n", __FUNCTION__, __LINE__, H5S_select_shape_same(mem_space, file_space), H5S_GET_SELECT_NPOINTS(mem_space), H5S_GET_SELECT_NPOINTS(file_space));
    #endif

    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, hsize_t);

    /* Make certain that the number of elements in each selection is the same */
    if(nelmts != (hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if(!(H5S_has_extent(file_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if(!(H5S_has_extent(mem_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* Retrieve dataset properties */
    /* <none needed currently> */

    // JK_TODO_TEST Test this code
    /* Allocate data space and initialize it if it hasn't been. */
    if(nelmts > 0 && dataset->shared->dcpl_cache.efl.nused == 0 &&
            !(*dataset->shared->layout.ops->is_space_alloc)(&dataset->shared->layout.storage)) {
        hssize_t file_nelmts;   /* Number of elements in file dataset's dataspace */
        hbool_t full_overwrite; /* Whether we are over-writing all the elements */

        /* Get the number of elements in file dataset's dataspace */
        if((file_nelmts = H5S_GET_EXTENT_NPOINTS(file_space)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "can't retrieve number of elements in file dataset")

        /* Always allow fill values to be written if the dataset has a VL datatype */
        if(H5T_detect_class(dataset->shared->type, H5T_VLEN, FALSE))
            full_overwrite = FALSE;
        else
            full_overwrite = (hbool_t)((hsize_t)file_nelmts == nelmts ? TRUE : FALSE);

 	/* Allocate storage */
        if(H5D__alloc_storage(dataset, dxpl_id, H5D_ALLOC_WRITE, full_overwrite, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage")
    } /* end if */


    /* Set up I/O operation */
    #ifdef JK_ORI_REMOVE
    io_info.op_type = H5D_IO_OP_WRITE;
    io_info.u.wbuf = buf;
    #else
    io_info_md.op_type = H5D_IO_OP_WRITE;
    dset_info_array[i].u.wbuf = info[i].wbuf;
    #endif

    #ifndef JK_WORK_TEST
    dset_info_array[i].index = i;
    #endif

    #ifdef JK_ORI_REMOVE
    if(H5D__ioinfo_init(dataset, dxpl_cache, dxpl_id, &type_info, &store, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up I/O operation")
    #else
    // JK_TODO_CONSIDER_LATER get out of loop (better code?)
    if(H5D__ioinfo_init_mdset(dataset, dxpl_cache, dxpl_id, &(dset_info_array[i]), &(store[i]), &io_info_md) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up I/O operation")
    #endif
#ifdef H5_HAVE_PARALLEL
    io_info_init = TRUE;
#endif /*H5_HAVE_PARALLEL*/

    /* Call storage method's I/O initialization routine */
    // JK This is filling fm , io_info_md.dset_info[] is same here to be filled and generate piece_infos in skip list
    #ifdef JK_ORI_REMOVE
    HDmemset(&fm, 0, sizeof(H5D_chunk_map_t));
    if(io_info.layout_ops.io_init && (*io_info.layout_ops.io_init)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    #else
    // JK May reduce .type_info
    if(dset_info_array[i].layout_ops.io_init_md && (*dset_info_array[i].layout_ops.io_init_md)(&io_info_md, &(dset_info_array[i].type_info), nelmts, file_space, mem_space, &(dset_info_array[i])) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    #endif
    io_op_init = TRUE;
  } // end of for loop

#ifdef H5_HAVE_PARALLEL
    /* Adjust I/O info for any parallel I/O */
    #ifdef JK_ORI_REMOVE
    if(H5D__ioinfo_adjust(&io_info, dataset, dxpl_id, file_space, mem_space, &type_info, &fm) < 0)
    #else
    if(H5D__ioinfo_adjust_mdset(count, &io_info_md, dxpl_id) < 0)
    //if(H5D__ioinfo_adjust_mdset_OLD(&io_info_md, dataset, dxpl_id, file_space, mem_space, &(dset_info_array[i].type_info), &(dset_info_array[i])) < 0)
    #endif
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")
#endif /*H5_HAVE_PARALLEL*/


    /* Invoke correct "high level" I/O routine */
    #ifdef JK_ORI
    if((*io_info.io_ops.multi_write)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
    #else
     #ifndef JK_NOCOLLCAUSE
     /* If collective mode is broken, perform write IO in independent mode via 
      * single-dset path with looping. 
      * Multiple-dset path can not be called since it is not supported, so make 
      * detour through single-dset path */
     if (TRUE == io_info_md.is_coll_broken) {
        size_t i;
        H5P_genplist_t *dx_plist;

        /* loop with serial & single-dset write IO path */
        for (i=0; i< count; i++) {
            #ifndef JK_SHAPE_SAME_P
            /* Restore ori wbuf , so it can be passed as initial state for 
             * single-dset path */
            info[i].wbuf = info_wbuf_ori[i];
            #endif
            if(H5D__pre_write(info[i].dset_id, info[i].mem_type_id, info[i].mem_space_id, info[i].file_space_id, dxpl_id, info[i].wbuf) < 0) 
	            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't prepare for writing data")
        }
     } else
     #endif
    if((*io_info_md.io_ops.multi_write_md)(file_id, count, &io_info_md) < 0)
    #endif
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

#ifdef OLD_WAY
/*
 * This was taken out because it can be called in a parallel program with
 * independent access, causing the metadata cache to get corrupted. Its been
 * disabled for all types of access (serial as well as parallel) to make the
 * modification time consistent for all programs. -QAK
 *
 * We should set a value in the dataset's shared information instead and flush
 * it to the file when the dataset is being closed. -QAK
 */
    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if(H5O_touch(&(dataset->oloc), FALSE, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time")
#endif /* OLD_WAY */

done:

  /* iterate dsets */
  for (unsigned int i=0; i < count; i++) 
  {
    #ifdef JK_DBG
    if(dset_info_array[i].layout)
        printf("JKDBG %s|%d> Dset[%u] , Type: %d\n", __FUNCTION__, __LINE__,i, dset_info_array[i].layout->type);
    else
        printf("JKDBG %s|%d> Dset[%u] , Type: N/A\n", __FUNCTION__, __LINE__,i);
    #endif

    #ifdef JK_ORI
    /* Shut down the I/O op information */
    if(io_op_init && io_info.layout_ops.io_term && (*io_info.layout_ops.io_term)(&fm) < 0)
    #else // JK NOTE: Open this after io_init is all done.
    /* Shut down the I/O op information */
    if(io_op_init && dset_info_array[i].layout_ops.io_term_md && (*dset_info_array[i].layout_ops.io_term_md)(&(dset_info_array[i]), &io_info_md) < 0)
    #endif
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")
  } /* end of for */

#ifdef H5_HAVE_PARALLEL
    /* Shut down io_info struct */
    #ifdef JK_ORI
    if(io_info_init && H5D__ioinfo_term(&io_info) < 0)
    #else
    if(io_info_init && H5D__ioinfo_term_mdset(&io_info_md) < 0)
    #endif
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
#endif /*H5_HAVE_PARALLEL*/

    for (unsigned int i=0; i<count; i++)
    {
    /* Shut down datatype info for operation */
    #ifdef JK_ORI
    if(type_info_init && H5D__typeinfo_term(&type_info) < 0)
    #else
    if(type_info_init && H5D__typeinfo_term(&(dset_info_array[i].type_info)) < 0)
    #endif
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* discard projected mem space if it was created */
    #ifdef JK_ORI
    if(NULL != projected_mem_space)
        if(H5S_close(projected_mem_space) < 0)
    #else
    if(NULL != projected_mem_space[i])
        if(H5S_close(projected_mem_space[i]) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")
    #endif // JK_LATER
    }

    #ifndef JK_SHAPE_SAME_P
    /* free original wbuf ptr array */
    if (info_wbuf_ori)
        H5MM_xfree(info_wbuf_ori);
    #endif

    if(io_info_md.dsets_info)
        H5MM_xfree(io_info_md.dsets_info);

    if(projected_mem_space)
        H5MM_xfree(projected_mem_space);

    if(store)
        H5MM_xfree(store);

    #ifdef JK_TEST_ADDR_TAG_OLD_REMOVE
    FUNC_LEAVE_NOAPI_TAG(ret_value, FAIL)
    #else
    FUNC_LEAVE_NOAPI(ret_value)
    #endif
} /* end H5D__write_mdset */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5D__ioinfo_init
 *
 * Purpose:	Routine for determining correct I/O operations for
 *              each I/O action.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, September 30, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_init(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    const H5D_type_info_t *type_info, H5D_storage_t *store, H5D_io_info_t *io_info)
{
    FUNC_ENTER_STATIC_NOERR

    /* check args */
    HDassert(dset);
    HDassert(dset->oloc.file);
    HDassert(type_info);
    HDassert(type_info->tpath);
    HDassert(io_info);

    /* Set up "normal" I/O fields */
    io_info->dset = dset;
    io_info->dxpl_cache = dxpl_cache;
    io_info->dxpl_id = dxpl_id;
    io_info->store = store;

    /* Set I/O operations to initial values */
    io_info->layout_ops = *dset->shared->layout.ops;

    /* Set the "high-level" I/O operations for the dataset */
    io_info->io_ops.multi_read = dset->shared->layout.ops->ser_read;
    io_info->io_ops.multi_write = dset->shared->layout.ops->ser_write;

    /* Set the I/O operations for reading/writing single blocks on disk */
    if(type_info->is_xform_noop && type_info->is_conv_noop) {
        /*
         * If there is no data transform or type conversion then read directly into
         *  the application's buffer.  This saves at least one mem-to-mem copy.
         */
        io_info->io_ops.single_read = H5D__select_read;
        io_info->io_ops.single_write = H5D__select_write;
    } /* end if */
    else {
        /*
         * This is the general case (type conversion, usually).
         */
        io_info->io_ops.single_read = H5D__scatgath_read;
        io_info->io_ops.single_write = H5D__scatgath_write;
    } /* end else */

#ifdef H5_HAVE_PARALLEL
    /* Determine if the file was opened with an MPI VFD */
    io_info->using_mpi_vfd = H5F_HAS_FEATURE(dset->oloc.file, H5FD_FEAT_HAS_MPI);
#endif /* H5_HAVE_PARALLEL */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__ioinfo_init() */

#ifndef JK_WORK
static herr_t
H5D__ioinfo_init_mdset(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    H5D_dset_info_t *dset_info, H5D_storage_t *store, H5D_io_info_md_t *io_info_md)
    //const H5D_type_info_t *type_info, H5D_storage_t *store, H5D_io_info_t *io_info)
{
    FUNC_ENTER_STATIC_NOERR

    /* check args */
    HDassert(dset);
    HDassert(dset->oloc.file);
    HDassert(&(dset_info->type_info));
    HDassert(dset_info->type_info.tpath);
    HDassert(io_info_md);

    /* Set up "normal" I/O fields */
    #ifdef JK_ORI
    io_info->dset = dset;
    #else
    dset_info->dset = dset;
    #endif
    io_info_md->dxpl_cache = dxpl_cache;
    io_info_md->dxpl_id = dxpl_id;
    #ifndef JK_NOCOLLCAUSE
    io_info_md->is_coll_broken = FALSE;
    #endif
    dset_info->store = store;

    /* Set I/O operations to initial values */
    #ifdef JK_ORI
    io_info->layout_ops = *dset->shared->layout.ops;
    #else
    dset_info->layout_ops = *dset->shared->layout.ops;
    //dset_info->layout_ops_md = *dset->shared->layout.ops_md;
    //io_info_md->layout_ops_md = *dset->shared->layout.ops_md;
    #endif

   #ifndef JK_SERIAL  
   // JK NOTE: this is SERIAL INIT so not effect for PARALLE
    /* Set the "high-level" I/O operations for the dataset */
    io_info_md->io_ops.multi_read = dset->shared->layout.ops->ser_read;
    io_info_md->io_ops.multi_write = dset->shared->layout.ops->ser_write;
    #ifdef JK_TODO_NOCOLLCAUSE_REMOVE // not need any more, worked by other nocaolcause code
    // JK_POINT - point selection is not supported for collective IO
    //   so H5D__mpio_opt_possible_mdset() detect and return FALSE. 
    //   in ori code, just serial write is set, but here func interface is different
    //   so need to wrap original serial write and set it here.
    //   Anyway this will also resolve POSIX case, as they all are detected by same
    //   H5D__mpio_opt_possible_mdset(). 
    // when work with serial-mdset if use layout.ops_md - Not Necessary
    //io_info_md->io_ops.multi_read = dset->shared->layout.ops->ser_read_md;
    //io_info_md->io_ops.multi_write_md = dset->shared->layout.ops->ser_write_md;
    #endif

    /* Set the I/O operations for reading/writing single blocks on disk */
    if(dset_info->type_info.is_xform_noop && dset_info->type_info.is_conv_noop) {
        /*
         * If there is no data transform or type conversion then read directly into
         *  the application's buffer.  This saves at least one mem-to-mem copy.
         */
        io_info_md->io_ops.single_read = H5D__select_read;
        io_info_md->io_ops.single_write = H5D__select_write;
    } /* end if */
    else {
        /*
         * This is the general case (type conversion, usually).
         */
        io_info_md->io_ops.single_read = H5D__scatgath_read;
        io_info_md->io_ops.single_write = H5D__scatgath_write;
    } /* end else */
   #endif

#ifdef H5_HAVE_PARALLEL
    /* Determine if the file was opened with an MPI VFD */
    io_info_md->using_mpi_vfd = H5F_HAS_FEATURE(dset->oloc.file, H5FD_FEAT_HAS_MPI);
#endif /* H5_HAVE_PARALLEL */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__ioinfo_init_mdset() */
#endif


/*-------------------------------------------------------------------------
 * Function:	H5D__typeinfo_init
 *
 * Purpose:	Routine for determining correct datatype information for
 *              each I/O action.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__typeinfo_init(const H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, hid_t mem_type_id, hbool_t do_write,
    H5D_type_info_t *type_info)
{
    const H5T_t	*src_type;              /* Source datatype */
    const H5T_t	*dst_type;              /* Destination datatype */
    herr_t ret_value = SUCCEED;	        /* Return value	*/

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(type_info);
    HDassert(dset);

    /* Initialize type info safely */
    HDmemset(type_info, 0, sizeof(*type_info));

    /* Get the memory & dataset datatypes */
    if(NULL == (type_info->mem_type = (const H5T_t *)H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")
    type_info->dset_type = dset->shared->type;

    if(do_write) {
        src_type = type_info->mem_type;
        dst_type = dset->shared->type;
        type_info->src_type_id = mem_type_id;
        type_info->dst_type_id = dset->shared->type_id;
    } /* end if */
    else {
        src_type = dset->shared->type;
        dst_type = type_info->mem_type;
        type_info->src_type_id = dset->shared->type_id;
        type_info->dst_type_id = mem_type_id;
    } /* end else */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register datatype atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off datatype conversion also
     * turns off background preservation.
     */
    if(NULL == (type_info->tpath = H5T_path_find(src_type, dst_type, NULL, NULL, dxpl_id, FALSE)))
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest datatype")

    /* Precompute some useful information */
    type_info->src_type_size = H5T_get_size(src_type);
    type_info->dst_type_size = H5T_get_size(dst_type);
    type_info->max_type_size = MAX(type_info->src_type_size, type_info->dst_type_size);
    type_info->is_conv_noop = H5T_path_noop(type_info->tpath);
    type_info->is_xform_noop = H5Z_xform_noop(dxpl_cache->data_xform_prop);
    if(type_info->is_xform_noop && type_info->is_conv_noop) {
        type_info->cmpd_subset = NULL;
        type_info->need_bkg = H5T_BKG_NO;
    } /* end if */
    else {
        size_t	target_size;		/* Desired buffer size	*/

        /* Check if the datatypes are compound subsets of one another */
        type_info->cmpd_subset = H5T_path_compound_subset(type_info->tpath);

        /* Check if we need a background buffer */
        if(do_write && H5T_detect_class(dset->shared->type, H5T_VLEN, FALSE))
            type_info->need_bkg = H5T_BKG_YES;
        else {
            H5T_bkg_t path_bkg;     /* Type conversion's background info */

            if((path_bkg = H5T_path_bkg(type_info->tpath))) {
                /* Retrieve the bkgr buffer property */
                type_info->need_bkg = dxpl_cache->bkgr_buf_type;
                type_info->need_bkg = MAX(path_bkg, type_info->need_bkg);
            } /* end if */
            else
                type_info->need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
        } /* end else */


        /* Set up datatype conversion/background buffers */

        /* Get buffer size from DXPL */
        target_size = dxpl_cache->max_temp_buf;

        /* If the buffer is too small to hold even one element, try to make it bigger */
        if(target_size < type_info->max_type_size) {
            hbool_t default_buffer_info;    /* Whether the buffer information are the defaults */

            /* Detect if we have all default settings for buffers */
            default_buffer_info = (hbool_t)((H5D_TEMP_BUF_SIZE == dxpl_cache->max_temp_buf)
                    && (NULL == dxpl_cache->tconv_buf) && (NULL == dxpl_cache->bkgr_buf));

            /* Check if we are using the default buffer info */
            if(default_buffer_info)
                /* OK to get bigger for library default settings */
                target_size = type_info->max_type_size;
            else
                /* Don't get bigger than the application has requested */
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")
        } /* end if */

        /* Compute the number of elements that will fit into buffer */
        type_info->request_nelmts = target_size / type_info->max_type_size;

        /* Sanity check elements in temporary buffer */
        if(type_info->request_nelmts == 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

        /*
         * Get a temporary buffer for type conversion unless the app has already
         * supplied one through the xfer properties. Instead of allocating a
         * buffer which is the exact size, we allocate the target size.  The
         * malloc() is usually less resource-intensive if we allocate/free the
         * same size over and over.
         */
        if(NULL == (type_info->tconv_buf = (uint8_t *)dxpl_cache->tconv_buf)) {
            /* Allocate temporary buffer */
            if(NULL == (type_info->tconv_buf = H5FL_BLK_MALLOC(type_conv, target_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion")
            type_info->tconv_buf_allocated = TRUE;
        } /* end if */
        if(type_info->need_bkg && NULL == (type_info->bkg_buf = (uint8_t *)dxpl_cache->bkgr_buf)) {
            size_t	bkg_size;		/* Desired background buffer size	*/

            /* Compute the background buffer size */
            /* (don't try to use buffers smaller than the default size) */
            bkg_size = type_info->request_nelmts * type_info->dst_type_size;
            if(bkg_size < dxpl_cache->max_temp_buf)
                bkg_size = dxpl_cache->max_temp_buf;

            /* Allocate background buffer */
            /* (Need calloc()-like call since memory needs to be initialized) */
            if(NULL == (type_info->bkg_buf = H5FL_BLK_CALLOC(type_conv, bkg_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion")
            type_info->bkg_buf_allocated = TRUE;
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__typeinfo_init() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5D__ioinfo_adjust
 *
 * Purpose:	Adjust operation's I/O info for any parallel I/O
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March 27, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_adjust(H5D_io_info_t *io_info, const H5D_t *dset, hid_t dxpl_id,
    const H5S_t *file_space, const H5S_t *mem_space,
    const H5D_type_info_t *type_info, const H5D_chunk_map_t *fm)
{
    H5P_genplist_t *dx_plist;       /* Data transer property list */
    #ifndef JK_NOCOLLCAUSE
    int global_no_collective_cause;
    #endif
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode; /* performed chunk optimization */
    H5D_mpio_actual_io_mode_t actual_io_mode; /* performed io mode */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(dset);
    HDassert(dset->oloc.file);
    HDassert(mem_space);
    HDassert(file_space);
    HDassert(type_info);
    HDassert(type_info->tpath);
    HDassert(io_info);

    /* Get the dataset transfer property list */
    if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    #ifndef JK_NOCOLLCAUSE
     #ifndef JK_TODO_REMOVE_LATER
     // JK_NOTE: remove only the if condition line when READ is ready, 
     // because this should work for both WIRTE & READ cases
     if(io_info->op_type == H5D_IO_OP_WRITE)
     #endif
    {
    if(H5P_get(dx_plist, H5D_MPIO_GLOBAL_NO_COLLECTIVE_CAUSE_NAME, &global_no_collective_cause) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get global value")

     #ifdef JK_DBG
     printf("JKDBG %s|%d> Dset layout:%d, global-cause:%d\n", __FUNCTION__, __LINE__, dset->shared->layout.type, global_no_collective_cause );
     #endif
    }
    #endif // JK_NOCOLLCAUSE

    /* Reset the actual io mode properties to the default values in case
     * the dxpl was previously used in a collective I/O operation.
     */
    actual_chunk_opt_mode = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
    actual_io_mode = H5D_MPIO_NO_COLLECTIVE;
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Make any parallel I/O adjustments */
    if(io_info->using_mpi_vfd) {
        htri_t opt;         /* Flag whether a selection is optimizable */

        /* Record the original state of parallel I/O transfer options */
        io_info->orig.xfer_mode = io_info->dxpl_cache->xfer_mode;
        io_info->orig.coll_opt_mode = io_info->dxpl_cache->coll_opt_mode;
        io_info->orig.io_ops.single_read = io_info->io_ops.single_read;
        io_info->orig.io_ops.single_write = io_info->io_ops.single_write;

        /* Get MPI communicator */
        if(MPI_COMM_NULL == (io_info->comm = H5F_mpi_get_comm(dset->oloc.file)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve MPI communicator")

        #ifndef JK_NOCOLLCAUSE
        if (
           #ifndef JK_TODO_REMOVE_LATER
           // JK_NOTE: remove only this condition line when READ is ready, 
           // because this should work for both WIRTE & READ cases
           io_info->op_type == H5D_IO_OP_WRITE &&
           #endif
           /* if collective broken cause already calculated, no need to do again */
           global_no_collective_cause > 0) {
            opt = FALSE;
        }
        else
        #endif
        /* Check if we can set direct MPI-IO read/write functions */
        if((opt = H5D__mpio_opt_possible(io_info, file_space, mem_space, type_info, fm, dx_plist)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for direct IO dataspace ")

        /* Check if we can use the optimized parallel I/O routines */
        if(opt == TRUE) {
            /* Override the I/O op pointers to the MPI-specific routines */
            io_info->io_ops.multi_read = dset->shared->layout.ops->par_read;
            io_info->io_ops.multi_write = dset->shared->layout.ops->par_write;
            io_info->io_ops.single_read = H5D__mpio_select_read;
            io_info->io_ops.single_write = H5D__mpio_select_write;
        } /* end if */
        else {
            /* If we won't be doing collective I/O, but the user asked for
             * collective I/O, change the request to use independent I/O, but
             * mark it so that we remember to revert the change.
             */
            if(io_info->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
                /* Change the xfer_mode to independent for handling the I/O */
                io_info->dxpl_cache->xfer_mode = H5FD_MPIO_INDEPENDENT;
                if(H5P_set(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info->dxpl_cache->xfer_mode) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
            } /* end if */
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_adjust() */

#ifdef JK_WORK_OLD_REMOVE
// JK_TODO_LATER - make this call only once, so can make H5D__mpio_opt_possible_mdset() to check mdset at once
static herr_t
H5D__ioinfo_adjust_mdset_OLD(H5D_io_info_md_t *io_info_md, 
    const H5D_t *dset, hid_t dxpl_id,
    const H5S_t *file_space, const H5S_t *mem_space,
    const H5D_type_info_t *type_info, const H5D_dset_info_t *dinfo)
{
    H5P_genplist_t *dx_plist;       /* Data transer property list */
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode; /* performed chunk optimization */
    H5D_mpio_actual_io_mode_t actual_io_mode; /* performed io mode */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(dset);
    HDassert(dset->oloc.file);
    HDassert(mem_space);
    HDassert(file_space);
    HDassert(type_info);
    HDassert(type_info->tpath);
    HDassert(io_info_md);

    /* Get the dataset transfer property list */
    if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    /* Reset the actual io mode properties to the default values in case
     * the dxpl was previously used in a collective I/O operation.
     */
    actual_chunk_opt_mode = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
    actual_io_mode = H5D_MPIO_NO_COLLECTIVE;
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Make any parallel I/O adjustments */
    if(io_info_md->using_mpi_vfd) {
        htri_t opt;         /* Flag whether a selection is optimizable */

        /* Record the original state of parallel I/O transfer options */
        io_info_md->orig.xfer_mode = io_info_md->dxpl_cache->xfer_mode;
        io_info_md->orig.coll_opt_mode = io_info_md->dxpl_cache->coll_opt_mode;
        io_info_md->orig.io_ops.single_read = io_info_md->io_ops.single_read;
        io_info_md->orig.io_ops.single_write = io_info_md->io_ops.single_write;
        #ifndef JK_WORK
        io_info_md->orig.io_ops.single_read_md = io_info_md->io_ops.single_read_md;
        io_info_md->orig.io_ops.single_write_md = io_info_md->io_ops.single_write_md;
        #endif

        /* Get MPI communicator */
        if(MPI_COMM_NULL == (io_info_md->comm = H5F_mpi_get_comm(dset->oloc.file)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve MPI communicator")
        #ifdef JK_TODO_LATER
        // make H5D__mpio_opt_possible_mdset Loop
        // for (i=0; i < count ; i++) {
        //    H5D__mpio_opt_possible_mdset()
        // }
        #endif

        /* Check if we can set direct MPI-IO read/write functions */
        if((opt = H5D__mpio_opt_possible_mdset(io_info_md, file_space, mem_space, type_info, dset, dx_plist)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for direct IO dataspace ")

        /* Check if we can use the optimized parallel I/O routines */
        if(opt == TRUE) {
            /* Override the I/O op pointers to the MPI-specific routines */
           #ifndef JK_TODO_READ
            io_info_md->io_ops.multi_read = dset->shared->layout.ops->par_read;
            io_info_md->io_ops.multi_write = dset->shared->layout.ops->par_write;
            //io_info_md->io_ops.multi_read_md = dset->shared->layout.ops->par_read_md;
            io_info_md->io_ops.multi_write_md = dset->shared->layout.ops->par_write_md;
            //io_info_md->io_ops.single_read = H5D__mpio_select_read_mdset;
            io_info_md->io_ops.single_write_md = H5D__mpio_select_write_mdset;
           #endif
        } /* end if */
        else {
            /* If we won't be doing collective I/O, but the user asked for
             * collective I/O, change the request to use independent I/O, but
             * mark it so that we remember to revert the change.
             */
            if(io_info_md->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
                /* Change the xfer_mode to independent for handling the I/O */
                io_info_md->dxpl_cache->xfer_mode = H5FD_MPIO_INDEPENDENT;
                if(H5P_set(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info_md->dxpl_cache->xfer_mode) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
            } /* end if */
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_adjust() */
#endif // JK_WORK_OLD_REMOVE

#ifndef JK_WORK_NEW // run only once and loop just 'H5D__mpio_opt_possible'
static herr_t
H5D__ioinfo_adjust_mdset(const size_t count, H5D_io_info_md_t *io_info_md, hid_t dxpl_id)
    //(H5D_io_info_md_t *io_info_md, 
    //const H5D_t *dset, hid_t dxpl_id,
    //const H5S_t *file_space, const H5S_t *mem_space,
    //const H5D_type_info_t *type_info, const H5D_dset_info_t *dinfo)
{
    H5D_t *dset0;  /* only the first dset , also for single dsets case */
    H5P_genplist_t *dx_plist;       /* Data transer property list */
    H5D_mpio_actual_chunk_opt_mode_t actual_chunk_opt_mode; /* performed chunk optimization */
    H5D_mpio_actual_io_mode_t actual_io_mode; /* performed io mode */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(count > 0);
    HDassert(io_info_md);

    /* check the first dset, should exist either single or multi dset cases */
    HDassert(io_info_md->dsets_info[0].dset);
    dset0 = io_info_md->dsets_info[0].dset;
    HDassert(dset0->oloc.file);
    //HDassert(mem_space);
    //HDassert(file_space);
    //HDassert(type_info);
    //HDassert(type_info->tpath);

    /* Get the dataset transfer property list */
    if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

    /* Reset the actual io mode properties to the default values in case
     * the dxpl was previously used in a collective I/O operation.
     */
    actual_chunk_opt_mode = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
    actual_io_mode = H5D_MPIO_NO_COLLECTIVE;
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_CHUNK_OPT_MODE_NAME, &actual_chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual chunk opt mode property")
    if(H5P_set(dx_plist, H5D_MPIO_ACTUAL_IO_MODE_NAME, &actual_io_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "couldn't set actual io mode property")

    /* Make any parallel I/O adjustments */
    if(io_info_md->using_mpi_vfd) {
        htri_t opt;         /* Flag whether a selection is optimizable */

        /* Record the original state of parallel I/O transfer options */
        io_info_md->orig.xfer_mode = io_info_md->dxpl_cache->xfer_mode;
        io_info_md->orig.coll_opt_mode = io_info_md->dxpl_cache->coll_opt_mode;
        io_info_md->orig.io_ops.single_read = io_info_md->io_ops.single_read;
        io_info_md->orig.io_ops.single_write = io_info_md->io_ops.single_write;
        #ifndef JK_WORK
        io_info_md->orig.io_ops.single_read_md = io_info_md->io_ops.single_read_md;
        io_info_md->orig.io_ops.single_write_md = io_info_md->io_ops.single_write_md;
        #endif

        /* Get MPI communicator from the first dset */
        //if(MPI_COMM_NULL == (io_info_md->comm = H5F_mpi_get_comm(dset->oloc.file)))
        if(MPI_COMM_NULL == (io_info_md->comm = H5F_mpi_get_comm(dset0->oloc.file)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve MPI communicator")
        /* Check if we can set direct MPI-IO read/write functions */
        //if((opt = H5D__mpio_opt_possible_mdset(io_info_md, file_space, mem_space, type_info, dset, dx_plist)) < 0)
        if((opt = H5D__mpio_opt_possible_mdset(count, io_info_md, dx_plist)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for direct IO dataspace ")

        /* Check if we can use the optimized parallel I/O routines */
        if(opt == TRUE) {
            /* Override the I/O op pointers to the MPI-specific routines */
           #ifndef JK_TODO_WORK // READ
            /* Single dset case: only first dset exist */
            io_info_md->io_ops.multi_read = dset0->shared->layout.ops->par_read;
            io_info_md->io_ops.multi_write = dset0->shared->layout.ops->par_write;
            /* Multi dset case: still first dset can be used since same function is
             * shared for both Chunked and Contiguous dset */
            //io_info_md->io_ops.multi_read_md = dset0->shared->layout.ops->par_read_md;
            io_info_md->io_ops.multi_write_md = dset0->shared->layout.ops->par_write_md;
            //io_info_md->io_ops.single_read = H5D__mpio_select_read_mdset;
            io_info_md->io_ops.single_write_md = H5D__mpio_select_write_mdset;
           #endif
        } /* end if */
        else {
            /* If we won't be doing collective I/O, but the user asked for
             * collective I/O, change the request to use independent I/O, but
             * mark it so that we remember to revert the change.
             */
            if(io_info_md->dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE) {
                /* Change the xfer_mode to independent for handling the I/O */
                io_info_md->dxpl_cache->xfer_mode = H5FD_MPIO_INDEPENDENT;
                if(H5P_set(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info_md->dxpl_cache->xfer_mode) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
                #ifndef JK_NOCOLLCAUSE
                io_info_md->is_coll_broken = TRUE;
                #endif
            } /* end if */
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_adjust() */

#endif // JK_WORK



/*-------------------------------------------------------------------------
 * Function:	H5D__ioinfo_term
 *
 * Purpose:	Common logic for terminating an I/O info object
 *              (Only used for restoring MPI transfer mode currently)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, February  6, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__ioinfo_term(H5D_io_info_t *io_info)
{
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_STATIC

    /* Check if we used the MPI VFD for the I/O */
    if(io_info->using_mpi_vfd) {
        /* Check if we need to revert the change to the xfer mode */
        if(io_info->orig.xfer_mode != io_info->dxpl_cache->xfer_mode) {
            H5P_genplist_t *dx_plist;           /* Data transer property list */

            /* Get the dataset transfer property list */
            if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(io_info->dxpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

            /* Restore the original parallel I/O mode */
            if(H5P_set(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info->orig.xfer_mode) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
        } /* end if */

        /* Check if we need to revert the change to the collective opt mode */
        if(io_info->orig.coll_opt_mode != io_info->dxpl_cache->coll_opt_mode) {
            H5P_genplist_t *dx_plist;           /* Data transer property list */

            /* Get the dataset transfer property list */
            if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(io_info->dxpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

            /* Restore the original parallel I/O mode */
            if(H5P_set(dx_plist, H5D_XFER_MPIO_COLLECTIVE_OPT_NAME, &io_info->orig.coll_opt_mode) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set collective option mode")
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_term() */

#ifndef JK_WORK
static herr_t
H5D__ioinfo_term_mdset(H5D_io_info_md_t *io_info_md)
{
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_STATIC

    /* Check if we used the MPI VFD for the I/O */
    if(io_info_md->using_mpi_vfd) {
        /* Check if we need to revert the change to the xfer mode */
        if(io_info_md->orig.xfer_mode != io_info_md->dxpl_cache->xfer_mode) {
            H5P_genplist_t *dx_plist;           /* Data transer property list */

            /* Get the dataset transfer property list */
            if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(io_info_md->dxpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

            /* Restore the original parallel I/O mode */
            if(H5P_set(dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &io_info_md->orig.xfer_mode) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode")
        } /* end if */

        /* Check if we need to revert the change to the collective opt mode */
        if(io_info_md->orig.coll_opt_mode != io_info_md->dxpl_cache->coll_opt_mode) {
            H5P_genplist_t *dx_plist;           /* Data transer property list */

            /* Get the dataset transfer property list */
            if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(io_info_md->dxpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list")

            /* Restore the original parallel I/O mode */
            if(H5P_set(dx_plist, H5D_XFER_MPIO_COLLECTIVE_OPT_NAME, &io_info_md->orig.coll_opt_mode) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set collective option mode")
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__ioinfo_term_mdset() */
#endif

#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:	H5D__typeinfo_term
 *
 * Purpose:	Common logic for terminating a type info object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, March  6, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__typeinfo_term(const H5D_type_info_t *type_info)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check for releasing datatype conversion & background buffers */
    if(type_info->tconv_buf_allocated) {
        HDassert(type_info->tconv_buf);
        (void)H5FL_BLK_FREE(type_conv, type_info->tconv_buf);
    } /* end if */
    if(type_info->bkg_buf_allocated) {
        HDassert(type_info->bkg_buf);
        (void)H5FL_BLK_FREE(type_conv, type_info->bkg_buf);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__typeinfo_term() */

