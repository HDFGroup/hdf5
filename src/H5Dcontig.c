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
 * Programmer: 	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *	       	Thursday, September 28, 2000
 *
 * Purpose:
 *      Contiguous dataset I/O functions. These routines are similar to
 *      the H5D_istore_* routines and really only an abstract way of dealing
 *      with the data sieve buffer from H5F_seq_read/write.
 */

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
#include "H5Fprivate.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Vprivate.h"		/* Vector and array functions		*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5D_contig_write(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, const H5D_storage_t *store, hsize_t offset, size_t size, const void *buf);

/*********************/
/* Package Variables */
/*********************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a PQ free list to manage the sieve buffer information */
H5FL_BLK_DEFINE(sieve_buf);

/* Declare extern the free list to manage blocks of type conversion data */
H5FL_BLK_EXTERN(type_conv);


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_create
 *
 * Purpose:	Allocate file space for a contiguously stored dataset
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		April 19, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_create(H5F_t *f, hid_t dxpl_id, H5O_layout_t *layout /*out */ )
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_create, FAIL)

    /* check args */
    HDassert(f);
    HDassert(layout);

    /* Allocate space for the contiguous data */
    if(HADDR_UNDEF == (layout->u.contig.addr = H5MF_alloc(f, H5FD_MEM_DRAW, dxpl_id, layout->u.contig.size)))
        HGOTO_ERROR(H5E_IO, H5E_NOSPACE, FAIL, "unable to reserve file space")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_create */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_fill
 *
 * Purpose:	Write fill values to a contiguously stored dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		August 22, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_fill(H5D_t *dset, hid_t dxpl_id)
{
    H5D_storage_t store;                /* Union of storage info for dataset */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    hssize_t    snpoints;       /* Number of points in space (for error checking) */
    size_t      npoints;        /* Number of points in space */
    hsize_t	offset;         /* Offset of dataset */
#ifdef H5_HAVE_PARALLEL
    MPI_Comm	mpi_comm = MPI_COMM_NULL;	/* MPI communicator for file */
    int         mpi_rank = (-1);  /* This process's rank  */
    int         mpi_code;       /* MPI return code */
    hbool_t     blocks_written = FALSE; /* Flag to indicate that chunk was actually written */
    hbool_t     using_mpi = FALSE;      /* Flag to indicate that the file is being accessed with an MPI-capable file driver */
#endif /* H5_HAVE_PARALLEL */
    H5D_fill_buf_info_t fb_info;        /* Dataset's fill buffer info */
    hbool_t     fb_info_init = FALSE;   /* Whether the fill value buffer has been initialized */
    hid_t       my_dxpl_id;     /* DXPL ID to use for this operation */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_fill, FAIL)

    /* Check args */
    HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
    HDassert(dset && H5D_CONTIGUOUS == dset->shared->layout.type);
    HDassert(H5F_addr_defined(dset->shared->layout.u.contig.addr));
    HDassert(dset->shared->layout.u.contig.size > 0);
    HDassert(dset->shared->space);
    HDassert(dset->shared->type);

#ifdef H5_HAVE_PARALLEL
    /* Retrieve MPI parameters */
    if(IS_H5FD_MPI(dset->oloc.file)) {
        /* Get the MPI communicator */
        if(MPI_COMM_NULL == (mpi_comm = H5F_mpi_get_comm(dset->oloc.file)))
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "Can't retrieve MPI communicator")

        /* Get the MPI rank */
        if((mpi_rank = H5F_mpi_get_rank(dset->oloc.file)) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "Can't retrieve MPI rank")

        /* Set the MPI-capable file driver flag */
        using_mpi = TRUE;

        /* Use the internal "independent" DXPL */
        my_dxpl_id = H5AC_ind_dxpl_id;
    } /* end if */
    else {
#endif  /* H5_HAVE_PARALLEL */
        /* Use the DXPL we were given */
        my_dxpl_id = dxpl_id;
#ifdef H5_HAVE_PARALLEL
    } /* end else */
#endif  /* H5_HAVE_PARALLEL */

    /* Fill the DXPL cache values for later use */
    if(H5D_get_dxpl_cache(my_dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Initialize storage info for this dataset */
    store.contig.dset_addr = dset->shared->layout.u.contig.addr;
    store.contig.dset_size = dset->shared->layout.u.contig.size;

    /* Get the number of elements in the dataset's dataspace */
    snpoints = H5S_GET_EXTENT_NPOINTS(dset->shared->space);
    HDassert(snpoints >= 0);
    H5_ASSIGN_OVERFLOW(npoints, snpoints, hssize_t, size_t);

    /* Initialize the fill value buffer */
    if(H5D_fill_init(&fb_info, NULL, FALSE, NULL, NULL, NULL, NULL,
            &dset->shared->dcpl_cache.fill,
            dset->shared->type, dset->shared->type_id, npoints,
            dxpl_cache->max_temp_buf, my_dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize fill buffer info")
    fb_info_init = TRUE;

    /* Start at the beginning of the dataset */
    offset = 0;

    /*
     * Fill the entire current extent with the fill value.  We can do
     * this quite efficiently by making sure we copy the fill value
     * in relatively large pieces.
     */

    /* Loop through writing the fill value to the dataset */
    while(npoints > 0) {
        size_t curr_points;     /* Number of elements to write on this iteration of the loop */
        size_t size;            /* Size of buffer to write */
        
        /* Compute # of elements and buffer size to write for this iteration */
        curr_points = MIN(fb_info.elmts_per_buf, npoints);
        size = curr_points * fb_info.file_elmt_size;

        /* Check for VL datatype & non-default fill value */
        if(fb_info.has_vlen_fill_type)
            /* Re-fill the buffer to use for this I/O operation */
            if(H5D_fill_refill_vl(&fb_info, curr_points, my_dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "can't refill fill value buffer")

#ifdef H5_HAVE_PARALLEL
            /* Check if this file is accessed with an MPI-capable file driver */
            if(using_mpi) {
                /* Write the chunks out from only one process */
                /* !! Use the internal "independent" DXPL!! -QAK */
                if(H5_PAR_META_WRITE == mpi_rank)
                    if(H5D_contig_write(dset, dxpl_cache, my_dxpl_id, &store, offset, size, fb_info.fill_buf) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to write fill value to dataset")

                /* Indicate that blocks are being written */
                blocks_written = TRUE;
            } /* end if */
            else {
#endif /* H5_HAVE_PARALLEL */
                H5_CHECK_OVERFLOW(size, size_t, hsize_t);
                if(H5D_contig_write(dset, dxpl_cache, my_dxpl_id, &store, offset, size, fb_info.fill_buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to write fill value to dataset")
#ifdef H5_HAVE_PARALLEL
            } /* end else */
#endif /* H5_HAVE_PARALLEL */

          npoints -= curr_points;
          offset += size;
      } /* end while */

#ifdef H5_HAVE_PARALLEL
    /* Only need to block at the barrier if we actually wrote fill values */
    /* And if we are using an MPI-capable file driver */
    if(using_mpi && blocks_written) {
        /* Wait at barrier to avoid race conditions where some processes are
         * still writing out fill values and other processes race ahead to data
         * in, getting bogus data.
         */
        if(MPI_SUCCESS != (mpi_code = MPI_Barrier(mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

done:
    /* Release the fill buffer info, if it's been initialized */
    if(fb_info_init && H5D_fill_term(&fb_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release fill buffer info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_delete
 *
 * Purpose:	Delete the file space for a contiguously stored dataset
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		March 20, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_delete(H5F_t *f, hid_t dxpl_id, const struct H5O_layout_t *layout)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_delete, FAIL)

    /* check args */
    HDassert(f);
    HDassert(layout);

    /* Free the file space for the chunk */
    if(H5MF_xfree(f, H5FD_MEM_DRAW, dxpl_id, layout->u.contig.addr, layout->u.contig.size) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_delete */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_get_addr
 *
 * Purpose:	Get the offset of the contiguous data on disk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		June  2, 2004
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5D_contig_get_addr(const H5D_t *dset)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5D_contig_get_addr)

    /* check args */
    assert(dset);
    assert(dset->shared->layout.type==H5D_CONTIGUOUS);

    FUNC_LEAVE_NOAPI(dset->shared->layout.u.contig.addr)
} /* end H5D_contig_get_addr */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_write
 *
 * Purpose:	Writes some data from a dataset into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 28, 2000
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_contig_write(H5D_t *dset, const H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, const H5D_storage_t *store,
    hsize_t offset, size_t size, const void *buf)
{
    H5D_io_info_t io_info;      /* Dataset I/O info */
    hsize_t dset_off=offset;    /* Offset in dataset */
    size_t dset_len=size;       /* Length in dataset */
    size_t dset_curr_seq=0;     /* "Current sequence" in dataset */
    hsize_t mem_off=0;          /* Offset in memory */
    size_t mem_len=size;        /* Length in memory */
    size_t mem_curr_seq=0;      /* "Current sequence" in memory */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_write, FAIL)

    assert (dset);
    assert (dxpl_cache);
    assert (store);
    assert (buf);

    H5D_BUILD_IO_INFO(&io_info,dset,dxpl_cache,dxpl_id,store);
    if(H5D_contig_writevv(&io_info, (size_t)1, &dset_curr_seq, &dset_len, &dset_off, 
            (size_t)1, &mem_curr_seq, &mem_len, &mem_off, (haddr_t)0, NULL, buf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vector write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D_contig_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_readvv
 *
 * Purpose:	Reads some data vectors from a dataset into a buffer.
 *		The data is contiguous.	 The address is the start of the dataset,
 *              relative to the base address for the file and the offsets and
 *              sequence lengths are in bytes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 3, 2001
 *
 * Notes:
 *      Offsets in the sequences must be monotonically increasing
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_contig_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED address, void UNUSED *pointer, void *_buf)
{
    H5F_t *file = io_info->dset->oloc.file;        /* File for dataset */
    H5D_rdcdc_t *dset_contig=&(io_info->dset->shared->cache.contig); /* Cached information about contiguous data */
    const H5D_contig_storage_t *store_contig=&(io_info->store->contig);    /* Contiguous storage info for this I/O operation */
    unsigned char *buf=(unsigned char *)_buf;   /* Pointer to buffer to fill */
    haddr_t addr;               /* Actual address to read */
    size_t total_size=0;        /* Total size of sequence in bytes */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
    size_t v;                   /* Counting variable */
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_readvv, FAIL)

    /* Check args */
    assert(io_info);
    assert(io_info->dset);
    assert(io_info->store);
    assert(buf);

    /* Check if data sieving is enabled */
    if(H5F_HAS_FEATURE(file,H5FD_FEAT_DATA_SIEVE)) {
        haddr_t sieve_start=HADDR_UNDEF, sieve_end=HADDR_UNDEF;     /* Start & end locations of sieve buffer */
        haddr_t contig_end;             /* End locations of block to write */
        size_t sieve_size=(size_t)-1;   /* size of sieve buffer */
        haddr_t abs_eoa;	        /* Absolute end of file address		*/
        haddr_t rel_eoa;	        /* Relative end of file address		*/
        hsize_t max_data;               /* Actual maximum size of data to cache */

        /* Set offsets in sequence lists */
        u=*dset_curr_seq;
        v=*mem_curr_seq;

        /* Stash local copies of these value */
        if(dset_contig->sieve_buf!=NULL) {
            sieve_start=dset_contig->sieve_loc;
            sieve_size=dset_contig->sieve_size;
            sieve_end=sieve_start+sieve_size;
        } /* end if */

        /* Works through sequences as fast as possible */
        for(; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=store_contig->dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (unsigned char *)_buf + mem_offset_arr[v];

            /* Check if the sieve buffer is allocated yet */
            if(dset_contig->sieve_buf==NULL) {
                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>dset_contig->sieve_buf_size) {
                    if (H5F_block_read(file, H5FD_MEM_DRAW, addr, size, io_info->dxpl_id, buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed")
                } /* end if */
                else {
                    /* Allocate room for the data sieve buffer */
                    if (NULL==(dset_contig->sieve_buf=H5FL_BLK_MALLOC(sieve_buf,dset_contig->sieve_buf_size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

                    /* Determine the new sieve buffer size & location */
                    dset_contig->sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(file)))
                        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size")

                    /* Adjust absolute EOA address to relative EOA address */
                    rel_eoa=abs_eoa-H5F_get_base_addr(file);

                    /* Set up the buffer parameters */
                    max_data=store_contig->dset_size-dset_offset_arr[u];

                    /* Compute the size of the sieve buffer */
                    H5_ASSIGN_OVERFLOW(dset_contig->sieve_size,MIN3(rel_eoa-dset_contig->sieve_loc,max_data,dset_contig->sieve_buf_size),hsize_t,size_t);

                    /* Read the new sieve buffer */
                    if (H5F_block_read(file, H5FD_MEM_DRAW, dset_contig->sieve_loc, dset_contig->sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed")

                    /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                    HDmemcpy(buf,dset_contig->sieve_buf,size);

                    /* Reset sieve buffer dirty flag */
                    dset_contig->sieve_dirty=0;

                    /* Stash local copies of these value */
                    sieve_start=dset_contig->sieve_loc;
                    sieve_size=dset_contig->sieve_size;
                    sieve_end=sieve_start+sieve_size;
                } /* end else */
            } /* end if */
            else {
                /* Compute end of sequence to retrieve */
                contig_end=addr+size-1;

                /* If entire read is within the sieve buffer, read it from the buffer */
                if(addr>=sieve_start && contig_end<sieve_end) {
                    unsigned char *base_sieve_buf=dset_contig->sieve_buf+(addr-sieve_start);

                    /* Grab the data out of the buffer */
                    HDmemcpy(buf,base_sieve_buf,size);
                } /* end if */
                /* Entire request is not within this data sieve buffer */
                else {
                    /* Check if we can actually hold the I/O request in the sieve buffer */
                    if(size>dset_contig->sieve_buf_size) {
                        /* Check for any overlap with the current sieve buffer */
                        if((sieve_start>=addr && sieve_start<(contig_end+1))
                                || ((sieve_end-1)>=addr && (sieve_end-1)<(contig_end+1))) {
                            /* Flush the sieve buffer, if it's dirty */
                            if(dset_contig->sieve_dirty) {
                                /* Write to file */
                                if (H5F_block_write(file, H5FD_MEM_DRAW, sieve_start, sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")

                                /* Reset sieve buffer dirty flag */
                                dset_contig->sieve_dirty=0;
                            } /* end if */
                        } /* end if */

                        /* Read directly into the user's buffer */
                        if (H5F_block_read(file, H5FD_MEM_DRAW, addr, size, io_info->dxpl_id, buf)<0)
                            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed")
                    } /* end if */
                    /* Element size fits within the buffer size */
                    else {
                        /* Flush the sieve buffer if it's dirty */
                        if(dset_contig->sieve_dirty) {
                            /* Write to file */
                            if (H5F_block_write(file, H5FD_MEM_DRAW, sieve_start, sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")

                            /* Reset sieve buffer dirty flag */
                            dset_contig->sieve_dirty=0;
                        } /* end if */

                        /* Determine the new sieve buffer size & location */
                        dset_contig->sieve_loc=addr;

                        /* Make certain we don't read off the end of the file */
                        if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(file)))
                            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size")

                        /* Adjust absolute EOA address to relative EOA address */
                        rel_eoa=abs_eoa-H5F_get_base_addr(file);

                        /* Only need this when resizing sieve buffer */
                        max_data=store_contig->dset_size-dset_offset_arr[u];

                        /* Compute the size of the sieve buffer */
                        /* Don't read off the end of the file, don't read past the end of the data element and don't read more than the buffer size */
                        H5_ASSIGN_OVERFLOW(dset_contig->sieve_size,MIN3(rel_eoa-dset_contig->sieve_loc,max_data,dset_contig->sieve_buf_size),hsize_t,size_t);

                        /* Update local copies of sieve information */
                        sieve_start=dset_contig->sieve_loc;
                        sieve_size=dset_contig->sieve_size;
                        sieve_end=sieve_start+sieve_size;

                        /* Read the new sieve buffer */
                        if (H5F_block_read(file, H5FD_MEM_DRAW, dset_contig->sieve_loc, dset_contig->sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed")

                        /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                        HDmemcpy(buf,dset_contig->sieve_buf,size);

                        /* Reset sieve buffer dirty flag */
                        dset_contig->sieve_dirty=0;
                    } /* end else */
                } /* end else */
            } /* end else */

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            total_size+=size;
        } /* end for */
    } /* end if */
    else {
        /* Work through all the sequences */
        for(u=*dset_curr_seq, v=*mem_curr_seq; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=store_contig->dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (unsigned char *)_buf + mem_offset_arr[v];

            /* Write data */
            if (H5F_block_read(file, H5FD_MEM_DRAW, addr, size, io_info->dxpl_id, buf)<0)
                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            total_size+=size;
        } /* end for */
    } /* end else */

    /* Update current sequence vectors */
    *dset_curr_seq=u;
    *mem_curr_seq=v;

    /* Set return value */
    H5_ASSIGN_OVERFLOW(ret_value,total_size,size_t,ssize_t);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D_contig_readvv() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_writevv
 *
 * Purpose:	Writes some data vectors into a dataset from vectors into a
 *              buffer.  The address is the start of the dataset,
 *              relative to the base address for the file and the offsets and
 *              sequence lengths are in bytes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 2, 2003
 *
 * Notes:
 *      Offsets in the sequences must be monotonically increasing
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_contig_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED address, void UNUSED *pointer, const void *_buf)
{
    H5F_t *file = io_info->dset->oloc.file;        /* File for dataset */
    H5D_rdcdc_t *dset_contig=&(io_info->dset->shared->cache.contig); /* Cached information about contiguous data */
    const H5D_contig_storage_t *store_contig=&(io_info->store->contig);    /* Contiguous storage info for this I/O operation */
    const unsigned char *buf=(const unsigned char *)_buf;      /* Pointer to buffer to fill */
    haddr_t addr;               /* Actual address to read */
    size_t total_size=0;        /* Size of sequence in bytes */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
    size_t v;                   /* Counting variable */
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_writevv, FAIL)

    /* Check args */
    assert(io_info);
    assert(io_info->dset);
    assert(io_info->store);
    assert(buf);

    /* Check if data sieving is enabled */
    if(H5F_HAS_FEATURE(file,H5FD_FEAT_DATA_SIEVE)) {
        haddr_t sieve_start=HADDR_UNDEF, sieve_end=HADDR_UNDEF;     /* Start & end locations of sieve buffer */
        haddr_t contig_end;             /* End locations of block to write */
        size_t sieve_size=(size_t)-1;   /* size of sieve buffer */
        haddr_t abs_eoa;	        /* Absolute end of file address		*/
        haddr_t rel_eoa;	        /* Relative end of file address		*/
        hsize_t max_data;               /* Actual maximum size of data to cache */

        /* Set offsets in sequence lists */
        u=*dset_curr_seq;
        v=*mem_curr_seq;

        /* Stash local copies of these values */
        if(dset_contig->sieve_buf!=NULL) {
            sieve_start=dset_contig->sieve_loc;
            sieve_size=dset_contig->sieve_size;
            sieve_end=sieve_start+sieve_size;
        } /* end if */

        /* Works through sequences as fast as possible */
        for(; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=store_contig->dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (const unsigned char *)_buf + mem_offset_arr[v];

            /* No data sieve buffer yet, go allocate one */
            if(dset_contig->sieve_buf==NULL) {
                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>dset_contig->sieve_buf_size) {
                    if (H5F_block_write(file, H5FD_MEM_DRAW, addr, size, io_info->dxpl_id, buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")
                } /* end if */
                else {
                    /* Allocate room for the data sieve buffer */
                    if (NULL==(dset_contig->sieve_buf=H5FL_BLK_MALLOC(sieve_buf,dset_contig->sieve_buf_size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#ifdef H5_CLEAR_MEMORY
if(dset_contig->sieve_size > size)
    HDmemset(dset_contig->sieve_buf + size, 0, (dset_contig->sieve_size - size));
#endif /* H5_CLEAR_MEMORY */

                    /* Determine the new sieve buffer size & location */
                    dset_contig->sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(file)))
                        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size")

                    /* Adjust absolute EOA address to relative EOA address */
                    rel_eoa=abs_eoa-H5F_get_base_addr(file);

                    /* Set up the buffer parameters */
                    max_data=store_contig->dset_size-dset_offset_arr[u];

                    /* Compute the size of the sieve buffer */
                    H5_ASSIGN_OVERFLOW(dset_contig->sieve_size,MIN3(rel_eoa-dset_contig->sieve_loc,max_data,dset_contig->sieve_buf_size),hsize_t,size_t);

                    /* Check if there is any point in reading the data from the file */
                    if(dset_contig->sieve_size>size) {
                        /* Read the new sieve buffer */
                        if (H5F_block_read(file, H5FD_MEM_DRAW, dset_contig->sieve_loc, dset_contig->sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed")
                    } /* end if */

                    /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                    HDmemcpy(dset_contig->sieve_buf,buf,size);

                    /* Set sieve buffer dirty flag */
                    dset_contig->sieve_dirty=1;

                    /* Stash local copies of these values */
                    sieve_start=dset_contig->sieve_loc;
                    sieve_size=dset_contig->sieve_size;
                    sieve_end=sieve_start+sieve_size;
                } /* end else */
            } /* end if */
            else {
                /* Compute end of sequence to retrieve */
                contig_end=addr+size-1;

                /* If entire write is within the sieve buffer, write it to the buffer */
                if(addr>=sieve_start && contig_end<sieve_end) {
                    unsigned char *base_sieve_buf=dset_contig->sieve_buf+(addr-sieve_start);

                    /* Put the data into the sieve buffer */
                    HDmemcpy(base_sieve_buf,buf,size);

                    /* Set sieve buffer dirty flag */
                    dset_contig->sieve_dirty=1;
                } /* end if */
                /* Entire request is not within this data sieve buffer */
                else {
                    /* Check if we can actually hold the I/O request in the sieve buffer */
                    if(size>dset_contig->sieve_buf_size) {
                        /* Check for any overlap with the current sieve buffer */
                        if((sieve_start>=addr && sieve_start<(contig_end+1))
                                || ((sieve_end-1)>=addr && (sieve_end-1)<(contig_end+1))) {
                            /* Flush the sieve buffer, if it's dirty */
                            if(dset_contig->sieve_dirty) {
                                /* Write to file */
                                if (H5F_block_write(file, H5FD_MEM_DRAW, sieve_start, sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")

                                /* Reset sieve buffer dirty flag */
                                dset_contig->sieve_dirty=0;
                            } /* end if */

                            /* Force the sieve buffer to be re-read the next time */
                            dset_contig->sieve_loc=HADDR_UNDEF;
                            dset_contig->sieve_size=0;
                        } /* end if */

                        /* Write directly from the user's buffer */
                        if (H5F_block_write(file, H5FD_MEM_DRAW, addr, size, io_info->dxpl_id, buf)<0)
                            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")
                    } /* end if */
                    /* Element size fits within the buffer size */
                    else {
                        /* Check if it is possible to (exactly) prepend or append to existing (dirty) sieve buffer */
                        if(((addr+size)==sieve_start || addr==sieve_end) &&
                                (size+sieve_size)<=dset_contig->sieve_buf_size &&
                                dset_contig->sieve_dirty) {
                            /* Prepend to existing sieve buffer */
                            if((addr+size)==sieve_start) {
                                /* Move existing sieve information to correct location */
                                HDmemmove(dset_contig->sieve_buf+size,dset_contig->sieve_buf,dset_contig->sieve_size);

                                /* Copy in new information (must be first in sieve buffer) */
                                HDmemcpy(dset_contig->sieve_buf,buf,size);

                                /* Adjust sieve location */
                                dset_contig->sieve_loc=addr;

                            } /* end if */
                            /* Append to existing sieve buffer */
                            else {
                                /* Copy in new information */
                                HDmemcpy(dset_contig->sieve_buf+sieve_size,buf,size);
                            } /* end else */

                            /* Adjust sieve size */
                            dset_contig->sieve_size += size;

                            /* Update local copies of sieve information */
                            sieve_start=dset_contig->sieve_loc;
                            sieve_size=dset_contig->sieve_size;
                            sieve_end=sieve_start+sieve_size;
                        } /* end if */
                        /* Can't add the new data onto the existing sieve buffer */
                        else {
                            /* Flush the sieve buffer if it's dirty */
                            if(dset_contig->sieve_dirty) {
                                /* Write to file */
                                if (H5F_block_write(file, H5FD_MEM_DRAW, sieve_start, sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")

                                /* Reset sieve buffer dirty flag */
                                dset_contig->sieve_dirty=0;
                            } /* end if */

                            /* Determine the new sieve buffer size & location */
                            dset_contig->sieve_loc=addr;

                            /* Make certain we don't read off the end of the file */
                            if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(file)))
                                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size")

                            /* Adjust absolute EOA address to relative EOA address */
                            rel_eoa=abs_eoa-H5F_get_base_addr(file);

                            /* Only need this when resizing sieve buffer */
                            max_data=store_contig->dset_size-dset_offset_arr[u];

                            /* Compute the size of the sieve buffer */
                            /* Don't read off the end of the file, don't read past the end of the data element and don't read more than the buffer size */
                            H5_ASSIGN_OVERFLOW(dset_contig->sieve_size,MIN3(rel_eoa-dset_contig->sieve_loc,max_data,dset_contig->sieve_buf_size),hsize_t,size_t);

                            /* Update local copies of sieve information */
                            sieve_start=dset_contig->sieve_loc;
                            sieve_size=dset_contig->sieve_size;
                            sieve_end=sieve_start+sieve_size;

                            /* Check if there is any point in reading the data from the file */
                            if(dset_contig->sieve_size>size) {
                                /* Read the new sieve buffer */
                                if (H5F_block_read(file, H5FD_MEM_DRAW, dset_contig->sieve_loc, dset_contig->sieve_size, io_info->dxpl_id, dset_contig->sieve_buf)<0)
                                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed")
                            } /* end if */

                            /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                            HDmemcpy(dset_contig->sieve_buf,buf,size);

                            /* Set sieve buffer dirty flag */
                            dset_contig->sieve_dirty=1;
                        } /* end else */
                    } /* end else */
                } /* end else */
            } /* end else */

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            total_size+=size;
        } /* end for */
    } /* end if */
    else {
        /* Work through all the sequences */
        for(u=*dset_curr_seq, v=*mem_curr_seq; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=store_contig->dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (const unsigned char *)_buf + mem_offset_arr[v];

            /* Write data */
            if (H5F_block_write(file, H5FD_MEM_DRAW, addr, size, io_info->dxpl_id, buf)<0)
                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed")

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            total_size+=size;
        } /* end for */
    } /* end else */

    /* Update current sequence vectors */
    *dset_curr_seq=u;
    *mem_curr_seq=v;

    /* Set return value */
    H5_ASSIGN_OVERFLOW(ret_value,total_size,size_t,ssize_t);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D_contig_writevv() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_copy
 *
 * Purpose:	Copy contiguous storage raw data from SRC file to DST file.
 *
 * Return:	Non-negative on success, negative on failure.
 *
 * Programmer:  Quincey Koziol
 *	        Monday, November 21, 2005
 *
 * Modifier:    Peter Cao
 *              Saturday, January 07, 2006
 *              Add case to deal with compressed variable length datasets
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_copy(H5F_t *f_src, const H5O_layout_t *layout_src, H5F_t *f_dst, 
    H5O_layout_t *layout_dst, H5T_t *dt_src, H5O_copy_t *cpy_info, hid_t dxpl_id)
{
    haddr_t     addr_src;               /* File offset in source dataset */
    haddr_t     addr_dst;               /* File offset in destination dataset */
    H5T_path_t  *tpath_src_mem = NULL, *tpath_mem_dst = NULL;   /* Datatype conversion paths */
    H5T_t       *dt_dst = NULL;         /* Destination datatype */
    H5T_t       *dt_mem = NULL;         /* Memory datatype */
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    hid_t       tid_mem = -1;           /* Datatype ID for memory datatype */
    size_t      src_dt_size = 0;        /* Source datatype size */
    size_t      mem_dt_size = 0;        /* Memory datatype size */
    size_t      dst_dt_size = 0;        /* Destination datatype size */
    size_t      max_dt_size;            /* Max. datatype size */
    size_t      nelmts = 0;             /* Number of elements in buffer */
    size_t      src_nbytes;             /* Number of bytes to read from source */
    size_t      mem_nbytes;             /* Number of bytes to convert in memory */
    size_t      dst_nbytes;             /* Number of bytes to write to destination */
    hsize_t     total_src_nbytes;       /* Total number of bytes to copy */
    size_t      buf_size;               /* Size of copy buffer */
    void       *buf = NULL;             /* Buffer for copying data */
    void       *bkg = NULL;             /* Temporary buffer for copying data */
    void       *reclaim_buf = NULL;     /* Buffer for reclaiming data */
    H5S_t      *buf_space = NULL;       /* Dataspace describing buffer */
    hid_t       buf_sid = -1;           /* ID for buffer dataspace */
    hsize_t     buf_dim;                /* Dimension for buffer */
    hbool_t     is_vlen = FALSE;        /* Flag to indicate that VL type conversion should occur */
    hbool_t     fix_ref = FALSE;        /* Flag to indicate that ref values should be fixed */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_copy, FAIL)

    /* Check args */
    HDassert(f_src);
    HDassert(layout_src && H5D_CONTIGUOUS == layout_src->type);
    HDassert(f_dst);
    HDassert(layout_dst && H5D_CONTIGUOUS == layout_dst->type);
    HDassert(dt_src);

    /* Allocate space for destination raw data */
    if(H5D_contig_create(f_dst, dxpl_id, layout_dst) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to allocate contiguous storage")

    /* Set up number of bytes to copy, and initial buffer size */
    /* (actually use the destination size, which has been fixed up, if necessary) */
    total_src_nbytes = layout_dst->u.contig.size;
    H5_CHECK_OVERFLOW(total_src_nbytes, hsize_t, size_t);
    buf_size = MIN(H5D_TEMP_BUF_SIZE, (size_t)total_src_nbytes);

    /* Create datatype ID for src datatype.  We may or may not use this ID,
     * but this ensures that the src datatype will be freed.
     */
    if((tid_src = H5I_register(H5I_DATATYPE, dt_src)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register source file datatype")

    /* If there's a VLEN source datatype, set up type conversion information */
    if(H5T_detect_class(dt_src, H5T_VLEN) > 0) {
        /* create a memory copy of the variable-length datatype */
        if(NULL == (dt_mem = H5T_copy(dt_src, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy")
        if((tid_mem = H5I_register(H5I_DATATYPE, dt_mem)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register memory datatype")

        /* create variable-length datatype at the destinaton file */
        if(NULL == (dt_dst = H5T_copy(dt_src, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy")
        if(H5T_set_loc(dt_dst, f_dst, H5T_LOC_DISK) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "cannot mark datatype on disk")
        if((tid_dst = H5I_register(H5I_DATATYPE, dt_dst)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register destination file datatype")

        /* Set up the conversion functions */
        if(NULL == (tpath_src_mem = H5T_path_find(dt_src, dt_mem, NULL, NULL, dxpl_id, FALSE)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and mem datatypes")
        if(NULL == (tpath_mem_dst = H5T_path_find(dt_mem, dt_dst, NULL, NULL, dxpl_id, FALSE)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between mem and dst datatypes")

        /* Determine largest datatype size */
        if(0 == (src_dt_size = H5T_get_size(dt_src)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        if(0 == (mem_dt_size = H5T_get_size(dt_mem)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        max_dt_size = MAX(src_dt_size, mem_dt_size);
        if(0 == (dst_dt_size = H5T_get_size(dt_dst)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        max_dt_size = MAX(max_dt_size, dst_dt_size);

        /* Set maximum number of whole elements that fit in buffer */
        if(0 == (nelmts = buf_size / max_dt_size))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "element size too large")

        /* Set the number of bytes to transfer */
        src_nbytes = nelmts * src_dt_size;
        dst_nbytes = nelmts * dst_dt_size;
        mem_nbytes = nelmts * mem_dt_size;

        /* Adjust buffer size to be multiple of elements */
        buf_size = nelmts * max_dt_size;

        /* Create dataspace for number of elements in buffer */
        buf_dim = nelmts;

        /* Create the space and set the initial extent */
        if(NULL == (buf_space = H5S_create_simple((unsigned)1, &buf_dim, NULL)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace")

        /* Atomize */
        if((buf_sid = H5I_register(H5I_DATASPACE, buf_space)) < 0) {
            H5S_close(buf_space);
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")
        } /* end if */

        /* Set flag to do type conversion */
        is_vlen = TRUE;
    } /* end if */
    else {
        /* Check for reference datatype */
        if(H5T_get_class(dt_src, FALSE) == H5T_REFERENCE) {
            /* Need to fix values of references when copying across files */
            if(f_src != f_dst)
                fix_ref = TRUE; 
        } /* end if */

        /* Set the number of bytes to read & write to the buffer size */
        src_nbytes = dst_nbytes = mem_nbytes = buf_size;
    } /* end else */

    /* Allocate space for copy buffer */
    HDassert(buf_size);
    if(NULL == (buf = H5FL_BLK_MALLOC(type_conv, buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for copy buffer")

    /* Need extra buffer for datatype conversions, to prevent stranding/leaking memory */
    if(is_vlen || fix_ref) {
        if(NULL == (reclaim_buf = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for copy buffer")

        /* allocate temporary bkg buff for data conversion */
        if(NULL == (bkg = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for copy buffer")
    } /* end if */

    /* Loop over copying data */
    addr_src = layout_src->u.contig.addr;
    addr_dst = layout_dst->u.contig.addr;
    while(total_src_nbytes > 0) {
        /* Check if we should reduce the number of bytes to transfer */
        if(total_src_nbytes < src_nbytes) {
            /* Adjust bytes to transfer */
            src_nbytes = (size_t)total_src_nbytes;

            /* Adjust dataspace describing buffer */
            if(is_vlen) {
                /* Adjust destination & memory bytes to transfer */
                nelmts = src_nbytes / src_dt_size;
                dst_nbytes = nelmts * dst_dt_size;
                mem_nbytes = nelmts * mem_dt_size;

                /* Adjust size of buffer's dataspace dimension */
                buf_dim = nelmts;

                /* Adjust size of buffer's dataspace */
                if(H5S_set_extent_real(buf_space, &buf_dim) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "unable to change buffer dataspace size")
            } /* end if */
            else
                /* Adjust destination & memory bytes to transfer */
                dst_nbytes = mem_nbytes = src_nbytes;
        } /* end if */

        /* Read raw data from source file */
        if(H5F_block_read(f_src, H5FD_MEM_DRAW, addr_src, src_nbytes, H5P_DATASET_XFER_DEFAULT, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "unable to read raw data")

        /* Perform datatype conversion, if necessary */
        if(is_vlen) {
            /* Convert from source file to memory */
	    if(H5T_convert(tpath_src_mem, tid_src, tid_mem, nelmts, (size_t)0, (size_t)0, buf, NULL, dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

            /* Copy into another buffer, to reclaim memory later */
            HDmemcpy(reclaim_buf, buf, mem_nbytes);

            /* Set background buffer to all zeros */
            HDmemset(bkg, 0, buf_size);

            /* Convert from memory to destination file */
	    if(H5T_convert(tpath_mem_dst, tid_mem, tid_dst, nelmts, (size_t)0, (size_t)0, buf, bkg, dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

            /* Reclaim space from variable length data */
            if(H5D_vlen_reclaim(tid_mem, buf_space, H5P_DATASET_XFER_DEFAULT, reclaim_buf) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_BADITER, FAIL, "unable to reclaim variable-length data")
	} /* end if */
        else if(fix_ref) {
            /* Check for expanding references */
            if(cpy_info->expand_ref) {
                size_t ref_count;

                /* Determine # of reference elements to copy */
                ref_count = src_nbytes / H5T_get_size(dt_src);

                /* Copy the reference elements */
                if(H5O_copy_expand_ref(f_src, buf, dxpl_id, f_dst, bkg, ref_count, H5T_get_ref_type(dt_src), cpy_info) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "unable to copy reference attribute")

                /* After fix ref, copy the new reference elements to the buffer to write out */
                HDmemcpy(buf, bkg,  buf_size);
            } /* end if */
            else
                /* Reset value to zero */
                HDmemset(buf, 0, src_nbytes);
        } /* end if */

        /* Write raw data to destination file */
        if(H5F_block_write(f_dst, H5FD_MEM_DRAW, addr_dst, dst_nbytes, H5P_DATASET_XFER_DEFAULT, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to write raw data")

        /* Adjust loop variables */
        addr_src += src_nbytes;
        addr_dst += dst_nbytes;
        total_src_nbytes -= src_nbytes;
    } /* end while */

done:
    if(buf_sid > 0)
        if(H5I_dec_ref(buf_sid) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary dataspace ID")
    if(tid_src > 0)
        if(H5I_dec_ref(tid_src) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(tid_dst > 0)
        if(H5I_dec_ref(tid_dst) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(tid_mem > 0)
        if(H5I_dec_ref(tid_mem) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(buf)
        (void)H5FL_BLK_FREE(type_conv, buf);
    if(reclaim_buf)
        (void)H5FL_BLK_FREE(type_conv, reclaim_buf);
    if(bkg)
        (void)H5FL_BLK_FREE(type_conv, bkg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_copy_conv
 *
 * Purpose:	Copies contiguous data from SRC file to DST file
 *              and does data conversion.  This function is similar to
 *              H5D_contig_copy and is mainly used by H5Dmodify_dtype.  
 *
 * Return:	Non-negative on success. 
 *		Negative on failure.
 *
 * Programmer:  Raymond Lu
 *	        8 October 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_copy_conv(H5F_t *file, const H5D_t *dset_src, const H5D_t *dset_dst, 
    H5O_copy_t UNUSED *cpy_info, hid_t dxpl_id)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    haddr_t     addr_src;               /* File offset in source dataset */
    haddr_t     addr_dst;               /* File offset in destination dataset */
    H5T_path_t  *tpath = NULL;          /* Datatype conversion paths */
    H5T_t       *dt_src = NULL;         /* Source datatype */
    H5T_t       *dt_dst = NULL;         /* Destination datatype */
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    size_t      src_dt_size = 0;        /* Source datatype size */
    size_t      dst_dt_size = 0;        /* Destination datatype size */
    size_t      max_dt_size;            /* Max. datatype size */
    hsize_t     nelmts = 0;             /* Number of elements in buffer */
    hssize_t    snelmts = 0;            /* Number of elements in buffer */
    size_t      request_nelmts;         /* requested strip mine  */
    size_t      src_nbytes;             /* Number of bytes to read from source */
    size_t      dst_nbytes;             /* Number of bytes to write to destination */
    size_t      src_start, dst_start;
    size_t      elmts_start;
    hsize_t     total_nbytes;           /* Total number of bytes to copy */
    hsize_t     total_src_nbytes;       /* Total number of bytes of the source */
    hsize_t     total_dst_nbytes;       /* Total number of bytes of the destination */
    size_t	target_size;		/* Desired buffer size	*/
    void       *buf = NULL;             /* Buffer for copying data */
    void       *bkg = NULL;             /* Temporary buffer for copying data */
    H5S_t      *buf_space = NULL;       /* Dataspace describing buffer */
    hsize_t     buf_dim;                /* Dimension for buffer */
    hbool_t     is_vlen = FALSE;        /* Flag to indicate VL type */
    hbool_t     vlen_conv = TRUE;       /* Transfer property to indicate no conversion for vlen */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_copy_conv, FAIL)

    /* Check args */
    HDassert(file);
    HDassert(dset_src && H5D_CONTIGUOUS == dset_src->shared->layout.type);
    HDassert(dset_dst && H5D_CONTIGUOUS == dset_dst->shared->layout.type);

    /* dataset pointers and IDs */
    dt_src = dset_src->shared->type;
    dt_dst = dset_dst->shared->type;
    tid_src = dset_src->shared->type_id;
    tid_dst = dset_dst->shared->type_id;

    /* Set up number of bytes to copy, and initial buffer size */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(dset_src->shared->space)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute element sizes and other parameters */
    src_dt_size = H5T_get_size(dt_src);
    dst_dt_size = H5T_get_size(dt_dst);
    max_dt_size = MAX(src_dt_size, dst_dt_size);

    total_src_nbytes = nelmts*src_dt_size;
    total_dst_nbytes = nelmts*dst_dt_size;
    total_nbytes = MAX(total_src_nbytes, total_dst_nbytes);
    target_size = MIN(H5D_TEMP_BUF_SIZE, (size_t)total_nbytes);

    /* XXX: This could cause a problem if the user sets their buffer size
     * to the same size as the default, and then the dataset elements are
     * too large for the buffer... - QAK
     */
    if(target_size == H5D_TEMP_BUF_SIZE) {
        /* If the buffer is too small to hold even one element, make it bigger */
        if(target_size<max_dt_size)
            target_size = max_dt_size;
        /* If the buffer is too large to hold all the elements, make it smaller */
        else if(target_size>(nelmts*max_dt_size))
            target_size=(size_t)(nelmts*max_dt_size);
    } /* end if */
    request_nelmts = target_size / max_dt_size;

    /* Sanity check elements in temporary buffer */
    if (request_nelmts==0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small")

    /* If the datatype is or contains vlen, set the property to indicate no conversion 
     * is needed. */
    if((is_vlen = H5T_detect_class(dt_src, H5T_VLEN)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to detect dtatypes")

    if(is_vlen ) {
        vlen_conv = FALSE;
        if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADATOM, FAIL, "can't find object for ID")

        if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "Error setting vlen conv flag")
    }

    /* Set up the conversion functions */
    if(NULL == (tpath = H5T_path_find(dt_src, dt_dst, NULL, NULL, dxpl_id, FALSE)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and mem datatypes")

    /* Set the number of bytes to transfer */
    src_nbytes = request_nelmts * src_dt_size;
    dst_nbytes = request_nelmts * dst_dt_size;

    /* Allocate space for copy buffer */
    HDassert(target_size);
    if(NULL == (buf = H5FL_BLK_MALLOC(type_conv, target_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for copy buffer")

    /* allocate temporary bkg buff for data conversion */
    if(NULL == (bkg = H5FL_BLK_MALLOC(type_conv, target_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for copy buffer")

    /* Allocate space for destination raw data */
    if(H5D_contig_create(file, dxpl_id, &(dset_dst->shared->layout)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to allocate contiguous storage")

    /* Loop over copying data */
    addr_src = dset_src->shared->layout.u.contig.addr;
    addr_dst = dset_dst->shared->layout.u.contig.addr;

    for (src_start=0, dst_start=0, elmts_start=0; src_start<total_src_nbytes; src_start+=src_nbytes, dst_start+=dst_nbytes, elmts_start+=request_nelmts) {
        src_nbytes = MIN(src_nbytes, (total_src_nbytes-src_start));
        dst_nbytes = MIN(dst_nbytes, (total_dst_nbytes-dst_start));
        request_nelmts = MIN(request_nelmts, (nelmts-elmts_start));

        /* Read raw data from source file */
        if(H5F_block_read(file, H5FD_MEM_DRAW, addr_src, src_nbytes, H5P_DATASET_XFER_DEFAULT, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "unable to read raw data")

        /* Set background buffer to all zeros */
        HDmemset(bkg, 0, target_size);

        /* Convert from memory to destination file */
	if(H5T_convert(tpath, tid_src, tid_dst, request_nelmts, (size_t)0, (size_t)0, buf, bkg, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

        /* Write raw data to destination file */
        if(H5F_block_write(file, H5FD_MEM_DRAW, addr_dst, dst_nbytes, H5P_DATASET_XFER_DEFAULT, buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to write raw data")

        /* Adjust loop variables */
        addr_src += src_nbytes;
        addr_dst += dst_nbytes;
    } /* end while */

    /* Set the property of vlen conversion back to normal */
    if(is_vlen ) {
        vlen_conv = TRUE;

        if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "Error setting vlen conv flag")
    }

done:
    if(buf)
        H5FL_BLK_FREE(type_conv, buf);
    if(bkg)
        H5FL_BLK_FREE(type_conv, bkg);
    if(is_vlen && buf_space) {
        if(H5S_close(buf_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't free data space")
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_contig_copy_conv() */
