/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             H5MF.c
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             File memory management functions.
 *
 * Modifications:
 *
 *      Robb Matzke, 5 Aug 1997
 *      Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>

#define PABLO_MASK      H5MF_mask

/* Is the interface initialized? */
static intn             interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL

/*-------------------------------------------------------------------------
 * Function:    H5MF_alloc
 *
 * Purpose:     Allocate at least SIZE bytes of file memory and return
 *              the address where that contiguous chunk of file memory
 *              exists. The allocation operation should be either H5MF_META or
 *              H5MF_RAW depending on the purpose for which the storage is
 *              being requested.
 *
 * Return:      Success:        SUCCEED.  The file address of new chunk is
 *                              returned through the ADDR argument.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_alloc(H5F_t *f, intn op, hsize_t size, haddr_t *addr/*out*/)
{
    haddr_t                 tmp_addr;

    FUNC_ENTER(H5MF_alloc, FAIL);

    /* check arguments */
    assert(f);
    assert(H5MF_META == op || H5MF_RAW == op);
    assert(size > 0);
    assert(addr);

    /* Fail if we don't have write access */
    if (0==(f->intent & H5F_ACC_RDWR)) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_CANTINIT, FAIL, "file is read-only");
    }

    /*
     * Eventually we'll maintain a free list(s) and try to satisfy requests
     * from there.  But for now we just allocate more memory from the end of
     * the file.
     */
    if (H5F_low_extend(f->shared->lf, &(f->shared->access_parms), op,
		       size, addr/*out*/) < 0) {
        HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                      "low level mem management failed");
    }
    /* Convert from absolute to relative */
    addr->offset -= f->shared->base_addr.offset;

    /* Did we extend the size of the hdf5 data? */
    tmp_addr = *addr;
    H5F_addr_inc(&tmp_addr, size);
    if (H5F_addr_gt(&tmp_addr, &(f->shared->hdf5_eof))) {
        f->shared->hdf5_eof = tmp_addr;
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5MF_free
 *
 * Purpose:     Frees part of a file, making that part of the file
 *              available for reuse.
 *
 * Note:        This version of the function doesn't do anything.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 17 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_free(H5F_t __unused__ *f, const haddr_t *addr, hsize_t size)
{
    FUNC_ENTER(H5MF_free, FAIL);

    /* check arguments */
    assert(f);
    if (!addr || !H5F_addr_defined(addr) || 0 == size)
        HRETURN(SUCCEED);
    assert(!H5F_addr_zerop(addr));

#ifdef H5MF_DEBUG
    fprintf(stderr, "H5MF_free: lost %lu bytes of file storage\n",
            (unsigned long) size);
#endif

    FUNC_LEAVE(SUCCEED);
}
