/*
 * Copyright (C) 2000-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu> 
 *              August 5, 2002 
 *
 * Purpose:     Compact dataset I/O functions.  These routines are similar
 *              H5F_contig_* and H5F_istore_*.
 */

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "H5private.h"
#include "H5Eprivate.h"
#include "H5Fpkg.h"
#include "H5Oprivate.h"
#include "H5FDprivate.h"        /*file driver                             */
#include "H5FLprivate.h"        /*Free Lists                              */

/* Interface initialization */
#define PABLO_MASK      H5Fcompact_mask
static int              interface_initialize_g = 0;
#define INTERFACE_INIT NULL

/*-------------------------------------------------------------------------
 * Function:    H5F_compact_readv
 *
 * Purpose:     Reads some data vectors from a dataset into a buffer.
 *              The data is in compact dataset.  The address is relative 
 *              to the beginning address of the dataset.  The offsets and
 *              sequence lengths are in bytes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu 
 *              August 5, 2002 
 *
 * Notes:
 *              Offsets in the sequences must be monotonically increasing
 * 
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_compact_readv(H5F_t UNUSED *f, const H5O_layout_t *layout, size_t nseq, 
                  size_t size_arr[], hsize_t offset_arr[], 
                  hid_t UNUSED dxpl_id, void *_buf/*out*/)
{
    unsigned char       *buf=(unsigned char *)_buf;
    size_t              size;
    haddr_t             offset;
    unsigned            u;
    herr_t              ret_value=SUCCEED;
    
    FUNC_ENTER_NOAPI(H5F_compact_readv, FAIL);

    for(u=0; u<nseq; u++) {
        size=size_arr[u];
        offset=offset_arr[u];
        if(size > 0) {
            HDmemcpy(buf, (unsigned char*)layout->buf+offset, size); 
            buf +=size;
        }
    }

done:
    FUNC_LEAVE(ret_value);
}   /* end H5F_compact_readv() */

/*-------------------------------------------------------------------------
 * Function:    H5F_compact_writev
 * 
 * Purpose:     Writes some data vectors from a dataset into a buffer.
 *              The data is in compact dataset.  The address is relative 
 *              to the beginning address for the file.  The offsets and
 *              sequence lengths are in bytes.  This function only copies
 *              data into the buffer in the LAYOUT struct and mark it 
 *              as DIRTY.  Later in H5D_close, the data is copied into 
 *              header message in memory.  
 *              
 * Return:      Non-negative on success/Negative on failure
 * 
 * Programmer:  Raymond Lu
 *              August 5, 2002
 *              
 * Notes:       
 *              Offsets in the sequences must be monotonically increasing
 *              
 * Modifications:
 * 
 *-------------------------------------------------------------------------
 */
herr_t
H5F_compact_writev(H5F_t UNUSED *f, H5O_layout_t *layout, size_t nseq,
                  size_t size_arr[], hsize_t offset_arr[], 
                  hid_t UNUSED dxpl_id, const void *_buf)
{
    const unsigned char       *buf=(const unsigned char *)_buf;
    size_t              size;
    haddr_t             offset;
    unsigned            u;
    herr_t              ret_value=SUCCEED;
    
    FUNC_ENTER_NOAPI(H5F_compact_writev, FAIL);

    for(u=0; u<nseq; u++) {
        size=size_arr[u];
        offset=offset_arr[u];
        if(size > 0) {
            HDmemcpy((unsigned char*)layout->buf+offset, buf, size);
            buf += size;
        }
    }

    layout->dirty = TRUE;

done:   
    FUNC_LEAVE(ret_value);
}   /* end H5F_compact_writev */
