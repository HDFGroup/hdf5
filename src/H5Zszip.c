/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Zprivate.h"		/* Data filters				*/

#ifdef H5_HAVE_FILTER_SZIP

#ifdef H5_HAVE_SZLIB_H
#   include "szlib.h"
#endif

/* Interface initialization */
#define PABLO_MASK	H5Z_szip_mask
#define INTERFACE_INIT	NULL
static int interface_initialize_g = 0;


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_szip
 *
 * Purpose:	Implement an I/O filter around the 'rice' algorithm in
 *              libsz
 *
 * Return:	Success: Size of buffer filtered
 *		Failure: 0	
 *
 * Programmer:	Kent Yang
 *              Tuesday, April 1, 2003
 *
 * Modifications:
 *              Quincey Koziol, April 2, 2003
 *              Cleaned up code.
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Z_filter_szip (unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
    size_t nbytes, size_t *buf_size, void **buf)
{
    size_t ret_value = 0;       /* Return value */
    size_t size_out  = 0;       /* Size of output buffer */
    unsigned char *outbuf = NULL;    /* Pointer to new output buffer */
    unsigned char *newbuf = NULL;    /* Pointer to input buffer */
    SZ_com_t sz_param;          /* szip parameter block */

    FUNC_ENTER_NOAPI(H5Z_filter_szip, 0);

    /* Sanity check to make certain that we haven't drifted out of date with
     * the mask options from the szlib.h header */
    assert(H5_SZIP_RAW_OPTION_MASK==SZ_RAW_OPTION_MASK);
    assert(H5_SZIP_NN_OPTION_MASK==SZ_NN_OPTION_MASK);

    /* Check arguments */
    if (cd_nelmts!=4)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid deflate aggression level");

    /* Copy the filter parameters into the szip parameter block */
    sz_param.options_mask        = cd_values[0];
    sz_param.bits_per_pixel      = cd_values[1];
    sz_param.pixels_per_block    = cd_values[2];
    sz_param.pixels_per_scanline = cd_values[3];

    /* Input; uncompress */
    if (flags & H5Z_FLAG_REVERSE) {
        uint32_t stored_nalloc;  /* Number of bytes the compressed block will expand into */
        size_t nalloc;  /* Number of bytes the compressed block will expand into */

        /* Get the size of the uncompressed buffer */
        newbuf = *buf;
        UINT32DECODE(newbuf,stored_nalloc);
        H5_ASSIGN_OVERFLOW(nalloc,stored_nalloc,uint32_t,size_t);
        
        /* Check for uncompressed buffer */
        if(nalloc==0) {
            /* Set the correct number of bytes to allocate */
            nalloc=nbytes-4;

            /* Allocate space for the uncompressed buffer */
            if(NULL==(outbuf = H5MM_malloc(nalloc)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for szip decompression");

            /* Copy over the uncompressed data */
            HDmemcpy((void*)outbuf, (void*)newbuf, nalloc);
        } /* end if */
        else {
            /* Allocate space for the uncompressed buffer */
            if(NULL==(outbuf = H5MM_malloc(nalloc)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for szip decompression");

            /* Decompress the buffer */
            size_out=nalloc;
            if(SZ_BufftoBuffDecompress(outbuf, &size_out, newbuf, nbytes-4, &sz_param) != SZ_OK)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "szip_filter: decompression failed");
            assert(size_out==nalloc);
        } /* end else */

        /* Free the input buffer */
        H5MM_xfree(*buf);

        /* Set return values */
        *buf = outbuf;
        outbuf = NULL;
        *buf_size = nalloc;
        ret_value = nalloc;
    }
    /* Output; compress */
    else {
        unsigned char *dst = NULL;    /* Temporary pointer to new output buffer */

        /* Allocate space for the compressed buffer (assume it won't get bigger) */
        if(NULL==(dst=outbuf = H5MM_malloc(nbytes+4)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate szip destination buffer");

        /* Compress the buffer */
        size_out = nbytes;
        if(SZ_OK!= SZ_BufftoBuffCompress(outbuf+4, &size_out, *buf, nbytes, &sz_param)) {
            /* In the event that an error occurs, assume that the buffer
             * could not be compressed and just copy the input buffer to the
             * proper location in the output buffer */
            /* (This is necessary for the szip filter due to the uncompressed
             * size needing to be encoded for the decompression side of things)
             */
            HDmemcpy((void*)(outbuf+4), (void*)(*buf), nbytes);

            /* Set correct output size (again) */
            size_out=nbytes;

            /* Reset the "nbytes" to encode, so that the decompression side knows that the buffer is uncompressed */
            nbytes=0;
        } /* end if */

        /* Encode the uncompressed length */
        H5_CHECK_OVERFLOW(nbytes,size_t,uint32_t);
        UINT32ENCODE(dst,nbytes);

        /* Free the input buffer */
        H5MM_xfree(*buf);

        /* Set return values */
        *buf = outbuf;
        outbuf = NULL;
        *buf_size = size_out+4;
        ret_value = size_out+4;
    }

done: 
    if(outbuf)
        H5MM_xfree(outbuf);
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif /* H5_HAVE_FILTER_SZIP */

