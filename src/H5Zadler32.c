/*
 * Copyright © 1999-2001 NCSA
 *                       All rights reserved.
 *
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Jan 3, 2003
 */
#include "H5private.h"
#include "H5Eprivate.h"
#include "H5MMprivate.h"
#include "H5Zprivate.h"

#ifdef H5_HAVE_FILTER_ADLER32

#define ADLER_LEN       4
#define ADLER_BASE      65521

/* Interface initialization */
#define PABLO_MASK	H5Z_adler32_mask
#define INTERFACE_INIT	NULL
static int interface_initialize_g = 0;


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_adler32_compute
 *
 * Purpose:	Implement an Adler32 Checksum
 *
 * Return:	Success: Adler32 value	
 *
 *		Failure: Can't fail
 *
 * Programmer:	Raymond Lu
 *              Jan 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static unsigned int H5Z_filter_adler32_compute(unsigned char *buf, size_t len)
{
    size_t         i;
    register unsigned int s1 = 1;
    register unsigned int s2 = 0;

    FUNC_ENTER_NOINIT(H5Z_filter_adler32_compute);

    /* Compute checksum */
    for(i=0; i<len; i++) {
        s1 = (s1 + *buf++) % ADLER_BASE;
        s2 = (s2 + s1) % ADLER_BASE;
    }

    FUNC_LEAVE_NOAPI((s2 << 16) + s1);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_adler32
 *
 * Purpose:	Implement an I/O filter of Adler32 Checksum
 *
 * Return:	Success: size of data plus the size of Adler32 value	
 *
 *		Failure: 0	
 *
 * Programmer:	Raymond Lu
 *              Jan 3, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Z_filter_adler32 (unsigned flags, size_t cd_nelmts, const unsigned cd_values[], 
                     size_t nbytes, size_t *buf_size, void **buf)
{
    size_t	ret_value = 0;
    void	*outbuf = NULL;
    
    unsigned char *src = (unsigned char*)(*buf);
    unsigned int adler = 1;
    
    FUNC_ENTER_NOAPI(H5Z_filter_adler32, 0);

    assert(sizeof(unsigned int)==4);
   
    if (flags & H5Z_FLAG_REVERSE) { /* Read */
        size_t  src_nbytes = nbytes;
        unsigned int origin_adler;

        /* Do checksum if it's enabled for read; otherwise skip it
         * to save performance. */
        if (!(flags & H5Z_FLAG_SKIP_EDC)) { /* Read */
            unsigned char *tmp_src;

            src_nbytes -= ADLER_LEN;
            tmp_src=src+src_nbytes;
            UINT32DECODE(tmp_src, origin_adler);

            /* Compute checksum */
            adler = H5Z_filter_adler32_compute(src,src_nbytes);

            if(origin_adler != adler)
	        HGOTO_ERROR(H5E_STORAGE, H5E_READERROR, 0, "data error detected by Adler32 checksum");
        }
        
        *buf_size = nbytes - ADLER_LEN;
        ret_value = *buf_size;
    } else { /* Write */
        unsigned char *dst;

        /* Compute checksum */
        adler = H5Z_filter_adler32_compute(src,nbytes);
        
	if (NULL==(dst=outbuf=H5MM_malloc(nbytes+ADLER_LEN)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate Adler32 checksum destination buffer");
        
        /* Copy raw data */
        HDmemcpy((void*)dst, (void*)(*buf), nbytes);

        /* Append checksum to raw data */
        dst += nbytes;
        UINT32ENCODE(dst, adler);

        *buf_size = nbytes + ADLER_LEN;
 	H5MM_xfree(*buf);
	*buf = outbuf;
	outbuf = NULL;
	ret_value = *buf_size;           
    }

done:
    if(outbuf)
        H5MM_xfree(outbuf);
    FUNC_LEAVE_NOAPI(ret_value);
}

#endif /* H5_HAVE_FILTER_ADLER32 */
