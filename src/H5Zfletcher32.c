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

/*
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Jan 3, 2003
 */
#include "H5private.h"
#include "H5Eprivate.h"
#include "H5MMprivate.h"
#include "H5Zprivate.h"

#ifdef H5_HAVE_FILTER_FLETCHER32

#define FLETCHER_LEN       4

/* Interface initialization */
#define PABLO_MASK	H5Z_fletcher32_mask
#define INTERFACE_INIT	NULL
static int interface_initialize_g = 0;


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_fletcher32_compute
 *
 * Purpose:	Implement an Fletcher32 Checksum using 1's complement.
 *
 * Return:	Success: Fletcher32 value	
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
static unsigned int H5Z_filter_fletcher32_compute(unsigned short *buf, size_t len)
{
    size_t         count = len;
    register unsigned int s1 = 0;
    register unsigned int s2 = 0;
    unsigned short *src = buf;

    FUNC_ENTER_NOINIT(H5Z_filter_fletcher32_compute);

    /* Compute checksum */
    while(count > 1) {
        s1 += *src++;
        if(s1 & 0xFFFF0000) { /*Wrap around carry if occurred*/
            s1 &= 0xFFFF;
            s1++;
        }
        s2 += s1;
        if(s2 & 0xFFFF0000) { /*Wrap around carry if occurred*/
            s2 &= 0xFFFF;
            s2++;
        }
        count -= 2;
    }

    if(count==1) {
        s1 += *(unsigned char*)src;
        if(s1 & 0xFFFF0000) { /*Wrap around carry if occurred*/
            s1 &= 0xFFFF;
            s1++;
        }
        s2 += s1;
        if(s2 & 0xFFFF0000) { /*Wrap around carry if occurred*/
            s2 &= 0xFFFF;
            s2++;
        }
    }

    FUNC_LEAVE_NOAPI((s2 << 16) + s1);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_fletcher32
 *
 * Purpose:	Implement an I/O filter of Fletcher32 Checksum
 *
 * Return:	Success: size of data plus the size of Fletcher32 value	
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
H5Z_filter_fletcher32 (unsigned flags, size_t UNUSED cd_nelmts, const unsigned UNUSED cd_values[], 
                     size_t nbytes, size_t *buf_size, void **buf)
{
    size_t	ret_value = 0;
    void	*outbuf = NULL;
    
    unsigned char *src = (unsigned char*)(*buf);
    unsigned int fletcher = 0;
    
    FUNC_ENTER_NOAPI(H5Z_filter_fletcher32, 0);

    assert(sizeof(unsigned int)==4);
   
    if (flags & H5Z_FLAG_REVERSE) { /* Read */
        size_t  src_nbytes = nbytes;
        unsigned int origin_fletcher;

        /* Do checksum if it's enabled for read; otherwise skip it
         * to save performance. */
        if (!(flags & H5Z_FLAG_SKIP_EDC)) { /* Read */
            unsigned char *tmp_src;

            src_nbytes -= FLETCHER_LEN;
            tmp_src=src+src_nbytes;
            UINT32DECODE(tmp_src, origin_fletcher);

            /* Compute checksum */
            fletcher = H5Z_filter_fletcher32_compute((unsigned short*)src,src_nbytes);

            if(origin_fletcher != fletcher)
	        HGOTO_ERROR(H5E_STORAGE, H5E_READERROR, 0, "data error detected by Fletcher32 checksum");
        }
        
        *buf_size = nbytes - FLETCHER_LEN;
        ret_value = *buf_size;
    } else { /* Write */
        unsigned char *dst;

        /* Compute checksum */
        fletcher = H5Z_filter_fletcher32_compute((unsigned short*)src,nbytes);
        
	if (NULL==(dst=outbuf=H5MM_malloc(nbytes+FLETCHER_LEN)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate Fletcher32 checksum destination buffer");
        
        /* Copy raw data */
        HDmemcpy((void*)dst, (void*)(*buf), nbytes);

        /* Append checksum to raw data */
        dst += nbytes;
        UINT32ENCODE(dst, fletcher);

        *buf_size = nbytes + FLETCHER_LEN;
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

#endif /* H5_HAVE_FILTER_FLETCHER32 */
