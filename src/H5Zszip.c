/*
 * Copyright © 1999-2001 NCSA
 *                       All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Friday, August 27, 1999
 */
#include "H5private.h"
#include "H5Eprivate.h"
#include "H5MMprivate.h"
#include "H5Zprivate.h"

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
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Z_filter_szip (unsigned flags, 
		 size_t cd_nelmts,
		 const unsigned cd_values[], 
		 size_t nbytes,
		 size_t *buf_size, 
		 void **buf)
{

  size_t ret_value = 0;
  size_t size_out  = 0; 
  size_t size_in   = 0; 
  char *outbuf     = NULL;
  char *newbuf     = NULL;
  int status;
  SZ_com_t sz_param; 

  sz_param.options_mask        = cd_values[0];
  sz_param.bits_per_pixel      = cd_values[1];
  sz_param.pixels_per_block    = cd_values[2];
  sz_param.pixels_per_scanline = cd_values[3];

  FUNC_ENTER_NOAPI(H5Z_filter_szip, 0);

  if (flags & H5Z_FLAG_REVERSE) {
    /* Input; uncompress */

    size_t nalloc;

    newbuf = *buf;
    UINT32DECODE(newbuf,nalloc);

    size_out = nalloc;
    size_in = nbytes;

    if(NULL==(outbuf = H5MM_malloc(nalloc)))
      HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed for szip decompression");

     status = SZ_BufftoBuffDecompress(outbuf, &size_out, newbuf, size_in-4, &sz_param);

     if(status != SZ_OK) {
      HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "szip_filter: decompression failed");
    }

      *buf = newbuf -4;
      H5MM_xfree(*buf);
      *buf = outbuf;
      outbuf = NULL;
      *buf_size = nalloc;
      ret_value = size_out;
  }

    else {

      size_in = nbytes;
      size_out = nbytes;
      if(NULL==(outbuf = H5MM_malloc(size_out+4)))
	 HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate szip destination buffer");

      UINT32ENCODE(outbuf,nbytes);
      status = SZ_BufftoBuffCompress(outbuf, &size_out, *buf, size_in, &sz_param);
      if(SZ_OK!=status)
	HGOTO_ERROR (H5E_PLINE, H5E_CANTINIT, 0, "szip_filter: Compression failed"); 

      if(*buf) H5MM_xfree(*buf);
      *buf = outbuf-4;
      outbuf = NULL;
      *buf_size = size_out+4;
      ret_value = size_out+4;
    }

  done: 
    if(outbuf) H5MM_xfree(outbuf);
    FUNC_LEAVE_NOAPI(ret_value);
  }
  
#endif


  
