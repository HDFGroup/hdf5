/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*+
   FILE
       H5Dconv.c
   HDF5 trivial datatype converion routines

   EXPORTED ROUTINES

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
   + */

#include <H5private.h>  /* Generic Functions */
#include <H5Dprivate.h> /* Dataset functions */
#include <H5Eprivate.h> /* Error handling */

#define PABLO_MASK	H5D_mask

/*--------------------- Locally scoped variables -----------------------------*/

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL

/*--------------------------------------------------------------------------
 NAME
    H5D_convert_buf
 PURPOSE
    Byte-Swap a buffer of data
 USAGE
    herr_t H5D_convert_buf(dst, src, len, size)
        VOIDP dst;         OUT: Buffer to fill with converted data
        VOIDP src;         IN: Buffer to converted data from
        uintn len;         IN: Number of bytes to convert
        uintn size;        IN: Size of quantity to byte-swap
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function is a byte-swapping memcpy.
--------------------------------------------------------------------------*/
herr_t H5D_convert_buf(void *dst, const void *src, uintn len, uintn size)
{
    const char *s=(const char *)src;
    char *d=(char *)dst;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER(H5D_convert_buf, FAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    assert(dst);
    assert(src);
    assert(len>0);
    assert(size==8 || size==4 || size==2 || size==1);

    switch(size)
      {
          case 1:   /* straight memcpy() */
              HDmemcpy(d,s,len);
              break;

          case 2:   /* 2-byte swapping */
              while(len>0)
                {
                    *d++=*(s+1);
                    *d++=*s;
                    s+=2;
                    len-=2;
                } /* end while */
              break;

          case 4:   /* 4-byte swapping */
              while(len>0)
                {
                    *d++=*(s+3);
                    *d++=*(s+2);
                    *d++=*(s+1);
                    *d++=*s;
                    s+=4;
                    len-=4;
                } /* end while */
              break;

          case 8:   /* 8-byte swapping */
              while(len>0)
                {
                    *d++=*(s+7);
                    *d++=*(s+6);
                    *d++=*(s+5);
                    *d++=*(s+4);
                    *d++=*(s+3);
                    *d++=*(s+2);
                    *d++=*(s+1);
                    *d++=*s;
                    s+=8;
                    len-=8;
                } /* end while */
              break;

          default:
              HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL,
			  "not implemented yet");
      } /* end switch */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
} /* end H5D_convert_buf() */
