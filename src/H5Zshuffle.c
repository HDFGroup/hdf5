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
#include <stdio.h>

#ifdef H5_HAVE_FILTER_SHUFFLE

size_t H5Z_filter_shuffle(unsigned flags,
			  size_t cd_nelmts,
			  const unsigned cd_values[],
			  size_t nbytes,
			  size_t *buf_size,void **buf) {
  
  size_t i;
  size_t j;
  size_t k;
  size_t ret_value = 0;
  size_t byte_pos;
  size_t bytesoftype;
  void* dest = NULL;
  char* _src;
  char* _des;
  char* _dest;
  size_t numofelements;
   
  bytesoftype=cd_values[0];
  numofelements=nbytes/bytesoftype;
  _src =(char*)(*buf);

  dest = malloc((size_t)nbytes);
  _dest =(char*)dest;

  j = 0;
  k = 0;
  if(flags & H5Z_FLAG_REVERSE) {
     for(i=0;i<nbytes;i++) {
       byte_pos = 1 +j *numofelements; 
       if(byte_pos > nbytes) {
	 k++;
	 j=0;
	 byte_pos=1;
       }
       *(_dest+i)=*((char*)(_src+byte_pos-1+k));
       j++;
     }
     free(*buf);
     *buf = dest;
     dest = NULL;
     *buf_size=nbytes;
     ret_value = nbytes;
  }

  else {
    for (i=0;i<nbytes;i++){
      byte_pos = 1+j * bytesoftype; 
      if(byte_pos >nbytes) {
	k++;
	j=0;
	byte_pos = 1;
      }

      *(_dest+i)=*(_src+byte_pos-1+k);
      j++;
    }
    free(*buf);
     *buf = dest;
     dest = NULL;
     *buf_size=nbytes;
     ret_value = nbytes;
  }

}
#endif /*H5_HAVE_FILTER_SHUFFLE */
