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


#include "h5diff.h"
#include "H5private.h" 
#include <assert.h>
#include <math.h>



/*-------------------------------------------------------------------------
 * Function: array_diff
 *
 * Purpose: compare array; 
 *   currenttly only the NATIVE types below are supported
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 30, 2003
 *
 *-------------------------------------------------------------------------
 */
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, int rank, hsize_t *dims, 
                options_t options, const char *obj1, const char *obj2,
                hid_t m_type )
{
 char        fmt_llong[255],  fmt_ullong[255];
 char        fmt_llongp[255], fmt_ullongp[255];
 size_t      type_size;/* just check */
 int         nfound=0; /* number of differences found */
 int         ph=1;     /* print header  */
 int         acc[32];  /* accumulator and matrix position */
 int         pos[32];
 unsigned    i; 
 int         j;
 char        *_buf1 = (char*)buf1;
 char        *_buf2 = (char*)buf2;
  
 
 /* Build default formats for long long types */
 sprintf(fmt_llong,  "%%%sd              %%%sd               %%%sd\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
 sprintf(fmt_ullong, "%%%su              %%%su               %%%su\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
 sprintf(fmt_llongp,  "%%%sd             %%%sd               %%%sd               %%%sd\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
 sprintf(fmt_ullongp, "%%%su             %%%su               %%%su               %%%su\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);


 acc[rank-1]=1;
 for(j=(rank-2); j>=0; j--)
 {
  acc[j]=acc[j+1]*(int)dims[j+1];
 }

 /* Get the size. */
 type_size = H5Tget_size( m_type );

/*-------------------------------------------------------------------------
 * H5T_NATIVE_SCHAR
 *-------------------------------------------------------------------------
 */
  if (H5Tequal(m_type, H5T_NATIVE_SCHAR)) 
  {
   char        temp1_char;
   char        temp2_char;
   assert(type_size==sizeof(char));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_char, _buf1, sizeof(char));
    memcpy(&temp2_char, _buf2, sizeof(char));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_char-temp2_char) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char),
        abs(1-temp2_char/temp1_char));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options.percent && 
      abs(temp1_char-temp2_char) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char),
        abs(1-temp2_char/temp1_char));
      }
      nfound++;
     }
    }
    else if (temp1_char != temp2_char)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
     }
     nfound++;
    }
    
    _buf1+=sizeof(char);
    _buf2+=sizeof(char);
   }/* i */
   
  } /*H5T_NATIVE_SCHAR*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_UCHAR
 *-------------------------------------------------------------------------
 */
  else if (H5Tequal(m_type, H5T_NATIVE_UCHAR)) 
  {
   unsigned char      temp1_uchar;
   unsigned char      temp2_uchar;
   assert(type_size==sizeof(unsigned char));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_uchar, _buf1, sizeof(unsigned char));
    memcpy(&temp2_uchar, _buf2, sizeof(unsigned char));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_uchar-temp2_uchar) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar),
        abs(1-temp2_uchar/temp1_uchar));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options.percent && 
      abs(temp1_uchar-temp2_uchar) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar),
        abs(1-temp2_uchar/temp1_uchar));
      }
      nfound++;
     }
    }
    else if (temp1_uchar != temp2_uchar)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
     }
     nfound++;
    }
    
    _buf1+=sizeof(unsigned char);
    _buf2+=sizeof(unsigned char);
   }/* i */
   
  } /*H5T_NATIVE_UCHAR*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_SHORT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_SHORT))
  {
   short       temp1_short;
   short       temp2_short;
   assert(type_size==sizeof(short));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_short, _buf1, sizeof(short));
    memcpy(&temp2_short, _buf2, sizeof(short));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_short-temp2_short) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),
        abs(1-temp2_short/temp1_short));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options.percent && 
      abs(temp1_short-temp2_short) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),
        abs(1-temp2_short/temp1_short));
      }
      nfound++;
     }
    }
    else if (temp1_short != temp2_short)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
     }
     nfound++;
    }
    
    _buf1+=sizeof(short);
    _buf2+=sizeof(short);
   }/* i */
   
  } /*H5T_NATIVE_SHORT*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_USHORT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_USHORT))
  {
   unsigned short       temp1_ushort;
   unsigned short       temp2_ushort;
   assert(type_size==sizeof(short));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_ushort, _buf1, sizeof(unsigned short));
    memcpy(&temp2_ushort, _buf2, sizeof(unsigned short));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_ushort-temp2_ushort) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort),
        abs(1-temp2_ushort/temp1_ushort));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options.percent && 
      abs(temp1_ushort-temp2_ushort) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort),
        abs(1-temp2_ushort/temp1_ushort));
      }
      nfound++;
     }
    }
    else if (temp1_ushort != temp2_ushort)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
     }
     nfound++;
    }
    
    _buf1+=sizeof(unsigned short);
    _buf2+=sizeof(unsigned short);
   }/* i */
   
  } /*H5T_NATIVE_USHORT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_INT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_INT)) 
  {
   int         temp1_int;
   int         temp2_int;
   assert(type_size==sizeof(int));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_int, _buf1, sizeof(int));
    memcpy(&temp2_int, _buf2, sizeof(int));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_int-temp2_int) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),
        abs(1-temp2_int/temp1_int));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options.percent && 
      abs(temp1_int-temp2_int) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),
        abs(1-temp2_int/temp1_int));
      }
      nfound++;
     }
    }
    else if (temp1_int != temp2_int)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
     }
     nfound++;
    }
    
    _buf1+=sizeof(int);
    _buf2+=sizeof(int);
   }/* i */
   
  } /*H5T_NATIVE_INT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_UINT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_UINT)) 
  {
   unsigned int         temp1_uint;
   unsigned int         temp2_uint;
   assert(type_size==sizeof(int));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_uint, _buf1, sizeof(unsigned int));
    memcpy(&temp2_uint, _buf2, sizeof(unsigned int));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs((int)(temp1_uint-temp2_uint)) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(UIFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_uint!=0 && abs((int)(1-temp2_uint/temp1_uint)) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(UIPFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)),
        abs((int)(1-temp2_uint/temp1_uint)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_uint!=0 && abs((int)(1-temp2_uint/temp1_uint)) > options.percent && 
      abs((int)(temp1_uint-temp2_uint)) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(UIPFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)),
        abs((int)(1-temp2_uint/temp1_uint)));
      }
      nfound++;
     }
    }
    else if (temp1_uint != temp2_uint)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(UIFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)));
     }
     nfound++;
    }
    
    _buf1+=sizeof(unsigned int);
    _buf2+=sizeof(unsigned int);
   }/* i */
   
  } /*H5T_NATIVE_UINT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_LONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_LONG)) 
  {
   long        temp1_long;
   long        temp2_long;
   assert(type_size==sizeof(long));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_long, _buf1, sizeof(long));
    memcpy(&temp2_long, _buf2, sizeof(long));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (labs(temp1_long-temp2_long) > (long)options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long),
        labs(1-temp2_long/temp1_long));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options.percent && 
      labs(temp1_long-temp2_long) > (long)options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long),
        labs(1-temp2_long/temp1_long));
      }
      nfound++;
     }
    }
    else if (temp1_long != temp2_long)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
     }
     nfound++;
    }
    
    _buf1+=sizeof(long);
    _buf2+=sizeof(long);
   }/* i */
   
  } /*H5T_NATIVE_LONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_ULONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_ULONG)) 
  {
   unsigned long        temp1_ulong;
   unsigned long        temp2_ulong;
   assert(type_size==sizeof(unsigned long));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_ulong, _buf1, sizeof(unsigned long));
    memcpy(&temp2_ulong, _buf2, sizeof(unsigned long));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (labs((long)(temp1_ulong-temp2_ulong)) > (long)options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(ULIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_ulong!=0 && labs((long)(1-temp2_ulong/temp1_ulong)) > (long)options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(ULPIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)),
        labs((long)(1-temp2_ulong/temp1_ulong)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_ulong!=0 && labs((long)(1-temp2_ulong/temp1_ulong)) > (long)options.percent && 
      labs((long)(temp1_ulong-temp2_ulong)) > (long)options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(ULPIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)),
        labs((long)(1-temp2_ulong/temp1_ulong)));
      }
      nfound++;
     }
    }
    else if (temp1_ulong != temp2_ulong)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(ULIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)));
     }
     nfound++;
    }
    
    _buf1+=sizeof(unsigned long);
    _buf2+=sizeof(unsigned long);
   }/* i */
   
  } /*H5T_NATIVE_ULONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_LLONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_LLONG)) 
  {
   long_long        temp1_llong;
   long_long        temp2_llong;
   assert(type_size==sizeof(long_long));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_llong, _buf1, sizeof(long_long));
    memcpy(&temp2_llong, _buf2, sizeof(long_long));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (labs((long)(temp1_llong-temp2_llong)) > (long)options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_llong,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > (long)options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_llongp,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)),
        (long_long)labs((long)(1-temp2_llong/temp1_llong)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > (long)options.percent && 
      labs((long)(temp1_llong-temp2_llong)) > (long)options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_llongp,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)),
        (long_long)labs((long)(1-temp2_llong/temp1_llong)));
      }
      nfound++;
     }
    }
    else if (temp1_llong != temp2_llong)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(fmt_llong,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)));
     }
     nfound++;
    }
    
    _buf1+=sizeof(long_long);
    _buf2+=sizeof(long_long);
   }/* i */
   
  } /*H5T_NATIVE_LLONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_ULLONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_ULLONG)) 
  {
   unsigned long_long        temp1_ullong;
   unsigned long_long        temp2_ullong;
   assert(type_size==sizeof(unsigned long_long));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_ullong, _buf1, sizeof(unsigned long_long));
    memcpy(&temp2_ullong, _buf2, sizeof(unsigned long_long));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (labs((long)(temp1_ullong-temp2_ullong)) > (long)options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_ullong,temp1_ullong,temp2_ullong,
        (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > (long)options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_ullongp,temp1_ullong,temp2_ullong,
        (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)),
        (unsigned long_long)labs((long)(1-temp2_ullong/temp1_ullong)));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > (long)options.percent && 
      labs((long)(temp1_ullong-temp2_ullong)) > (long)options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_ullongp,temp1_ullong,temp2_ullong,
        (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)),
        (unsigned long_long)labs((long)(1-temp2_ullong/temp1_ullong)));
      }
      nfound++;
     }
    }
    else if (temp1_ullong != temp2_ullong)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(fmt_ullong,temp1_ullong,temp2_ullong,
       (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)));
     }
     nfound++;
    }
    
    _buf1+=sizeof(unsigned long_long);
    _buf2+=sizeof(unsigned long_long);
   }/* i */
   
  } /*H5T_NATIVE_ULLONG*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_FLOAT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_FLOAT)) 
  {
   float       temp1_float;
   float       temp2_float;
   assert(type_size==sizeof(float));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_float, _buf1, sizeof(float));
    memcpy(&temp2_float, _buf2, sizeof(float));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (fabs(temp1_float-temp2_float) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float),
        fabs(1-temp2_float/temp1_float));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options.percent && 
      fabs(temp1_float-temp2_float) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float),
        fabs(1-temp2_float/temp1_float));
      }
      nfound++;
     }
    }
    else if (temp1_float != temp2_float)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
     }
     nfound++;
    }
    
    _buf1+=sizeof(float);
    _buf2+=sizeof(float);
   }/* i */
   
  } /*H5T_NATIVE_FLOAT*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_DOUBLE
 *-------------------------------------------------------------------------
 */

  else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE)) 
  {
   double      temp1_double;
   double      temp2_double;
   assert(type_size==sizeof(double));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_double, _buf1, sizeof(double));
    memcpy(&temp2_double, _buf2, sizeof(double));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (fabs(temp1_double-temp2_double) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double),
        fabs(1-temp2_double/temp1_double));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options.percent && 
      fabs(temp1_double-temp2_double) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double),
        fabs(1-temp2_double/temp1_double));
      }
      nfound++;
     }
    }
    else if (temp1_double != temp2_double)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
     }
     nfound++;
    }
    
    _buf1+=sizeof(double);
    _buf2+=sizeof(double);
   }/* i */
   
  } /*H5T_NATIVE_DOUBLE*/

/*-------------------------------------------------------------------------
 * no more
 *-------------------------------------------------------------------------
 */
   else 
   {
    assert(0); 
   }
   
 return nfound;
}


