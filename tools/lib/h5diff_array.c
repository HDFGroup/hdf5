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

static 
int diff_array_mem( void       *_mem1, 
                    void       *_mem2, 
                    hid_t      m_type,
                    unsigned   i, 
                    int        rank, 
                    int        *acc,  
                    int        *pos, 
                    diff_opt_t *options, 
                    const char *obj1, 
                    const char *obj2);

static
int diff_native_uchar(unsigned char *mem1,
                      unsigned char *mem2,
                      size_t        type_size,
                      unsigned      i, 
                      int           rank, 
                      int           *acc,  
                      int           *pos, 
                      diff_opt_t    *options, 
                      const char    *obj1, 
                      const char    *obj2,
                      int           ph);


/*-------------------------------------------------------------------------
 * Function: diff_array
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
 * Modifications: October 30, 2003
 *  Added support for H5T_COMPOUND types; to handle compounds a recursive
 *  function is called. because of this , the data is compared datum by datum
 *  instead of the previous cycle that compared all the array  the native 
 *  types 
 * Added support for  
 *  H5T_STRING                          
 *  H5T_BITFIELD                          
 *  H5T_OPAQUE           
 *  H5T_ENUM		 
 *  H5T_VLEN		
 *  H5T_ARRAY	                 
 *
 *-------------------------------------------------------------------------
 */

int diff_array( void *_mem1, 
                void *_mem2, 
                hsize_t nelmts, 
                int rank, 
                hsize_t *dims, 
                diff_opt_t *options, 
                const char *name1, 
                const char *name2,
                hid_t m_type )
{ 
 int           nfound=0;          /* number of differences found */
 size_t        size;              /* size of datum */
 unsigned char *mem1 = (unsigned char*)_mem1;
 unsigned char *mem2 = (unsigned char*)_mem2;
 unsigned char *tmp1;
 unsigned char *tmp2;
 int           acc[32];           /* accumulator and matrix position */
 int           pos[32];
 unsigned      i;
 int           j;

 acc[rank-1]=1;
 for(j=(rank-2); j>=0; j--)
 {
  acc[j]=acc[j+1]*(int)dims[j+1];
 }


 if(H5Tis_variable_str(m_type)) 
 {
  tmp1 = ((unsigned char**)mem1)[0]; 
  tmp2 = ((unsigned char**)mem2)[0]; 
		nfound+=diff_array_mem(
   tmp1,
   tmp2, 
   m_type,
   0,
   rank,
   acc,
   pos,
   options,
   name1,
   name2);
 }

 else

 {
  
  /* get the size. */
  size = H5Tget_size( m_type );
  
  for ( i = 0; i < nelmts; i++)
  {
   nfound+=diff_array_mem(
    mem1 + i * size,
    mem2 + i * size, /* offset */
    m_type,
    i,
    rank,
    acc,
    pos,
    options,
    name1,
    name2);
   if (options->n && nfound>=options->count)
    return nfound;
  }
 }
  
 return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_array_mem
 *
 * Purpose: Compare the values pointed to in _MEM1 and _MEM2 of type M_TYPE
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 29, 2003
 *
 *-------------------------------------------------------------------------
 */

static 
int diff_array_mem( void       *_mem1, 
                    void       *_mem2, 
                    hid_t      m_type,
                    unsigned   i, 
                    int        rank, 
                    int        *acc,  
                    int        *pos, 
                    diff_opt_t *options, 
                    const char *obj1, 
                    const char *obj2)
{
 char          fmt_llong[255],  fmt_ullong[255];
 char          fmt_llongp[255], fmt_ullongp[255];
 size_t        type_size; 
 static int    ph=1;      /* print header  */
 int           nfound=0;  /* differences found */
 unsigned char *mem1 = (unsigned char*)_mem1;
 unsigned char *mem2 = (unsigned char*)_mem2;
 unsigned      u;
 hid_t         memb_type;
 size_t        offset;
 int           nmembs;
 int           j;
 hsize_t       dims[H5S_MAX_RANK];
 hsize_t       nelmts;
 hsize_t       ndims;
 size_t        size;


 /* Build default formats for long long types */
 sprintf(fmt_llong,  "%%%sd              %%%sd               %%%sd\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
 sprintf(fmt_ullong, "%%%su              %%%su               %%%su\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
 sprintf(fmt_llongp,  "%%%sd             %%%sd               %%%sd               %%%sd\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);
 sprintf(fmt_ullongp, "%%%su             %%%su               %%%su               %%%su\n", 
  H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH, H5_PRINTF_LL_WIDTH);


 /* Get the size. */
 type_size = H5Tget_size( m_type );


 switch (H5Tget_class(m_type)) 
 {
 default:
  assert(0);
  break;
 case H5T_TIME:
  assert(0);
  break;
 case H5T_REFERENCE:
  assert(0);
  break;
/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */
 case H5T_COMPOUND:
  nmembs = H5Tget_nmembers(m_type);
  for (j = 0; j < nmembs; j++) 
  {
   offset    = H5Tget_member_offset(m_type, j);
   memb_type = H5Tget_member_type(m_type, j);
   nfound+=diff_array_mem(
    mem1+offset,
    mem2+offset,
    memb_type,
    i,
    rank,
    acc,
    pos,
    options,
    obj1,
    obj2);
   H5Tclose(memb_type);
  }
  break;

/*-------------------------------------------------------------------------
 * H5T_STRING, H5T_BITFIELD, H5T_OPAQUE, H5T_REFERENCE, H5T_ENUM:
 *-------------------------------------------------------------------------
 */
 case H5T_STRING:
  
  if(H5Tis_variable_str(m_type)) 
   type_size = HDstrlen(mem1);
  else 
   type_size = H5Tget_size(m_type);
  
  for (u=0; u<type_size; u++)
   nfound+=diff_native_uchar(
   mem1 + u,
   mem2 + u, /* offset */
   type_size,
   u, 
   rank, 
   acc,
   pos,
   options, 
   obj1, 
   obj2,
   ph);
  

  break;

/*-------------------------------------------------------------------------
 * H5T_BITFIELD, H5T_OPAQUE, H5T_REFERENCE, H5T_ENUM:
 *-------------------------------------------------------------------------
 */
 case H5T_BITFIELD:
 case H5T_OPAQUE:
 case H5T_ENUM:

  for (u=0; u<type_size; u++)
   nfound+=diff_native_uchar(
   mem1 + u,
   mem2 + u, /* offset */
   type_size,
   u, 
   rank, 
   acc,
   pos,
   options, 
   obj1, 
   obj2,
   ph);

  
  break;
/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 case H5T_ARRAY:
  /* get the array's base datatype for each element */
  memb_type = H5Tget_super(m_type);
  size      = H5Tget_size(memb_type);
  ndims     = H5Tget_array_ndims(m_type);
  H5Tget_array_dims(m_type, dims, NULL);
  assert(ndims >= 1 && ndims <= H5S_MAX_RANK);
  
  /* calculate the number of array elements */
  for (u = 0, nelmts = 1; u <ndims; u++)
   nelmts *= dims[u];
  for (u = 0; u < nelmts; u++) 
   nfound+=diff_array_mem(
    mem1 + u * size,
    mem2 + u * size, /* offset */
    memb_type,
    u,
    rank,
    acc,
    pos,
    options,
    obj1,
    obj2);
  H5Tclose(memb_type);
  break;

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */
 case H5T_VLEN:

  /* get the VL sequences's base datatype for each element */
  memb_type = H5Tget_super(m_type);
  size      = H5Tget_size(memb_type);
    
  /* get the number of sequence elements */
  nelmts = ((hvl_t *)mem1)->len;
  
  for (j = 0; j < nelmts; j++) 
    nfound+=diff_array_mem(
    ((char *)(((hvl_t *)mem1)->p)) + j * size,
    ((char *)(((hvl_t *)mem2)->p)) + j * size, /* offset */
    memb_type,
    j,
    rank,
    acc,
    pos,
    options,
    obj1,
    obj2);
  
  H5Tclose(memb_type);
  
  break;
 case H5T_INTEGER:


/*-------------------------------------------------------------------------
 * H5T_NATIVE_SCHAR
 *-------------------------------------------------------------------------
 */
  if (H5Tequal(m_type, H5T_NATIVE_SCHAR)) 
  {
   char        temp1_char;
   char        temp2_char;
   assert(type_size==sizeof(char));
   memcpy(&temp1_char, mem1, sizeof(char));
   memcpy(&temp2_char, mem2, sizeof(char));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs(temp1_char-temp2_char) > options->delta)
     {
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options->percent )
     {
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options->percent && 
      abs(temp1_char-temp2_char) > options->delta )
     {
      if ( options->r==0 ) 
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
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
     }
     nfound++;
    }
      
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
 
    memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs(temp1_uchar-temp2_uchar) > options->delta)
     {
     if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent )
     {
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent && 
      abs(temp1_uchar-temp2_uchar) > options->delta )
     {
      if ( options->r==0 ) 
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
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
     }
     nfound++;
    }
  
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
  
    memcpy(&temp1_short, mem1, sizeof(short));
    memcpy(&temp2_short, mem2, sizeof(short));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs(temp1_short-temp2_short) > options->delta)
     {
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options->percent )
     {
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options->percent && 
      abs(temp1_short-temp2_short) > options->delta )
     {
      if ( options->r==0 ) 
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
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
     }
     nfound++;
    }
    
     
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
  
    memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
    memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs(temp1_ushort-temp2_ushort) > options->delta)
     {
   
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_ushort!=0 && abs(1-temp2_ushort/temp1_ushort) > options->percent && 
      abs(temp1_ushort-temp2_ushort) > options->delta )
     {
    
      if ( options->r==0 ) 
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
   
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_ushort,temp2_ushort,abs(temp1_ushort-temp2_ushort));
     }
     nfound++;
    }
  
   
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
 
    memcpy(&temp1_int, mem1, sizeof(int));
    memcpy(&temp2_int, mem2, sizeof(int));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs(temp1_int-temp2_int) > options->delta)
     {
    
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options->percent && 
      abs(temp1_int-temp2_int) > options->delta )
     {
     
      if ( options->r==0 ) 
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
   
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
     }
     nfound++;
    }
    
     
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
  
    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
    memcpy(&temp2_uint, mem2, sizeof(unsigned int));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (abs((int)(temp1_uint-temp2_uint)) > options->delta)
     {
     
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(UIFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_uint!=0 && abs((int)(1-temp2_uint/temp1_uint)) > options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_uint!=0 && abs((int)(1-temp2_uint/temp1_uint)) > options->percent && 
      abs((int)(temp1_uint-temp2_uint)) > options->delta )
     {
     
      if ( options->r==0 ) 
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
    
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(UIFORMAT,temp1_uint,temp2_uint,abs((int)(temp1_uint-temp2_uint)));
     }
     nfound++;
    }
    
   
   
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
 
    memcpy(&temp1_long, mem1, sizeof(long));
    memcpy(&temp2_long, mem2, sizeof(long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs(temp1_long-temp2_long) > (long)options->delta)
     {
    
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options->percent && 
      labs(temp1_long-temp2_long) > (long)options->delta )
     {
     
      if ( options->r==0 ) 
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
    
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
     }
     nfound++;
    }
    
  
   
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
  
    memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
    memcpy(&temp2_ulong, mem2, sizeof(unsigned long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs((long)(temp1_ulong-temp2_ulong)) > (long)options->delta)
     {
     
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(ULIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_ulong!=0 && labs((long)(1-temp2_ulong/temp1_ulong)) > (long)options->percent )
     {
      
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_ulong!=0 && labs((long)(1-temp2_ulong/temp1_ulong)) > (long)options->percent && 
      labs((long)(temp1_ulong-temp2_ulong)) > (long)options->delta )
     {
    
      if ( options->r==0 ) 
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
    
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(ULIFORMAT,temp1_ulong,temp2_ulong,labs((long)(temp1_ulong-temp2_ulong)));
     }
     nfound++;
    }
  
   
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
  
    memcpy(&temp1_llong, mem1, sizeof(long_long));
    memcpy(&temp2_llong, mem2, sizeof(long_long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs((long)(temp1_llong-temp2_llong)) > (long)options->delta)
     {
    
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(fmt_llong,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > (long)options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_llong!=0 && labs((long)(1-temp2_llong/temp1_llong)) > (long)options->percent && 
      labs((long)(temp1_llong-temp2_llong)) > (long)options->delta )
     {
     
      if ( options->r==0 ) 
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
   
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(fmt_llong,temp1_llong,temp2_llong,(long_long)labs((long)(temp1_llong-temp2_llong)));
     }
     nfound++;
    }
 
   
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
  
    memcpy(&temp1_ullong, mem1, sizeof(unsigned long_long));
    memcpy(&temp2_ullong, mem2, sizeof(unsigned long_long));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (labs((long)(temp1_ullong-temp2_ullong)) > (long)options->delta)
     {
     
      if ( options->r==0 ) 
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
    else if (!options->d && options->p)
    {
     if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > (long)options->percent )
     {
   
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_ullong!=0 && labs((long)(1-temp2_ullong/temp1_ullong)) > (long)options->percent && 
      labs((long)(temp1_ullong-temp2_ullong)) > (long)options->delta )
     {
    
      if ( options->r==0 ) 
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
   
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(fmt_ullong,temp1_ullong,temp2_ullong,
       (unsigned long_long)labs((long)(temp1_ullong-temp2_ullong)));
     }
     nfound++;
    }
  
  } /*H5T_NATIVE_ULLONG*/

   
  break; /* H5T_INTEGER class */
 case H5T_FLOAT:

/*-------------------------------------------------------------------------
 * H5T_NATIVE_FLOAT
 *-------------------------------------------------------------------------
 */
  
  if (H5Tequal(m_type, H5T_NATIVE_FLOAT)) 
  {
   float       temp1_float;
   float       temp2_float;
   assert(type_size==sizeof(float));
  
    memcpy(&temp1_float, mem1, sizeof(float));
    memcpy(&temp2_float, mem2, sizeof(float));


    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (fabs(temp1_float-temp2_float) > options->delta)
     {
    
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options->percent && 
      fabs(temp1_float-temp2_float) > options->delta )
     {
    
      if ( options->r==0 ) 
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
    
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
     }
     nfound++;
    }
  
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
 
    memcpy(&temp1_double, mem1, sizeof(double));
    memcpy(&temp2_double, mem2, sizeof(double));
    /* -d and !-p */
    if (options->d && !options->p)
    {
     if (fabs(temp1_double-temp2_double) > options->delta)
     {
   
      if ( options->r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options->d && options->p)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options->percent )
     {
     
      if ( options->r==0 ) 
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
    else if ( options->d && options->p)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options->percent && 
      fabs(temp1_double-temp2_double) > options->delta )
     {
   
      if ( options->r==0 ) 
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
    
     if ( options->r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
     }
     nfound++;
    }
  
  } /*H5T_NATIVE_DOUBLE*/

  
  break;   /* H5T_FLOAT class */
  
  } /* switch */
  
  
  return nfound;
}



/*-------------------------------------------------------------------------
 * Function: diff_native_uchar
 *
 * Purpose: compare H5T_NATIVE_UCHAR (used in H5T_NATIVE_UCHAR 
 *  and H5T_STRING class)
 *
 * Return: number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 29, 2003
 *
 *-------------------------------------------------------------------------
 */


static
int diff_native_uchar(unsigned char *mem1,
                      unsigned char *mem2,
                      size_t        type_size,
                      unsigned      i, 
                      int           rank, 
                      int           *acc,  
                      int           *pos, 
                      diff_opt_t    *options, 
                      const char    *obj1, 
                      const char    *obj2,
                      int           ph)
{
 int                nfound=0;  /* differences found */
 unsigned char      temp1_uchar;
 unsigned char      temp2_uchar;
 
 
 memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
 memcpy(&temp2_uchar, mem2, sizeof(unsigned char));

 /* -d and !-p */
 if (options->d && !options->p)
 {
  if (abs(temp1_uchar-temp2_uchar) > options->delta)
  {
   if ( options->r==0 ) 
   {
    print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
    printf(SPACES);
    printf(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
   }
   nfound++;
  }
 }
 /* !-d and -p */
 else if (!options->d && options->p)
 {
  if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent )
  {
   if ( options->r==0 ) 
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
 else if ( options->d && options->p)
 {
  if ( temp1_uchar!=0 && abs(1-temp2_uchar/temp1_uchar) > options->percent && 
   abs(temp1_uchar-temp2_uchar) > options->delta )
  {
   if ( options->r==0 ) 
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
  if ( options->r==0 ) 
  {
   print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
   printf(SPACES);
   printf(IFORMAT,temp1_uchar,temp2_uchar,abs(temp1_uchar-temp2_uchar));
  }
  nfound++;
 }
 
 return nfound;
} 
