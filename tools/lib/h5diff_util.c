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
#include "ph5diff.h"
#include "H5private.h"

/* global variables */
int      g_nTasks = 1;
unsigned char  g_Parallel = 0;  /*0 for serial, 1 for parallel */
char     outBuff[OUTBUFF_SIZE];
int      outBuffOffset;
FILE*    overflow_file = NULL;

/*-------------------------------------------------------------------------
 * Function: parallel_print
 *
 * Purpose: wrapper for printf for use in parallel mode.
 *
 * Programmer: Leon Arber
 *
 * Date: December 1, 2004
 *
 *-------------------------------------------------------------------------
 */
void parallel_print(const char* format, ...)
{
 int  bytes_written;
 va_list ap;
 
 va_start(ap, format);
 
 if(!g_Parallel)
  vprintf(format, ap);
 else
 {
  
  if(overflow_file == NULL) /*no overflow has occurred yet */
  {
#if 0
   printf("calling HDvsnprintf: OUTBUFF_SIZE=%ld, outBuffOffset=%ld, ", (long)OUTBUFF_SIZE, (long)outBuffOffset);
#endif
   bytes_written = HDvsnprintf(outBuff+outBuffOffset, OUTBUFF_SIZE-outBuffOffset, format, ap);
#if 0
   printf("bytes_written=%ld\n", (long)bytes_written);
#endif
   va_end(ap);
   va_start(ap, format);
   
#if 0
   printf("Result: bytes_written=%ld, OUTBUFF_SIZE-outBuffOffset=%ld\n", (long)bytes_written, (long)OUTBUFF_SIZE-outBuffOffset);
#endif
   
   if ((bytes_written < 0) ||
#ifdef H5_VSNPRINTF_WORKS
    (bytes_written >= (OUTBUFF_SIZE-outBuffOffset))
#else
    ((bytes_written+1) == (OUTBUFF_SIZE-outBuffOffset))
#endif
    )
   {
    /* Terminate the outbuff at the end of the previous output */
    outBuff[outBuffOffset] = '\0';
    
    overflow_file = HDtmpfile();
    if(overflow_file == NULL)
     fprintf(stderr, "Warning: Could not create overflow file.  Output may be truncated.\n");
    else
     bytes_written = HDvfprintf(overflow_file, format, ap);
   }
   else
    outBuffOffset += bytes_written;
  }
  else
   bytes_written = HDvfprintf(overflow_file, format, ap);
  
 }
 va_end(ap);
}


/*-------------------------------------------------------------------------
 * Function: print_dims
 *
 * Purpose: print dimensions
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
void print_dims( int r, hsize_t *d )
{
 int i;
 parallel_print("[ " );
 for ( i=0; i<r; i++ )
  parallel_print("%"H5_PRINTF_LL_WIDTH"u ",(unsigned long_long)d[i]  );
 parallel_print("] " );
}

/*-------------------------------------------------------------------------
 * Function: print_type
 *
 * Purpose: Print name of datatype
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: Adapted from h5dump for H5T_INTEGER and H5T_FLOAT classes only
 *
 *-------------------------------------------------------------------------
 */
void print_type(hid_t type)
{
 switch (H5Tget_class(type))
 {
 default:
  return;
 case H5T_INTEGER:
  if (H5Tequal(type, H5T_STD_I8BE)) {
   printf("H5T_STD_I8BE");
  } else if (H5Tequal(type, H5T_STD_I8LE)) {
   printf("H5T_STD_I8LE");
  } else if (H5Tequal(type, H5T_STD_I16BE)) {
   printf("H5T_STD_I16BE");
  } else if (H5Tequal(type, H5T_STD_I16LE)) {
   printf("H5T_STD_I16LE");
  } else if (H5Tequal(type, H5T_STD_I32BE)) {
   printf("H5T_STD_I32BE");
  } else if (H5Tequal(type, H5T_STD_I32LE)) {
   printf("H5T_STD_I32LE");
  } else if (H5Tequal(type, H5T_STD_I64BE)) {
   printf("H5T_STD_I64BE");
  } else if (H5Tequal(type, H5T_STD_I64LE)) {
   printf("H5T_STD_I64LE");
  } else if (H5Tequal(type, H5T_STD_U8BE)) {
   printf("H5T_STD_U8BE");
  } else if (H5Tequal(type, H5T_STD_U8LE)) {
   printf("H5T_STD_U8LE");
  } else if (H5Tequal(type, H5T_STD_U16BE)) {
   printf("H5T_STD_U16BE");
  } else if (H5Tequal(type, H5T_STD_U16LE)) {
   printf("H5T_STD_U16LE");
  } else if (H5Tequal(type, H5T_STD_U32BE)) {
   printf("H5T_STD_U32BE");
  } else if (H5Tequal(type, H5T_STD_U32LE)) {
   printf("H5T_STD_U32LE");
  } else if (H5Tequal(type, H5T_STD_U64BE)) {
   printf("H5T_STD_U64BE");
  } else if (H5Tequal(type, H5T_STD_U64LE)) {
   printf("H5T_STD_U64LE");
  } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
   printf("H5T_NATIVE_SCHAR");
  } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
   printf("H5T_NATIVE_UCHAR");
  } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
   printf("H5T_NATIVE_SHORT");
  } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
   printf("H5T_NATIVE_USHORT");
  } else if (H5Tequal(type, H5T_NATIVE_INT)) {
   printf("H5T_NATIVE_INT");
  } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
   printf("H5T_NATIVE_UINT");
  } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
   printf("H5T_NATIVE_LONG");
  } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
   printf("H5T_NATIVE_ULONG");
  } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
   printf("H5T_NATIVE_LLONG");
  } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
   printf("H5T_NATIVE_ULLONG");
  } else {
   printf("undefined integer");
  }
  break;

 case H5T_FLOAT:
  if (H5Tequal(type, H5T_IEEE_F32BE)) {
   printf("H5T_IEEE_F32BE");
  } else if (H5Tequal(type, H5T_IEEE_F32LE)) {
   printf("H5T_IEEE_F32LE");
  } else if (H5Tequal(type, H5T_IEEE_F64BE)) {
   printf("H5T_IEEE_F64BE");
  } else if (H5Tequal(type, H5T_IEEE_F64LE)) {
   printf("H5T_IEEE_F64LE");
  } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
   printf("H5T_NATIVE_FLOAT");
  } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
   printf("H5T_NATIVE_DOUBLE");
#if H5_SIZEOF_LONG_DOUBLE !=0
  } else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
   printf("H5T_NATIVE_LDOUBLE");
#endif
  } else {
   printf("undefined float");
  }
  break;

 }/*switch*/
}

/*-------------------------------------------------------------------------
 * Function: diff_basename
 *
 * Purpose: Returns a pointer to the last component absolute name
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
const char*
diff_basename(const char *name)
{
 size_t i;

 if (name==NULL)
  return NULL;

 /* Find the end of the base name */
 i = strlen(name);
 while (i>0 && '/'==name[i-1])
  --i;

 /* Skip backward over base name */
 while (i>0 && '/'!=name[i-1])
  --i;

 return(name+i);
}

/*-------------------------------------------------------------------------
 * Function: get_type
 *
 * Purpose: Returns the type as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
const char*
get_type(int type)
{
 switch (type)
 {
 case H5G_DATASET:
  return("H5G_DATASET");
 case H5G_GROUP:
  return("H5G_GROUP");
 case H5G_TYPE:
  return("H5G_TYPE");
 case H5G_LINK:
  return("H5G_LINK");
 default:
  return("user defined type");
 }
}

/*-------------------------------------------------------------------------
 * Function: get_sign
 *
 * Purpose: Returns the sign as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 *-------------------------------------------------------------------------
 */
const char*
get_sign(H5T_sign_t sign)
{
 switch (sign)
 {
 default:
  return("H5T_SGN_ERROR");
 case H5T_SGN_NONE:
  return("H5T_SGN_NONE");
 case H5T_SGN_2:
  return("H5T_SGN_2");
 }
}


/*-------------------------------------------------------------------------
 * Function: get_class
 *
 * Purpose: Returns the class as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
const char*
get_class(H5T_class_t tclass)
{
 switch (tclass)
 {
 default:
  return("Invalid class");
 case H5T_TIME:
  return("H5T_TIME");
 case H5T_INTEGER:
  return("H5T_INTEGER");
 case H5T_FLOAT:
  return("H5T_FLOAT");
 case H5T_STRING:
  return("H5T_STRING");
 case H5T_BITFIELD:
  return("H5T_BITFIELD");
 case H5T_OPAQUE:
  return("H5T_OPAQUE");
 case H5T_COMPOUND:
  return("H5T_COMPOUND");
 case H5T_REFERENCE:
  return("H5T_REFERENCE");
 case H5T_ENUM:
  return("H5T_ENUM");
 case H5T_VLEN:
  return("H5T_VLEN");
 case H5T_ARRAY:
  return("H5T_ARRAY");
 }
}

/*-------------------------------------------------------------------------
 * Function: print_found
 *
 * Purpose: print number of differences found
 *
 *-------------------------------------------------------------------------
 */
void print_found(hsize_t nfound)
{
 if(g_Parallel)
  parallel_print("%"H5_PRINTF_LL_WIDTH"u differences found\n", (unsigned long_long)nfound);
 else
  HDfprintf(stdout,"%Hu differences found\n",nfound);
}



