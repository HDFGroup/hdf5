/*-------------------------------------------------------------------------
 *
 * Copyright (C) 2000   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 */

/******************************************************************************

  Description: 

1. converter

See HDF4 to HDF5 mapping specification at
(http://hdf.ncsa.uiuc.edu/HDF5/papers/h4toh5) for the default mapping 
from HDF4 object to HDF5 object.
 
The whole converter includes 10 files, h4toh5util.h, h4toh5main.h, h4toh5util.c, h4toh5main.c, h4toh5sds.c, h4toh5image.c,h4toh5vdata.c,h4toh5vgroup.c,h4toh5pal.c and h4toh5anno.c.

2. this file 

including all routines that are useful for other files.

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/


#include "h4toh5util.h"


/* Function h4toh5_ZeroMemory
   Purpose: Zero out memory
   return:  None
   In: size_t n(DWORD in windows)
      void* s(PVOID in windows)
*/
void h4toh5_ZeroMemory(void*s,size_t n) {
#ifdef WIN32
     ZeroMemory(s,n);
#else
    bzero(s,n);
#endif
}

/*-------------------------------------------------------------------------
 * Function:	h5string_to_int
 *
 * Purpose:	This function will convert H5T_STRING into integer.
                This is a correction routine when the user define the 
		numerical datatype int8 as DFNT_CHAR8 and DFNT_UCHAR8

 * Errors:      will return error message to the interface
 * Return:	FAIL if failed, SUCCEED if success
  *
 * In :	        h4type: HDF4 datatype
                h4memsize: the real memory size of h4type

 * Out:         h5memtype: pointer of which value should be h5memtype(the real
                           data type stored at the memory)
                h5type:    pointer of which value should be h5type(the hdf5 
		           type stored at the disk).
 *
 *-------------------------------------------------------------------------
 */		
	
int h5string_to_int(const int32 h4type, hid_t* h5memtype,
		    const size_t h4memsize,hid_t* h5type) {

  switch(h4type) {

    case DFNT_CHAR8:

       *h5type = H5T_STD_I8BE;
       if (h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
          *h5memtype = H5T_NATIVE_SCHAR;
       else if(h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	  *h5memtype = H5T_NATIVE_SHORT;
       else if(h4memsize == H5Tget_size(H5T_NATIVE_INT))
	  *h5memtype = H5T_NATIVE_INT;
       else if(h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	  *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

    case DFNT_UCHAR8:
     
       *h5type = H5T_STD_U8BE;
       if (h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	  *h5memtype =  H5T_NATIVE_UCHAR;
       else if(h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	  *h5memtype = H5T_NATIVE_USHORT;
       else if(h4memsize == H5Tget_size(H5T_NATIVE_INT))
	  *h5memtype = H5T_NATIVE_UINT;
       else if(h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	  *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;
  }
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	h4type_to_h5type
 *
 * Purpose:	this function will convert HDF4 datatype into HDF5 datatype
                The converter includes file to file datatype and datasize
		conversion, file to memory datatype and datasize conversion. 
		Check the mapping document for details.

 * Errors:      will return error message to the interface.
 * Return:	false, FAIL. otherwise,SUCCEED.
  *
 * In :	        h4type: HDF4 datatype.
 * Out:         h4size: the file(disk) size of h4type.
                h4memsize: the real memory size of h4type.
 *              h5memtype: pointer of which value should be h5memtype(the real
                           type stored at the memory).
                h5type:    pointer of which value should be h5type(the hdf5 
		           type that is stored at the disk).
 *	     
 *
 *-------------------------------------------------------------------------
 */			  
int  h4type_to_h5type(const int32 h4type, hid_t* h5memtype,
		      size_t* h4memsize,size_t* h4size, hid_t *h5type)
{

    switch (h4type) {

     case DFNT_CHAR8:

       *h4size = 1;
       *h4memsize = sizeof(int8);
       /* assume DFNT_CHAR8 C type character. */
       *h5memtype = H5T_STRING;
       *h5type =  H5T_STRING;
       break;

     case DFNT_UCHAR8:

       *h4size = 1;
       *h4memsize = sizeof(int8);
       *h5memtype = H5T_STRING;
       *h5type = H5T_STRING;
       break;

     case DFNT_INT8:

       *h4size = 1;
       *h5type = H5T_STD_I8BE;
       *h4memsize = sizeof(int8);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype = H5T_NATIVE_SCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_UINT8:

       *h4size =1;
       *h5type = H5T_STD_U8BE;
       *h4memsize = sizeof(int8);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_NINT8:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting result may not be correct.\n");
       *h4size = 1;
       *h5type = H5T_NATIVE_SCHAR;
       *h4memsize = sizeof(int8);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_SCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
	break;

     case DFNT_NUINT8:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting result may not be correct.\n");
       *h4size = 1;
       *h5type = H5T_NATIVE_UCHAR;
       *h4memsize = sizeof(int8);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
	break;

     case DFNT_LINT8:
       *h4size = 1;
       *h5type = H5T_STD_I8LE;
       *h4memsize = sizeof(int8);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_LUINT8:
       *h4size = 1;
       *h5type = H5T_STD_U8LE;
       *h4memsize = sizeof(int8);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_INT16:
       *h4size = 2;
       *h5type = H5T_STD_I16BE;
       *h4memsize = sizeof(int16);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_CHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_UINT16:
       *h4size = 2;
       *h5type = H5T_STD_U16BE;
       *h4memsize = sizeof(int16);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_NINT16:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting result may not be correct.\n");
       *h4size = 2;
       *h5type = H5T_NATIVE_SHORT;
       *h4memsize = sizeof(int16);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_CHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_NUINT16:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting result may not be correct.\n");
       *h4size = 2;
       *h5type = H5T_NATIVE_USHORT;
       *h4memsize = sizeof(int16);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_LINT16:
       *h4size = 2;
       *h5type = H5T_STD_I16LE;
       *h4memsize = sizeof(int16);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_LUINT16:
       *h4size = 2;
       *h5type = H5T_STD_U16LE;
       *h4memsize = sizeof(int16);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_INT32:
       *h4size = 4;
       *h5type = H5T_STD_I32BE;
       *h4memsize = sizeof(int32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_CHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_UINT32:
       *h4size = 4;
       *h5type = H5T_STD_U32BE;
       *h4memsize = sizeof(int32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_NINT32:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting result may not be correct.\n");
       *h4size = 4;
       *h5type = H5T_NATIVE_INT;
       *h4memsize = sizeof(int32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_CHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_NUINT32:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting results may not be correct.\n");
       *h4size =4;
       *h5type = H5T_NATIVE_UINT;
       *h4memsize = sizeof(int32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_LINT32:
       *h4size =4;
       *h5type = H5T_STD_I32LE;
       *h4memsize = sizeof(int32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_CHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_SHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_INT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_LONG;
       else return FAIL;
       break;

     case DFNT_LUINT32:
       *h4size =4;
       *h5type = H5T_STD_U32LE;
       *h4memsize = sizeof(int32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_CHAR))
	 *h5memtype =  H5T_NATIVE_UCHAR;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_SHORT))
	 *h5memtype = H5T_NATIVE_USHORT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_INT))
	 *h5memtype = H5T_NATIVE_UINT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_LONG))
	 *h5memtype = H5T_NATIVE_ULONG;
       else return FAIL;
       break;

     case DFNT_FLOAT32:
       *h4size =4;
       *h5type = H5T_IEEE_F32BE;
       *h4memsize = sizeof(float32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_FLOAT))
	 *h5memtype = H5T_NATIVE_FLOAT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_DOUBLE))
	 *h5memtype = H5T_NATIVE_DOUBLE;
       else return FAIL;
	break;
  
     case DFNT_FLOAT64:
       *h4size = 8;
       *h5type = H5T_IEEE_F64BE;
       *h4memsize = sizeof(float64);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_FLOAT))
	 *h5memtype = H5T_NATIVE_FLOAT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_DOUBLE))
	 *h5memtype = H5T_NATIVE_DOUBLE;
       else return FAIL;
	break;

     case DFNT_NFLOAT32:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting results may not be correct.\n");
       *h4size = 4;
 	*h5type = H5T_NATIVE_FLOAT;
       *h4memsize = sizeof(float32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_FLOAT))
	 *h5memtype = H5T_NATIVE_FLOAT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_DOUBLE))
	 *h5memtype = H5T_NATIVE_DOUBLE;
       else return FAIL;
        break;

     case DFNT_NFLOAT64:
       printf("warning, Native HDF datatype is encountered");
       printf(" the converting result may not be correct.\n");
       *h4size = 8;
        *h5type = H5T_NATIVE_DOUBLE;
       *h4memsize = sizeof(float64);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_FLOAT))
	 *h5memtype = H5T_NATIVE_FLOAT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_DOUBLE))
	 *h5memtype = H5T_NATIVE_DOUBLE;
       else return FAIL;	
       break;

     case DFNT_LFLOAT32:
       *h4size = 4;
       *h5type = H5T_IEEE_F32LE;
       *h4memsize = sizeof(float32);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_FLOAT))
	 *h5memtype = H5T_NATIVE_FLOAT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_DOUBLE))
	 *h5memtype = H5T_NATIVE_DOUBLE;
       else return FAIL;
	break;

     case DFNT_LFLOAT64:
       *h4size = 8;
       *h5type = H5T_IEEE_F64LE;
       *h4memsize = sizeof(float64);
       if(*h4memsize == H5Tget_size(H5T_NATIVE_FLOAT))
	 *h5memtype = H5T_NATIVE_FLOAT;
       else if(*h4memsize == H5Tget_size(H5T_NATIVE_DOUBLE))
	 *h5memtype = H5T_NATIVE_DOUBLE;
       else return FAIL;
	break;

    default: 
      return FAIL;
    }
    return SUCCEED;
}
/*-------------------------------------------------------------------------
 * Function:	conv_int_str
 *
 * Purpose:	this function will convert numerical number into the 
                string format for a reference(<=65536).
 * Return:	SUCCEED if success, FAIL if failed.
  *
 * In :	        num: an unsigned digital number that is not greater than 65536.
                
 * Out:         str_num: character string format of the number. 

 *	     
 *
 *-------------------------------------------------------------------------
 */			

int conv_int_str(uint16 num, char* str_num) {

  /* the maximum reference number is 65536. */

   
  if(str_num == NULL) {
    printf(" memory for str_num should be allocated.\n");
    return FAIL;
  }

  /*  Adding this line will cause problems, investigating this later.
h4toh5_ZeroMemory(str_num,strlen(str_num)+1);*/

  sprintf(str_num,"%d",num);
  return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	lookup
 *
 * Purpose:	this function will use objref as a key to check whether
 *              the current object is touched.

 * Return:	1, the object is found. 0,the object is not found.
                -1, the table doesn't exist.
 *
 * In :	        objref: reference number of the current object.
                SIZE:   the hashtable SIZE.
		hashtab: pointer to the hash table.
 
 *-------------------------------------------------------------------------
 */			  

int lookup(int objref,int SIZE,struct table*hashtab) {

  struct table *np;
  if(hashtab == NULL) {
    printf("the table doesn't exist. \n");
    return -1;
  }
  np = hashtab+objref%SIZE;

  for (np = hashtab+objref%SIZE; np!=NULL;np=np->next){
    if (np->ref == objref){
       return 1;
    }
  }
  return 0;
}

/*-------------------------------------------------------------------------
 * Function:	init_tab
 *
 * Purpose:	this function will initialize the hash table.
 *             

 * Return:	SUCCEED, table is initialzed. FAIL,otherwise.
 *
 * In :	       
                SIZE:   the hashtable SIZE.
		hashtab: pointer to the hash table.
 
 *-------------------------------------------------------------------------
 */	

int init_tab(int SIZE,struct table *hashtab) {

  int i;
  if(hashtab == NULL) {
    printf("memory for hashing table is not allocated.\n");
    return FAIL;
  }
  for (i = 0;i < SIZE; i++) {
    (hashtab+i%SIZE)->ref  = -1;
    (hashtab+i%SIZE)->next = NULL;
    (hashtab+i%SIZE)->name = NULL;
  }
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	init_nametab
 *
 * Purpose:	this function will initialize the name hash table.
 *             

 * Return:	SUCCEED, table is initialzed. FAIL,otherwise.
 *
 * In :	       
                SIZE:   the hashtable SIZE.
		name_hashtab: pointer to the hash table.
 
 *-------------------------------------------------------------------------
 */	
int init_nametab(int SIZE, struct name_table * name_hashtab) {

  int i;

  if(name_hashtab == NULL) {
    printf("cannot allocate memory for name hashing table.\n");
    return FAIL;
  }
  for (i=0;i < SIZE; i++) {
    (name_hashtab+i%SIZE)->name = NULL;
    (name_hashtab+i%SIZE)->next = NULL;
  }
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	get_name
 *
 * Purpose:     obtain the name of the object
 *              
 * Return:	the object name 
 *
 * In :	        objref: reference number of the current object.
                SIZE:   the hashtable SIZE.
		hashtab: pointer to the hash table
		pcheck_get: a flag to check errors
 
 *-------------------------------------------------------------------------
 */			  

char* get_name(int objref,int SIZE,struct table*hashtab, int* pcheck_get) {

  struct table *np;
  char* tempname;

  np = hashtab+objref%SIZE;

  for (np = hashtab+objref%SIZE; np!=NULL;np=np->next){

     if (np->ref==objref){

       if (np->name == NULL) {
          *pcheck_get = -1;
	  return NULL;
       }

       else {
	 tempname = malloc(strlen(np->name)+1);
	 if(tempname == NULL) {
	   *pcheck_get = -2;
	   return NULL;
	 }
         strcpy(tempname,np->name);
	 return tempname;
       }
     }
  }

  *pcheck_get = 0;
  return NULL;
}


/*-------------------------------------------------------------------------
 * Function:	set_name
 *
 * Purpose:     store the name of the object into the hash table
 *              
 * Return:	SUCCEED: the name is either set before or set in this routine
 *              FAIL:  the name is not set properly    
 *
 * In :	        objref: reference number of the current object
                SIZE:   the hashtable SIZE
		hashtab: hash table
		namein: object name
 
 *-------------------------------------------------------------------------
 */			


int set_name(int objref,int SIZE,struct table*hashtab, char* namein) {

  struct table *np;
  struct table *temptr;

  temptr = malloc(sizeof(struct table));
  if(temptr == NULL) {
    printf("not enough memory to be allocated. \n");
    return FAIL;
  }

  np = hashtab+objref%SIZE;
  if(namein == NULL) {
    printf("error in inputting name into the table.\n");
    return FAIL;
  }

  for (np = hashtab+objref%SIZE; np!= NULL;np = np->next){
     if (np->ref==objref){
       /* the name is set already, don't do anything.*/
         return SUCCEED;
     }
     if (np->next == NULL) {
        np->next = temptr;
        temptr->ref = objref;
        temptr->next = NULL;
        temptr->name = malloc(strlen(namein)+1);
	if(temptr->name == NULL) {
	  printf("error in allocating memory. \n");
	  return FAIL;
	}
	strcpy(temptr->name,namein);
        return SUCCEED;
     }
  }
  return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	lookup_name
 *
 * Purpose:     1. look up whether the same name is used for different objects 
                2. then  update the table
 *              
 * Return:	1, if the name is in the name hash table.
                0, if the name is to be added into the name table.
                -1, otherwise.
 *
 * In :	        
                size:   the hashtable SIZE.
		nametab: name hash table
		name:   the name to be looked up
 
 *-------------------------------------------------------------------------
 */			

int lookup_name(char* name, int size,struct name_table *nametab) {

  /* temporary pointer of the name table that points to the beginning 
     address of the current bucket.*/
  struct name_table *np;

  /* temporary pointer of the added name table.*/
  struct name_table *temptr; 

  if(name == NULL) {
    printf("the name to be looked up is NULL.\n");
    return -1;
  }

  temptr = malloc(sizeof(struct name_table));
  if(temptr == NULL) {
    printf("not enough memory to be allocated. \n");
    return -1;
  }

  if(nametab == NULL) {
    printf("no name_table for this category of objects.\n");
    return -1;
  }
  np = nametab+hash_fun(name,size);

  temptr->name = malloc(strlen(name)+1);
  if(temptr->name == NULL) {
    printf("not enough memory to be allocated to table name.\n");
    return -1;
  }

  /* look through the linked list starting from the current bucket. 
     If the name  is found, return 1, otherwise, return 0
     after inserting the new bucket. */

  for(np = nametab+hash_fun(name,size); np!= NULL;np = np->next) {
     if(np->name == NULL) {
        np->name = malloc(strlen(name)+1);
	if(np->name == NULL) {
	  printf("cannot allocate memory for object name.\n");
	  return -1;
	}
        strcpy(np->name,name);
	free(temptr->name);
	free(temptr);
        return 0;
     }
     if(strcmp(name,np->name)==0){
       free(temptr->name);
       free(temptr);
        return 1;
     }
     if (np->next == NULL) {
	np->next = temptr;
        temptr->next = NULL;
        strcpy(temptr->name,name);
        return 0;
     }
  }
  return -1;
}


/*-------------------------------------------------------------------------
 * Function:	hash_fun
 *
 * Purpose:     to get the hash value based on the key 
 *              
 * Return:	No. of the hashtable 
 *
 * In :	        name:   object name 
                size:   the hashtable size.
	 
 *-------------------------------------------------------------------------
 */	
int hash_fun(char *name,int size) {

int hashval;

  for (hashval = 0;*name !='\0';)
      hashval += *name++;
  return(hashval%size); 

}

/*-------------------------------------------------------------------------
 * Function:	freenametable
 *
 * Purpose:     free the memory of hash table 
 *              
 * Return:	0
 *
 * In :	        
                SIZE:   the hashtable SIZE.
		nametab: hash table of the name 
 
 *-------------------------------------------------------------------------
 */	
int freenametable(int SIZE,struct name_table *nametab) {

  struct name_table *np,*temptr,*temptr1;
  int i;
  
  if(nametab == NULL) return 0;
  /* we first free the additional linked items of the hashtable,
     and then free the whole hash table. */
  for (i = 0;i < SIZE; i++) {
     np = nametab+i;
     temptr1 = np->next;
     while(temptr1 != NULL) {
       temptr = temptr1;
       temptr1 = temptr1->next;
       free(temptr->name);
       free(temptr);
     } 
     if(np->name !=NULL) free(np->name);
  }
  free(nametab);
  return 0;
}


/*-------------------------------------------------------------------------
 * Function:	freetable
 *
 * Purpose:     free the memory of hash table 
 *              
 * Return:	0
 *
 * In :	        
                SIZE:   the hashtable SIZE.
		nametab: hash table 
 
 *-------------------------------------------------------------------------
 */	
int freetable(int SIZE,struct table *hashtab) {

  struct table *np,*temptr,*temptr1;
  int i;
  if(hashtab == NULL) return 0; 

  /* we first free the additional linked items of the hashtable,
     and then free the whole hash table. */
  for (i =0;i < SIZE; i++) {
    np = hashtab+i;
    temptr1 = np->next;
    while(temptr1 != NULL) {
       temptr = temptr1;
       temptr1 = temptr1->next;
        free(temptr->name);
       free(temptr);
    }
    if(np->name != NULL) free(np->name);
  }

  free(hashtab);
  return 0;
}

/*-------------------------------------------------------------------------
 * Function:	mkstr
 *
 * Purpose:     make hdf5 string type
 *              
 * Return:	type
 *
 * In :	        
                size:    String Size
		H5T_str_t: pad
 
 *-------------------------------------------------------------------------
 */	

hid_t mkstr(int size, H5T_str_t pad) {

  hid_t type;

  if((type=H5Tcopy(H5T_C_S1))<0) return -1;
  if((H5Tset_size(type,(size_t)size))<0) return -1;
  if((H5Tset_strpad(type,pad))<0) return -1;

  return type;
}

/*-------------------------------------------------------------------------
 * Function:	h4_transnumattr
 *
 * Purpose:     translate reference number into hdf5 attribute
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                h5g: hdf5 group id
		refname: reference name
		group_ref: reference number
 
 *-------------------------------------------------------------------------
 */    
int h4_transnumattr(hid_t h5g,const char *refname,uint16 group_ref) {

  hid_t    h5memtype=(-1);
  hid_t    h5a_id;
  hid_t    h5a_sid;
  herr_t   ret;

  h5a_sid = H5Screate(H5S_SCALAR);

  if (h5a_sid < 0) {
     fprintf(stderr,"failed to create attribute space for HDF4_REF_NUM. \n"); 
     return FAIL;
  }

  h5a_id = H5Acreate(h5g,refname,H5T_STD_U16BE,h5a_sid,H5P_DEFAULT);

  if(h5a_id <0) {
    fprintf(stderr,"failed to obtain attribute id for HDF4_REF_NUM. \n");
    H5Sclose(h5a_sid);
    return FAIL;
  }

  if(H5Tget_size(H5T_NATIVE_CHAR)== sizeof(uint16))
    h5memtype =  H5T_NATIVE_UCHAR;
  else if(H5Tget_size(H5T_NATIVE_SHORT)== sizeof(uint16))
    h5memtype = H5T_NATIVE_USHORT;
  else if(H5Tget_size(H5T_NATIVE_INT) == sizeof(uint16))
    h5memtype = H5T_NATIVE_UINT;
  else if(H5Tget_size(H5T_NATIVE_LONG)== sizeof(uint16))
    h5memtype = H5T_NATIVE_ULONG;

  ret = H5Awrite(h5a_id,h5memtype,(void *)&group_ref);

  if(ret <0) {
    printf("failed to obtain attribute.\n ");
    H5Sclose(h5a_sid);
    H5Aclose(h5a_id);
    return FAIL;
  }

  ret = H5Sclose(h5a_sid);
  ret = H5Aclose(h5a_id);
  return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	h4_transpredattrs
 *
 * Purpose:     translate predefined attributes into hdf5 attribute
 *              predefined attributes include HDF4 OBJECT TYPE, 
                HDF4 OBJECT NAME, HDF4 CLASS etc. They are all in
		H5T_STRING format.

 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                h5g: group id
		attrname: attribute name
		data: attribute data
 
 *-------------------------------------------------------------------------
 */	   
int h4_transpredattrs(hid_t h5g,const char *attrname,char*data){

   hsize_t   h5str_size;
   hid_t     h5a_id;
   hid_t     h5a_sid;
   hid_t     h5str_type;
   herr_t    ret;

   if(data == NULL) {
     printf("attribute data is not available.\n");
     return FAIL;
   }

   h5str_size = strlen(data);

   if ((h5str_type = mkstr(h5str_size,H5T_STR_SPACEPAD))<0) {
      printf("error in making string for predefined ATTR. \n");
      return FAIL;
   }

   h5a_sid = H5Screate(H5S_SCALAR);

   if (h5a_sid < 0) {
      printf("failed to create attribute space for HDF4_OBJECT. \n");
      return FAIL;
   }

   h5a_id = H5Acreate(h5g,attrname,h5str_type,h5a_sid,H5P_DEFAULT);

   if(h5a_id <0) {
     fprintf(stderr,"failed to obtain attribute id for HDF4_OBJECT. \n");
     H5Sclose(h5a_sid);
     return FAIL;
   }

   ret = H5Awrite(h5a_id,h5str_type,(void *)data);

   if(ret <0) {
     fprintf(stderr,"failed to obtain attribute.\n ");
     H5Aclose(h5a_id);
     H5Sclose(h5a_sid);
     return FAIL;
   }
   ret = H5Sclose(h5a_sid);
   ret = H5Aclose(h5a_id);
   return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	vg_transattrs
 *
 * Purpose:     translate predefined vgroup attributes into hdf5 attribute
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                h4vg: hdf4 vgroup id
		h5g:  hdf5 group id
 
 *-------------------------------------------------------------------------
 */	

int vg_transattrs(int32 h4vg,hid_t h5g) {

  /* define variables for hdf4. */
   char      vgroup_name[VGNAMELENMAX];
   char      vgroup_class[VGNAMELENMAX]; 
   char      vgattr_name[MAX_NC_NAME];
   char      obtype[MAX_NC_NAME];

   int32     vgroup_cref;
   int32     num_vgattr;
   int32     count_vgattr;
   int32     vg_atype;
   int32     attr_size;

   size_t    sh4_size;
   size_t    sh4_amemsize;

   /* define variables for hdf5. */
   hid_t     sh5a_sid;
   hid_t     sh5a_id;
   hid_t     sh5_atype;
   hid_t     sh5_amemtype;
   hid_t     sh5str_type;
   hid_t     sh5str_memtype;
   hsize_t   sh5dims[MAX_VAR_DIMS];
   void*     vg_adata;
   herr_t    sret;
   int       i;

   num_vgattr = Vnattrs(h4vg);
 
   for (i = 0;i <num_vgattr;i++) {

     if (Vattrinfo(h4vg,i,vgattr_name,&vg_atype,
		   &count_vgattr,&attr_size)== FAIL){  
        printf("unable to obtain attribute information. \n"); 
        return FAIL;
     }

     /* convert attribute datatype into the corresponding hdf5 datatype */

     if(h4type_to_h5type(vg_atype,&sh5_amemtype,&sh4_amemsize,
			 &sh4_size,&sh5_atype)==FAIL){
       printf("unable to do data type converting.\n");
       return FAIL;
     }

     vg_adata = malloc(sh4_amemsize*count_vgattr);

     if(vg_adata == NULL) {
       printf("error in allocating vgroup attribute data. \n");
       return FAIL;
     }

     if(Vgetattr(h4vg,i,(VOIDP)vg_adata)==FAIL){
       printf("unable to get attribute.\n");
       free(vg_adata);
       return FAIL;
     }

     /* if the attribute doesn't have a name, a default name is set. */
     if(vgattr_name[0] == '\0') 
       strcpy(vgattr_name,trans_obj_name(DFTAG_VG,i));

     /* now do attribute-transferring.
       1. deal with string data type
       2. set attribute space
       3. get attribute name, set property list. */
           
     if (sh5_atype ==  H5T_STRING ) {

	sh5a_sid = H5Screate(H5S_SCALAR);

	if (sh5a_sid < 0) {
	   printf("failed to create attribute space ");
           printf("for HDF4_OBJECT_TYPE SDS. \n"); 
	   free(vg_adata);
	   return FAIL;
	}

	if ((sh5str_type = mkstr(count_vgattr*sh4_size,H5T_STR_SPACEPAD))<0) {
           fprintf(stderr,"error in making string for VGROUP ATTR. \n");
	   free(vg_adata);
	   return FAIL;
	}

        
       	if ((sh5str_memtype = mkstr(count_vgattr*sh4_amemsize,
				    H5T_STR_SPACEPAD))<0){
	  fprintf(stderr,"error in making memory string for VGROUP ATTR. \n");
	  free(vg_adata);
	  return FAIL;
	}

        sh5a_id = H5Acreate(h5g,vgattr_name,sh5str_type,sh5a_sid,H5P_DEFAULT);

        if (sh5a_id <0) {
	   printf("failed to obtain attribute id"); 
           printf(" for HDF4_OBJECT_TYPE VGROUP. \n");
	   free(vg_adata);
	   return FAIL;
	}
        sret = H5Awrite(sh5a_id,sh5str_memtype,(void *)vg_adata);

	if (sret <0) {
	  fprintf(stderr,"failed to obtain attribute.\n ");
	  free(vg_adata);
	  return FAIL;
	}
        sret = H5Sclose(sh5a_sid);
        sret = H5Aclose(sh5a_id);
     }
	 
     else {
      
       if (count_vgattr == 1) {
	 sh5a_sid = H5Screate(H5S_SCALAR);
	 if (sh5a_sid < 0) {
	    fprintf(stderr,"failed to create space id. \n");
	    free(vg_adata);
	    return FAIL;
	 }
       }

       else {
         
         sh5dims[0] = count_vgattr;
	 sh5a_sid =  H5Screate_simple(1,sh5dims,NULL);
	 if (sh5a_sid < 0)  {
	   fprintf(stderr,"failed to create vgroup attribute space. \n");
	   free(vg_adata);
	   return FAIL;
	 }
       }

       sh5a_id = H5Acreate(h5g,vgattr_name,sh5_atype,sh5a_sid,H5P_DEFAULT);

       if(sh5a_id <0) {
	 fprintf(stderr,"failed to obtain attribute id. \n");
	 free(vg_adata);
	 H5Sclose(sh5a_sid);
	 return FAIL;
       }
       sret = H5Awrite(sh5a_id,sh5_amemtype,(void *)vg_adata);

       if(sret < 0) {
	 fprintf(stderr,"failed to obtain attribute.\n ");
	 free(vg_adata);
	 H5Sclose(sh5a_sid);
	 H5Aclose(sh5a_id);
	 return FAIL;
       }

       sret = H5Sclose(sh5a_sid);
       sret = H5Aclose(sh5a_id);
     }
     free(vg_adata);
   }
  
   /*** check this line later. ***/
   strcpy(obtype,VGROUPLABEL);
   vgroup_class[0] = '\0';

   /* ignore CDF0.0 and RIG0.0 vgroups. */
   if(Vgetclass(h4vg,vgroup_class) == SUCCEED){
     if(vgroup_class[0] != '\0') {
       if(!strcmp(vgroup_class,_HDF_CDF)||
	  !strcmp(vgroup_class,GR_NAME))
	 return SUCCEED;
     }
   }
    
   /* transfer predefined attributes. */
   if(h4_transpredattrs(h5g,HDF4_OBJECT_TYPE,obtype)==FAIL){
     printf("error in data attribute transferring.\n");
     return FAIL;
   }

   if(Vgetname(h4vg,vgroup_name) == SUCCEED){
     if(vgroup_name[0] != '\0') {
       if(h4_transpredattrs(h5g,HDF4_OBJECT_NAME,vgroup_name)==FAIL){
	 printf("error in data attribute transferring.\n");
	 return FAIL;
       }
     }
   }
  
   if(vgroup_class[0] !='\0') {
     if(h4_transpredattrs(h5g,HDF4_VGROUP_CLASS,vgroup_class)==FAIL){
       printf("error in data attribute transferring.\n");
       return FAIL;
     } 
   }

   vgroup_cref = VQueryref(h4vg);
   if(vgroup_cref == FAIL) {
     printf("failed to obtain group reference number.\n");
     return FAIL;
   }

   if(h4_transnumattr(h5g,HDF4_REF_NUM,vgroup_cref)==FAIL){
     printf("error in data attribute transferring.\n");
     return FAIL;
   }
   
   return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	get_obj_aboname
 *
 * Purpose:     get absolute path name of hdf5 object
                In this function, we will deal with name clashing.
		If we find an object name(by using lookup_name routine)
		that has already been used,
		we will remake name for this object. We will follow
		object type(vgroup,sds,image,palette, vdata) plus reference
		number to make it unique.
 *              
 * Return:	NULL if failed, object name if successful.
 *
 * In :	        
                obj_name: relative object name 
		ref_str: reference number in character format
		path_name: absolute path 
		objstr: object type in character format
 
 *-------------------------------------------------------------------------
 */	

char* get_obj_aboname(char* obj_name,char* refstr,char* path_name,
		     const char*objstr ) {

  char *abo_objname;
  int check_name;
  char check_char;

   
  /* sometimes although the object name is not NULL, but it is empty. 
     We will use make_objname_no under this situation. */
  if(obj_name != NULL) check_char = *obj_name;

  /* obtain the absolute name of the object. */
  if (obj_name == NULL || check_char == '\0')
    abo_objname = make_objname_no(refstr,path_name,objstr);
  else 
    abo_objname = make_objname_yes(obj_name,path_name);
 
  /* look up the name and see whether there is name clashing here.
     if yes, remake the object name.*/
  check_name = lookup_name(abo_objname,num_objects,name_hashtab);

  if(check_name == 1) {
    /* name_clashing is found. */
    if(objstr != NULL && refstr != NULL){
      free(abo_objname);

      if(path_name != NULL) {
	abo_objname= malloc(strlen(path_name)+strlen(objstr)+
			    strlen(refstr)+3);
	if(abo_objname == NULL) {
	  printf("error in allocating memory. \n");
	  return NULL;
	}
	h4toh5_ZeroMemory(abo_objname,strlen(path_name)+strlen(objstr)+
	      strlen(refstr)+3);
	strcpy(abo_objname,path_name);
	strcat(abo_objname,"/");
	strcat(abo_objname,objstr);
	strcat(abo_objname,"_");
	strcat(abo_objname,refstr);
      }
 
      else {
	abo_objname= malloc(strlen(objstr)+strlen(refstr)+3); 
	if(abo_objname == NULL) {
	  printf("error in allocating memory. \n");
	  return NULL;
	}
	h4toh5_ZeroMemory(abo_objname,strlen(objstr)+strlen(refstr)+3);
	strcat(abo_objname,"/");
	strcat(abo_objname,objstr);
	strcat(abo_objname,"_");
	strcat(abo_objname,refstr);
     }
    }
  }
 
  return abo_objname;
}

/*-------------------------------------------------------------------------
 * Function:	make_objname_no
 *
 * Purpose:     get absolute path name of hdf5 object when object name is 
                not defined. 
                We will use path name and 
		object type(vgroup,sds,image,palette, vdata) plus reference
		number to make it unique.
 *              
 * Return:	NULL if failed, object name if successful.
 *
 * In :	        
		ref_str: reference number in character format
		path_name: absolute path 
		objstr: object type in character format
 
 *-------------------------------------------------------------------------
 */	

char* make_objname_no(char* refstr,char* path_name,const char*objstr) {

  char *new_objname;

    if(objstr == NULL || refstr == NULL) {
      printf("error, object type and ref. number should be defined.\n");
      return NULL;
    }

    if (path_name == NULL) {/* under root group. */
      
      new_objname= malloc(strlen(objstr)+strlen(refstr)+3);
      if(new_objname == NULL) {
	  printf("error in allocating memory for object name. \n");
	  return NULL;
      }
      h4toh5_ZeroMemory(new_objname,strlen(objstr)+strlen(refstr)+3);
      strcpy(new_objname,"/");
      strcat(new_objname,objstr);
      strcat(new_objname,"_");
      strcat(new_objname,refstr);
    }

    else {

      new_objname= malloc(strlen(path_name)+strlen(objstr)+strlen(refstr)+3);
      if(new_objname == NULL) {
	printf("error in allocating memory. \n");
	return NULL;
      }
      h4toh5_ZeroMemory(new_objname,strlen(path_name)+strlen(objstr)+strlen(refstr)+3);
      strcpy(new_objname,path_name);
      strcat(new_objname,"/");
      strcat(new_objname,objstr);
      strcat(new_objname,"_");
      strcat(new_objname,refstr);
    }

    return new_objname;
}

/*-------------------------------------------------------------------------
 * Function:	make_objname_yes
 *
 * Purpose:     get absolute path name of hdf5 object when object name is 
                defined. 
               
 *              
 * Return:	NULL if failed, object name if successful.
 *
 * In :	        obj_name: object name
		path_name: absolute path 
 
 *-------------------------------------------------------------------------
 */	

char* make_objname_yes(char* obj_name,char* path_name){

  char*new_objname;

  if(path_name == NULL) {
   new_objname = malloc(strlen(obj_name)+2);
   if(new_objname == NULL) {
     printf("error in allocating memory. \n");
     return NULL;
   }
   h4toh5_ZeroMemory(new_objname,strlen(obj_name)+2);
   strcpy(new_objname,"/");
   strcat(new_objname,obj_name);
  }
  else {
   new_objname = malloc(strlen(path_name)+strlen(obj_name)+2);
   if(new_objname == NULL) {
     printf("error in allocating memory. \n");
     return NULL;
   }
   h4toh5_ZeroMemory(new_objname,strlen(path_name)+strlen(obj_name)+2);
   strcpy(new_objname,path_name);
   strcat(new_objname,"/");
   strcat(new_objname,obj_name);
  }
  return new_objname;
}

/*-------------------------------------------------------------------------
 * Function:	trans_obj_name
 *
 * Purpose:     obtain hdf4 attribute name from hdf4 object type
                plus ATTR plus reference number.
 *              
 * Return:      object name;
 *
 * In :	        
                obj_tag:  hdf4 tag 
		index  :  hdf5 group id
 
 *-------------------------------------------------------------------------
 */	     
char* trans_obj_name(int32 obj_tag,int32 index) {

  char* obj_name;
  char indstr[5];

  /* the reason why we allocate memory with strlen(HDF4_PALETTE) is 
     HDF4_PALETTE is the longest string among HDF4_??? */
  obj_name = malloc(strlen(HDF4_PALETTE)+strlen(ATTR)+8);
  if(obj_name == NULL) {
    printf("cannot allocate memory for object name. \n");
    return NULL;
  }

  h4toh5_ZeroMemory(obj_name,strlen(HDF4_PALETTE)+strlen(ATTR)+8);

  if(conv_int_str(index,indstr)== FAIL) {
    printf("indstr is not allocated. \n");
    return NULL;
  }

  switch(obj_tag) {

      case DFTAG_SD:
      case DFTAG_NDG:
      case DFTAG_SDG:
	strcpy(obj_name,HDF4_SDS);
	break;

      case DFTAG_RIG:
      case DFTAG_RI:
      case DFTAG_RI8:
	strcpy(obj_name,HDF4_IMAGE);
	break;

      case DFTAG_VG:
	strcpy(obj_name,HDF4_VGROUP);
	break;

      case DFTAG_VH:
      case DFTAG_VS:
	strcpy(obj_name,HDF4_VDATA);
	break;

      case DFTAG_LUT: 
	strcpy(obj_name,HDF4_PALETTE);
	break;

      default: 
	printf("error, object tag is transferred out of limits. \n");
	free(obj_name);
	return NULL;
  }
    
  strcat(obj_name,"_");
  strcat(obj_name,ATTR);
  strcat(obj_name,"_");
  strcat(obj_name,indstr);

  return obj_name;
}
/*-------------------------------------------------------------------------
 * Function:	freehashmemory
 *
 * Purpose:     free memories allocated for hash tables.
                
 *              
 * Return:      NULL 
 *
 * In :	        
                
 
 *-------------------------------------------------------------------------
 */	     

void freehashmemory(void){

  if(estnum_vg > 0) freetable(estnum_vg,vg_hashtab);
  if(estnum_vd > 0) freetable(estnum_vd,vd_hashtab);

  if(num_sds > 0) {
    freetable(2*num_sds,sds_hashtab);
    freenametable(DIM_HASHSIZE,dim_hashtab);
  }

  if(num_images > 0) {
    freetable(2*num_images,gr_hashtab);
    freetable(PAL_HASHSIZE,pal_hashtab);
  }

  if(num_objects > 0) freenametable(num_objects,name_hashtab);

}

/*-------------------------------------------------------------------------
 * Function:    correct_name
 *
 * Purpose:     modify the hdf4 object name when the name contains '/'. Change
                this character into '_'.
                
 *              
 * Return:      the corrected name
 *
 * In :	        old name
                
 
 *-------------------------------------------------------------------------
 */	  
char *correct_name(char* oldname){

  char * cptr;
  char * newname;
  
  if(oldname == NULL) {
    printf("inputting name is wrong.\n");
    return NULL;
  }

  newname = malloc(strlen(oldname)+1);
  h4toh5_ZeroMemory(newname,strlen(oldname)+1);
  newname = strncpy(newname, oldname, strlen(oldname));

  while(strchr(newname,ORI_SLASH)!= NULL){
    cptr = strchr(newname,ORI_SLASH);
    *cptr = CHA_SLASH;
  }

  return newname;
}
















