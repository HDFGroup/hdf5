

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

#include "hdf5.h"
#include "h5tools.h"

/*-------------------------------------------------------------------------
 * print a warning message
 *-------------------------------------------------------------------------
 */
static void print_warning(const char *dname, const char *fname)
{
 printf("Warning: dataset <%s> cannot be read, %s filter is not available\n",
  dname,fname);
}

/*-------------------------------------------------------------------------
 * Function: h5tools_canreadf
 *
 * Purpose: check if the dataset creation property list has filters that 
 * are not registered in the current configuration 
 * 1) the external filters GZIP and SZIP might not be available
 * 2) the internal filters might be turned off
 *
 * Return: 1, can read, 0, cannot, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: March 1, 2004
 *
 *-------------------------------------------------------------------------
 */
int h5tools_canreadf(const char* name, /* object name, serves also as boolean print */
                     hid_t dcpl_id)    /* dataset creation property list */
{

 int          nfilters;       /* number of filters */
 H5Z_filter_t filtn;          /* filter identification number */
 int          i;              /* index */
 int          have_deflate=0; /* assume initially we do not have filters */
 int          have_szip=0;
 int          have_shuffle=0; 
 int          have_fletcher=0;

#ifdef H5_HAVE_FILTER_DEFLATE
 have_deflate=1;
#endif
#ifdef H5_HAVE_FILTER_SZIP
 have_szip=1;
#endif
#ifdef H5_HAVE_FILTER_SHUFFLE
 have_shuffle=1;
#endif
#ifdef H5_HAVE_FILTER_FLETCHER32
 have_fletcher=1;
#endif

 /* get information about filters */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0) 
  return -1;

 /* if we do not have filters, we can read the dataset safely */
 if (!nfilters)
  return 1;
 
 /* check availability of filters */
 for (i=0; i<nfilters; i++) 
 {
  if ((filtn = H5Pget_filter(dcpl_id,(unsigned)i,0,0,0,0,0))<0) 
   return -1;
  
  switch (filtn)
  {
  default:
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_DEFLATE	   1 , deflation like gzip	   
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_DEFLATE:
   if (!have_deflate)
   {
    if (name)
     print_warning(name,"deflate");
    return 0;
   }
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_SZIP       4 , szip compression 
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SZIP:
   if (!have_szip)
   {
    if (name)
     print_warning(name,"SZIP");
    return 0;
   }
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_SHUFFLE    2 , shuffle the data
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_SHUFFLE:
   if (!have_shuffle)
   {
    if (name)
     print_warning(name,"shuffle");
    return 0;
   }
   break;
/*-------------------------------------------------------------------------
 * H5Z_FILTER_FLETCHER32 3 , fletcher32 checksum of EDC
 *-------------------------------------------------------------------------
 */
  case H5Z_FILTER_FLETCHER32:
   if (!have_fletcher)
   {
    if (name)
     print_warning(name,"fletcher32");
    return 0;
   }
   break;
  }/*switch*/
 }/*for*/

 return 1;
}



 
