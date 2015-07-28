/****h* H5Zf/H5Zf
 * PURPOSE
 *   This file contains C stubs for H5Z Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
*/

#include "H5f90.h"

/****if* H5Zf/h5zunregister_c
 * NAME
 *        h5zunregister_c
 * PURPOSE
 *     Call H5Zunregister to unregister filter
 * INPUTS
 *      filter identifier
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Wednesday, March 12, 2003
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5zunregister_c (int_f *filter)
/******/
{
     int ret_value = -1;
     herr_t status;
     H5Z_filter_t c_filter;

     /*
      * Call H5Zunregister function.
      */
     c_filter = (H5Z_filter_t)*filter;
     status = H5Zunregister(c_filter);
     if (status < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
/****if* H5Zf/h5zfiletr_avail_c
 * NAME
 *        h5zfiletr_avail_c
 * PURPOSE
 *     Call H5Zfilter_avail to find if filter is available
 * INPUTS
 *      filter - filter identifier
 * OUTPUTS
 *     flag - status flag
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *              Wednesday, March 12, 2003
 * HISTORY
 *
 * SOURCE
*/

int_f
nh5zfilter_avail_c ( int_f *filter , int_f *flag )
/******/
{
  int ret_value = 0;
  H5Z_filter_t c_filter;
  htri_t status;

  c_filter = (H5Z_filter_t)*filter;
  status = H5Zfilter_avail(c_filter);
  *flag = (int_f)status;
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/****if* H5Zf/h5zget_filter_info_c
 * NAME
 *        h5zget_filter_info_c
 * PURPOSE
 *     Call H5Zget_filter_info to find if filter has its encoder
 *              and/or its decoder available
 * INPUTS
 *      filter - filter identifier
 * OUTPUTS
 *     flag - status flag
 * RETURNS
 *     0 on success, -1 on failure
 * AUTHOR
 *  Nat Furrer and James Laird
 *              Wednesday, June 16, 2004
 * HISTORY
 *
 * SOURCE
*/

int_f
nh5zget_filter_info_c ( int_f *filter , int_f *flag )
/******/
{
  int ret_value = 0;
  H5Z_filter_t c_filter;
  unsigned int c_flag;

  c_filter = (H5Z_filter_t)*filter;
  ret_value = H5Zget_filter_info(c_filter, &c_flag);
  *flag = (int_f)c_flag;

  return ret_value;
}
