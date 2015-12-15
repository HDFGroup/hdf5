/****h* H5Tf/H5Tf
 * PURPOSE
 *  This file contains C stubs for H5T Fortran APIs
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

/****if* H5Tf/h5topen_c
 * NAME
 *  h5topen_c
 * PURPOSE
 *  Call H5Topen2 to open a datatype
 * INPUTS
 *  loc_id - file or group identifier
 *  name - name of the datatype within file or  group
 *  namelen - name length
 *  tapl_id - datatype access property list identifier
 * OUTPUTS
 *  type_id - dataset identifier
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/
int_f
h5topen_c (hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *tapl_id)
/******/
{
    char *c_name = NULL;
    hid_t c_type_id;
    int ret_value = -1;

    /*
     * Convert FORTRAN name to C name
     */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        goto done;

    /*
     * Call H5Topen2 function.
     */
    if((c_type_id = H5Topen2((hid_t)*loc_id, c_name, (hid_t)*tapl_id)) < 0)
        goto done;
    *type_id = (hid_t_f)c_type_id;

    ret_value = 0;

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/****if* H5Tf/h5tcommit_c
 * NAME
 *  h5tcommit_c
 * PURPOSE
 *  Call H5Tcommit2 to commit a datatype
 * INPUTS
 *  loc_id - file or group identifier
 *  name - name of the datatype within file or  group
 *  namelen - name length
 *  type_id - dataset identifier
 *  lcpl_id - Link creation property list
 *  tcpl_id - Datatype creation property list
 *  tapl_id - Datatype access property list
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 *              - Added passing optional parameters for version 1.8
 *                M. Scot Breitenfeld
 * SOURCE
*/
int_f
h5tcommit_c(hid_t_f *loc_id, _fcd name, int_f *namelen, hid_t_f *type_id,
	     hid_t_f *lcpl_id, hid_t_f *tcpl_id, hid_t_f *tapl_id)
/******/
{
    char *c_name = NULL;
    int ret_value = -1;

    /* Convert FORTRAN name to C name */
    if(NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        goto done;

    /* Call H5Tcommit2 function */
    if(H5Tcommit2((hid_t)*loc_id, c_name, (hid_t)*type_id, (hid_t)*lcpl_id, (hid_t)*tcpl_id, (hid_t)*tapl_id) < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name)
        HDfree(c_name);
    return ret_value;
}

/****if* H5Tf/h5tclose_c
 * NAME
 *  h5tclose_c
 * PURPOSE
 *  Call H5Tclose to close the datatype
 * INPUTS
 *  type_id - identifier of the datatype to be closed
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tclose_c ( hid_t_f *type_id )
/******/
{
  int ret_value = 0;
  hid_t c_type_id;

  c_type_id = *type_id;
  if ( H5Tclose(c_type_id) < 0  ) ret_value = -1;
  return ret_value;
}

/****if* H5Tf/h5tcopy_c
 * NAME
 *  h5tcopy_c
 * PURPOSE
 *  Call H5Tcopy to copy a datatype
 * INPUTS
 *  type_id - identifier of the datatype to be copied
 * OUTPUTS
 *  new_type_id - identifier of the new datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tcopy_c ( hid_t_f *type_id , hid_t_f *new_type_id)
/******/
{
  int ret_value = 0;
  hid_t c_type_id;
  hid_t c_new_type_id;

  c_type_id = *type_id;
  c_new_type_id = H5Tcopy(c_type_id);
  if ( c_new_type_id < 0  ) ret_value = -1;
  *new_type_id = (hid_t_f)c_new_type_id;
  return ret_value;
}

/****if* H5Tf/h5tequal_c
 * NAME
 *  h5tequal_c
 * PURPOSE
 *  Call H5Tequal to copy a datatype
 * INPUTS
 *  type1_id - datatype identifier
 *  type2_id - datatype identifier
 * OUTPUTS
 *  c_flag - flag; indicates if two datatypes are equal or not.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, February 22, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tequal_c ( hid_t_f *type1_id , hid_t_f *type2_id, int_f *c_flag)
/******/
{
  int ret_value = -1;
  hid_t c_type1_id, c_type2_id;
  htri_t status;

  c_type1_id = *type1_id;
  c_type2_id = *type2_id;
  status = H5Tequal(c_type1_id, c_type2_id);
  if ( status < 0  ) return ret_value;
  *c_flag = (int_f)status;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_class_c
 * NAME
 *  h5tget_class_c
 * PURPOSE
 *  Call H5Tget_class to determine the datatype class
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  classtype - class type; possible values are:
 *               H5T_NO_CLASS_F (-1)
 *               H5T_INTEGER_F (0)
 *               H5T_FLOAT_F (1)
 *               H5T_TIME_F (2)
 *               H5T_STRING_F (3)
 *               H5T_BITFIELD_F (4)
 *               H5T_OPAQUE_F (5)
 *               H5T_COMPOUNDF (6)
 *               H5T_REFERENCE_F (7)
 *               H5T_ENUM_F (8)
 *               H5T_VLEN_F (9)
 *               H5T_ARRAY_F (10)
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_class_c ( hid_t_f *type_id , int_f *classtype)
/******/
{
  int ret_value = 0;
  hid_t c_type_id;
  H5T_class_t c_classtype;

  c_type_id = *type_id;
  c_classtype = H5Tget_class(c_type_id);
  if (c_classtype == H5T_NO_CLASS ) {
      /* *classtype = H5T_NO_CLASS_F; */
      *classtype = (int_f)H5T_NO_CLASS;
       ret_value = -1;
       return ret_value;
  }
  *classtype = c_classtype;
/*
  if (c_classtype == H5T_INTEGER)   *classtype = H5T_INTEGER_F;
  if (c_classtype == H5T_FLOAT)     *classtype = H5T_FLOAT_F;
  if (c_classtype == H5T_TIME)      *classtype = H5T_TIME_F;
  if (c_classtype == H5T_STRING)    *classtype = H5T_STRING_F;
  if (c_classtype == H5T_BITFIELD)  *classtype = H5T_BITFIELD_F;
  if (c_classtype == H5T_OPAQUE)    *classtype = H5T_OPAQUE_F;
  if (c_classtype == H5T_COMPOUND)  *classtype = H5T_COMPOUND_F;
  if (c_classtype == H5T_REFERENCE) *classtype = H5T_REFERENCE_F;
  if (c_classtype == H5T_ENUM)      *classtype = H5T_ENUM_F;
*/
  return ret_value;
}

/****if* H5Tf/h5tget_order_c
 * NAME
 *  h5tget_order_c
 * PURPOSE
 *  Call H5Tget_order to determine byte order
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  order; possible values are:
 *              H5T_ORDER_LE_F (0)
 *              H5T_ORDER_BE_F (1)
 *              H5T_ORDER_VAX_F (2)
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_order_c ( hid_t_f *type_id , int_f *order)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_order_t c_order;

  c_type_id = *type_id;
  c_order = H5Tget_order(c_type_id);
  if ( c_order < 0  ) return ret_value;
  *order = (int_f)c_order;
   ret_value = 0;
/*
  if ( c_order == H5T_ORDER_LE)  *order = H5T_ORDER_LE_F;
  if ( c_order == H5T_ORDER_BE)  *order = H5T_ORDER_BE_F;
  if ( c_order == H5T_ORDER_VAX) *order = H5T_ORDER_VAX_F;
*/
  return ret_value;
}


/****if* H5Tf/h5tset_order_c
 * NAME
 *  h5tset_order_c
 * PURPOSE
 *  Call H5Tset_order to set byte order
 * INPUTS
 *  type_id - identifier of the dataspace
 *  order; possible values are:
 *              H5T_ORDER_LE_F (0)
 *              H5T_ORDER_BE_F (1)
 *              H5T_ORDER_VAX_F (2)
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_order_c ( hid_t_f *type_id , int_f *order)
/******/
{
  int ret_value = 0;
  hid_t c_type_id;
  H5T_order_t c_order;
  herr_t status;
  c_order = (H5T_order_t)*order;
/*
  if ( *order == H5T_ORDER_LE_F) c_order = H5T_ORDER_LE;
  if ( *order == H5T_ORDER_BE_F) c_order = H5T_ORDER_BE;
  if ( *order == H5T_ORDER_VAX_F) c_order = H5T_ORDER_VAX;
*/
  c_type_id = *type_id;
  status = H5Tset_order(c_type_id, c_order);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}

/****if* H5Tf/h5tget_size_c
 * NAME
 *  h5tget_size_c
 * PURPOSE
 *  Call H5Tget_size to get size of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  size (in bytes)
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_size_c ( hid_t_f *type_id , size_t_f *size)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_size;

  c_type_id = *type_id;
  c_size = H5Tget_size(c_type_id);
  if ( c_size == 0  ) return ret_value;
  *size = (size_t_f)c_size ;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_size_c
 * NAME
 *  h5tset_size_c
 * PURPOSE
 *  Call H5Tget_size to get size of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  size (in bytes)
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Saturday, August 14, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_size_c ( hid_t_f *type_id , size_t_f *size)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_size;
  herr_t status;

  c_size = (size_t)*size;
  c_type_id = *type_id;
  status = H5Tset_size(c_type_id, c_size);
  if ( status < 0  ) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_precision_c
 * NAME
 *  h5tget_precision_c
 * PURPOSE
 *  Call H5Tget_precision to get precision of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  precision -  number of significant bits
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Tuesday, January 25, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_precision_c ( hid_t_f *type_id , size_t_f *precision)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_precision;

  c_type_id = *type_id;
  c_precision = H5Tget_precision(c_type_id);
  if ( c_precision == 0  ) return ret_value;
  *precision = (size_t_f)c_precision ;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_precision_c
 * NAME
 *  h5tset_precision_c
 * PURPOSE
 *  Call H5Tset_precision to set precision of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 *  precision -  number of significant bits
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Tuesday, January 25, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_precision_c ( hid_t_f *type_id , size_t_f *precision)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_precision;
  herr_t status;

  c_type_id = *type_id;
  c_precision = (size_t)*precision;
  status = H5Tset_precision(c_type_id, c_precision);
  if ( status < 0 ) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_offset_c
 * NAME
 *  h5tget_offset_c
 * PURPOSE
 *  Call H5Tget_offset to get bit offset of the first
 *  significant bit of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  offset - bit offset of the first significant bit
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Tuesday, January 25, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_offset_c ( hid_t_f *type_id , size_t_f *offset)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  int c_offset;

  c_type_id = *type_id;
  c_offset = H5Tget_offset(c_type_id);
  if ( c_offset < 0  ) return ret_value;

  *offset = (size_t_f)c_offset ;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_offset_c
 * NAME
 *  h5tset_offset_c
 * PURPOSE
 *  Call H5Tset_offset to set bit offset of the first
 *  significant bit of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 *  offset - bit offset of the first significant bit
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Tuesday, January 25, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_offset_c ( hid_t_f *type_id , size_t_f *offset)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_offset;
  herr_t status;

  c_offset = (size_t)*offset;
  c_type_id = *type_id;
  status = H5Tset_offset(c_type_id, c_offset);
  if ( status < 0 ) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_pad_c
 * NAME
 *  h5tget_pad_c
 * PURPOSE
 *  Call H5Tget_pad to get the padding type of the least and
 *  most-significant bit padding
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  lsbpad - padding type of the least significant bit
 *  msbpad - padding type of the least significant bit
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_pad_c ( hid_t_f *type_id , int_f * lsbpad, int_f * msbpad)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  H5T_pad_t c_lsb, c_msb;

  c_type_id = *type_id;
  status = H5Tget_pad(c_type_id, &c_lsb, &c_msb);
  if ( status < 0  ) return ret_value;

  *lsbpad = (int_f) c_lsb;
  *msbpad = (int_f) c_msb;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_pad_c
 * NAME
 *  h5tset_pad_c
 * INPUTS
 *  type_id - identifier of the dataspace
 * PURPOSE
 *  Call H5Tset_pad to set the padding type of the least and
 *  most-significant bit padding
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 *  lsbpad - padding type of the least significant bit
 *  msbpad - padding type of the least significant bit
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_pad_c ( hid_t_f *type_id, int_f * lsbpad, int_f* msbpad )
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  H5T_pad_t c_lsb, c_msb;

  c_type_id = *type_id;
  c_lsb = (H5T_pad_t)*lsbpad;
  c_msb = (H5T_pad_t)*msbpad;
  status = H5Tset_pad(c_type_id, c_lsb, c_msb);
  if ( status < 0 ) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_sign_c
 * NAME
 *  h5tget_sign_c
 * PURPOSE
 *  Call H5Tget_sign to get sign type for an integer type
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  sign - sign type for an integer type
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_sign_c ( hid_t_f *type_id , int_f *sign)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_sign_t c_sign;

  c_type_id = *type_id;
  c_sign = H5Tget_sign(c_type_id);
  if ( c_sign == -1  ) return ret_value;
  *sign = (int_f)c_sign ;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_sign_c
 * NAME
 *  h5tset_sign_c
 * PURPOSE
 *  Call H5Tset_sign to set sign type for an integer type
 * INPUTS
 *  type_id - identifier of the dataspace
 *  sign - sign type for an integer typ
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_sign_c ( hid_t_f *type_id , int_f* sign)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_sign_t c_sign;
  herr_t status;

  c_type_id = *type_id;
  c_sign = (H5T_sign_t)*sign;
  status = H5Tset_sign(c_type_id, c_sign);
  if ( status < 0 ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_fields_c
 * NAME
 *  h5tget_fields_c
 * PURPOSE
 *  Call H5Tget_fields to get floating point datatype
 *  bit field information
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  epos -  exponent bit-position
 *  esize - size of exponent in bits
 *  mpos -  mantissa bit-position
 *  msize -  size of mantissa in bits
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, January 27, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_fields_c ( hid_t_f *type_id , size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  size_t  c_spos, c_epos, c_esize, c_mpos, c_msize;

  c_type_id = *type_id;
  status = H5Tget_fields(c_type_id, &c_spos, &c_epos, &c_esize, &c_mpos, &c_msize);
  if ( status < 0  ) return ret_value;
  *spos = (size_t_f) c_spos;
  *epos = (size_t_f) c_epos;
  *esize = (size_t_f) c_esize;
  *mpos = (size_t_f) c_mpos;
  *msize = (size_t_f) c_msize;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_fields_c
 * NAME
 *  h5tset_fields_c
 * PURPOSE
 *  Call H5Tset_fields to set floating point datatype
 *  bit field information
 * INPUTS
 *  type_id - identifier of the dataspace
 *  epos -  exponent bit-position
 *  esize - size of exponent in bits
 *  mpos -  mantissa bit-position
 *  msize -  size of mantissa in bits
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_fields_c ( hid_t_f *type_id, size_t_f *spos, size_t_f *epos, size_t_f* esize, size_t_f* mpos, size_t_f* msize)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  size_t  c_spos, c_epos, c_esize, c_mpos, c_msize;

  c_spos = (size_t)*spos;
  c_epos = (size_t)*epos;
  c_esize = (size_t)*esize;
  c_mpos = (size_t)*mpos;
  c_msize = (size_t)*msize;
  c_type_id = *type_id;
  status = H5Tset_fields(c_type_id, c_spos, c_epos, c_esize, c_mpos, c_msize);
  if ( status < 0 ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_ebias_c
 * NAME
 *  h5tget_ebias_c
 * PURPOSE
 *  Call H5Tget_ebias to get  exponent bias of a
 *  floating-point type of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  ebias - exponent bias of a floating-point type of the datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Friday, January 27, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_ebias_c ( hid_t_f *type_id , size_t_f *ebias)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_ebias;

  c_type_id = *type_id;
  c_ebias = H5Tget_ebias(c_type_id);
  if ( c_ebias == 0  ) return ret_value;

  *ebias = (size_t_f)c_ebias;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_ebias_c
 * NAME
 *  h5tset_ebias_c
 * PURPOSE
 *  Call H5Tset_ebias to set exponent bias of a
 *  floating-point type of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 *  ebias - exponent bias of a floating-point type of the datatyp
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Friday, January 27, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_ebias_c ( hid_t_f *type_id , size_t_f *ebias)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  size_t c_ebias;
  herr_t status;

  c_type_id = *type_id;
  c_ebias = (size_t)*ebias;
  status = H5Tset_ebias(c_type_id, c_ebias);
  if ( status < 0  ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_norm_c
 * NAME
 *  h5tget_norm_c
 * PURPOSE
 *  Call H5Tget_norm to get mantissa normalization
 *  of a floating-point datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  norm - mantissa normalization of a floating-point type
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Friday, January 27, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_norm_c ( hid_t_f *type_id , int_f *norm)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_norm_t  c_norm;

  c_type_id = *type_id;
  c_norm = H5Tget_norm(c_type_id);
  if ( c_norm == H5T_NORM_ERROR  ) return ret_value;

  *norm = (int_f)c_norm;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_norm_c
 * NAME
 *  h5tset_norm_c
 * PURPOSE
 *  Call H5Tset_norm to set mantissa normalization of
 *  floating-point type of the datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 *  norm -  mantissa normalization of a floating-point type
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Friday, January 27, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_norm_c ( hid_t_f *type_id , int_f *norm)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_norm_t  c_norm;
  herr_t status;

  c_type_id = *type_id;
  c_norm = (H5T_norm_t)*norm;
  status = H5Tset_norm(c_type_id, c_norm);
  if ( status < 0  ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_inpad_c
 * NAME
 *  h5tget_inpad_c
 * PURPOSE
 *  Call H5Tget_inpad to get the padding type for
 *  unused bits in floating-point datatypes
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  padtype - padding type for
 *  unused bits in floating-point datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_inpad_c ( hid_t_f *type_id , int_f * padtype)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_pad_t c_padtype;

  c_type_id = *type_id;
  c_padtype = H5Tget_inpad(c_type_id);
  if ( c_padtype == H5T_PAD_ERROR  ) return ret_value;

  *padtype = (int_f) c_padtype;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_inpad_c
 * NAME
 *  h5tset_inpad_c
 * INPUTS
 *  type_id - identifier of the dataspace
 * PURPOSE
 *  Call H5Tset_inpad to set the padding type
 *  unused bits in floating-point datatype
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 *  padtype - padding type for unused bits
 *  in floating-point datatypes
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_inpad_c ( hid_t_f *type_id, int_f * padtype)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  H5T_pad_t c_padtype;

  c_type_id = *type_id;
  c_padtype = (H5T_pad_t)*padtype;
  status = H5Tset_inpad(c_type_id, c_padtype);
  if ( status < 0 ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_cset_c
 * NAME
 *  h5tget_cset_c
 * PURPOSE
 *  Call H5Tget_cset to get character set
 *  type of a string datatype
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  cset - character set type of a string datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_cset_c ( hid_t_f *type_id , int_f * cset)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_cset_t  c_cset;

  c_type_id = *type_id;
  c_cset = H5Tget_cset(c_type_id);
  if ( c_cset == H5T_CSET_ERROR  ) return ret_value;

  *cset = (int_f) c_cset;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_cset_c
 * NAME
 *  h5tset_cset_c
 * INPUTS
 *  type_id - identifier of the dataspace
 * PURPOSE
 *  Call H5Tset_cset to set character set
 *  type of a string datatype
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 *  cset -  character set type of a string datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_cset_c ( hid_t_f *type_id, int_f * cset)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  H5T_cset_t c_cset;

  c_type_id = *type_id;
  c_cset = (H5T_cset_t)*cset;
  status = H5Tset_cset(c_type_id, c_cset);

  if ( status < 0 ) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_strpad_c
 * NAME
 *  h5tget_strpad_c
 * PURPOSE
 *  Call H5Tget_strpad to get string padding method
 *  for a string datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  strpad - string padding method for a string datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tget_strpad_c ( hid_t_f *type_id , int_f * strpad)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  H5T_str_t  c_strpad;

  c_type_id = *type_id;
  c_strpad = H5Tget_strpad(c_type_id);
  if ( c_strpad == H5T_STR_ERROR  ) return ret_value;

  *strpad = (int_f) c_strpad;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_strpad_c
 * NAME
 *  h5tset_strpad_c
 * INPUTS
 *  type_id - identifier of the dataspace
 * PURPOSE
 *  Call H5Tset_strpad to set string padding method
 *  for a string datatype
 *
 * INPUTS
 *  type_id - identifier of the dataspace
 *  strpad - string padding method for a string datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tset_strpad_c ( hid_t_f *type_id, int_f * strpad)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;
  H5T_str_t c_strpad;

  c_type_id = *type_id;
  c_strpad = (H5T_str_t)*strpad;
  status = H5Tset_strpad(c_type_id, c_strpad);
  if ( status < 0 ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_nmembers_c
 * NAME
 *  h5tget_nmembers_c
 * PURPOSE
 *  Call H5Tget_nmembers to get number of fields
 *  in a compound datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  num_members - number of fields in a compound datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_nmembers_c ( hid_t_f *type_id , int_f * num_members)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;

  c_type_id = *type_id;
  *num_members = (int_f)H5Tget_nmembers(c_type_id);
  if (*num_members < 0  ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_member_name_c
 * NAME
 *  h5tget_member_name_c
 * PURPOSE
 *  Call H5Tget_member_name to get name
 *  of a compound datatype
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  member_name - name of a field of a compound datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *  Elena Pourmal
 *  Added namelen parameter to return length of the name to Fortran user
 * SOURCE
*/

int_f
h5tget_member_name_c ( hid_t_f *type_id ,int_f* idx, _fcd member_name, int_f *namelen)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  unsigned c_index;
  char *c_name;

  c_type_id = *type_id;
  c_index = (unsigned)*idx;
  c_name = H5Tget_member_name(c_type_id, c_index);
  if (c_name == NULL ) return ret_value;

  HD5packFstring(c_name, _fcdtocp(member_name), strlen(c_name));
  *namelen = (int_f)strlen(c_name);
  H5free_memory(c_name);
  ret_value = 0;
  return ret_value;
}
/****if* H5Tf/h5tget_member_index_c
 * NAME
 *  h5tget_member_index_c
 * PURPOSE
 *  Call H5Tget_member_index to get an index of
 *  the specified datatype filed or member.
 * INPUTS
 *  type_id - datatype identifier
 *  name - name of the datatype within file or  group
 *  namelen - name length
 * OUTPUTS
 *  index - 0-based index
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Thursday, September 26, 2002
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tget_member_index_c (hid_t_f *type_id, _fcd name, int_f *namelen, int_f *idx)
/******/
{
     int ret_value = -1;
     char *c_name;
     hid_t c_type_id;
     int c_index;

     /*
      * Convert FORTRAN name to C name
      */
     c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Tget_member_index function.
      */
     c_type_id = (hid_t)*type_id;
     c_index = H5Tget_member_index(c_type_id, c_name);

     if (c_index < 0) goto DONE;
     *idx = (int_f)c_index;
DONE:
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}


/****if* H5Tf/h5tget_member_offset_c
 * NAME
 *  h5tget_member_offset_c
 * PURPOSE
 *  Call H5Tget_member_offset to get byte offset of the
 *  beginning of a field within a compound datatype with
 *  respect to the beginning of the compound data type datum
 * INPUTS
 *  type_id - identifier of the dataspace
 *  member_no - Number of the field whose offset is requested
 * OUTPUTS
 *  offset - byte offset of the the beginning of the field of
 *  a compound datatype
 * RETURNS
 *  always 0
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_member_offset_c ( hid_t_f *type_id ,int_f* member_no, size_t_f * offset)
/******/
{
  int ret_value = -1;
  size_t c_offset;

  c_offset = H5Tget_member_offset((hid_t)*type_id, (unsigned)*member_no);
  *offset = (size_t_f)c_offset;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_array_dims_c
 * NAME
 *  h5tget_array_dims_c
 * PURPOSE
 *  Call H5Tget_array_dims2 to get
 *  dimensions of array datatype
 * INPUTS
 *  type_id - identifier of the array datatype
 * OUTPUTS
 *  dims -  dimensions(sizes of dimensions) of the array
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Thursday, November 16, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_array_dims_c ( hid_t_f *type_id , hsize_t_f * dims)
/******/
{
    hsize_t c_dims[H5S_MAX_RANK];
    int rank, i;
    int ret_value = -1;

    if((rank = H5Tget_array_ndims((hid_t)*type_id)) < 0)
        goto DONE;

    if(H5Tget_array_dims2((hid_t)*type_id, c_dims) < 0)
        goto DONE;

    for(i = 0; i < rank; i++)
        dims[(rank - i) - 1] = (hsize_t_f)c_dims[i];

    ret_value = 0;

DONE:
    return ret_value;
}

/****if* H5Tf/h5tget_array_ndims_c
 * NAME
 *  h5tget_array_ndims_c
 * PURPOSE
 *  Call H5Tget_array_ndims to get number
 *  of dimensions of array datatype
 * INPUTS
 *  type_id - identifier of the array datatype
 * OUTPUTS
 *  ndims -  number of dimensions of the array
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Thursday, November 16, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_array_ndims_c ( hid_t_f *type_id , int_f * ndims)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  int c_ndims;

  c_type_id = (hid_t)*type_id;
  c_ndims = H5Tget_array_ndims(c_type_id);
  if (c_ndims < 0) return ret_value;

  *ndims = (int_f)c_ndims;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_super_c
 * NAME
 *  h5tget_super_c
 * PURPOSE
 *  Call H5Tget_super to get base datatype from which
 *  datatype was derived
 * INPUTS
 *  type_id - identifier of the array datatype
 * OUTPUTS
 *  base_type_id - base datatype identifier
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Thursday, November 16, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_super_c ( hid_t_f *type_id , hid_t_f *base_type_id)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  hid_t c_base_type_id;

  c_type_id = (hid_t)*type_id;
  c_base_type_id = H5Tget_super(c_type_id);
  if (c_base_type_id < 0) return ret_value;

  *base_type_id = (hid_t_f)c_base_type_id;
  ret_value = 0;
  return ret_value;
}


/****if* H5Tf/h5tget_member_type_c
 * NAME
 *  h5tget_member_type_c
 * PURPOSE
 *  Call H5Tget_member_type to get the identifier of a copy of
 *  the datatype of the field
 * INPUTS
 *  type_id - identifier of the datatype
 *  field_idx - Field index (0-based) of the field type to retrieve
 * OUTPUTS
 *  datatype - identifier of a copy of the datatype of the field
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_member_type_c ( hid_t_f *type_id ,int_f* field_idx, hid_t_f * datatype)
/******/
{
  int ret_value = -1;

  *datatype = (hid_t_f)H5Tget_member_type((hid_t)*type_id, (unsigned)*field_idx);
  if(*datatype < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}


/****if* H5Tf/h5tcreate_c
 * NAME
 *  h5tcreate_c
 * PURPOSE
 *  Call H5Tcreate to create a datatype
 * INPUTS
 *  cls - class type
 *  size - size of the class memeber
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Thursday, February 17, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tcreate_c(int_f *cls, size_t_f *size, hid_t_f *type_id)
/******/
{
  int ret_value = -1;
  H5T_class_t c_class;
  size_t c_size;

  c_size =(size_t) *size;
  c_class = (H5T_class_t) *cls;

  *type_id = (hid_t_f)H5Tcreate(c_class, c_size);
  if(*type_id < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tinsert_c
 * NAME
 *  h5tinsert_c
 * PURPOSE
 *  Call H5Tinsert to adds another member to the compound datatype
 * INPUTS
 *  type_id  - identifier of the datatype
 *  name     - Name of the field to insert
 *  namelen - length of the name
 *  offset   - Offset in memory structure of the field to insert
 *  field_id - datatype identifier of the new member
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tinsert_c(hid_t_f *type_id, _fcd name, int_f* namelen, size_t_f *offset, hid_t_f * field_id)
/******/
{
  int ret_value = -1;
  char* c_name;
  herr_t error;

  c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
  if (c_name == NULL) return ret_value;

  error = H5Tinsert((hid_t)*type_id, c_name, (size_t)*offset, (hid_t)*field_id);

  HDfree(c_name);
  if(error < 0) return ret_value;
  ret_value = 0;
  return ret_value;
}


/****if* H5Tf/h5tpack_c
 * NAME
 *  h5tpack_c
 * PURPOSE
 *  Call H5Tpack tor ecursively remove padding from
 *  within a compound datatype to make it more efficient
 *  (space-wise) to store that data
 * INPUTS
 *  type_id - identifier of the datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tpack_c(hid_t_f * type_id)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  herr_t status;

  c_type_id = *type_id;
  status = H5Tpack(c_type_id);
  if (status < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tarray_create_c
 * NAME
 *  h5tarray_create_c
 * PURPOSE
 *  Call H5Tarray_create2 to create array datatype
 * INPUTS
 *  base_id - identifier of array base datatype
 *  rank - array's rank
 *  dims - Size of new member array
 *  type_id - identifier of the array datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Thursday, November 16, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tarray_create_c(hid_t_f * base_id, int_f *rank, hsize_t_f* dims, hid_t_f* type_id)
/******/
{
    hsize_t c_dims[H5S_MAX_RANK];
    hid_t c_type_id;
    unsigned u;                 /* Local index variable */
    int ret_value = -1;


    /*
     * Transpose dimension arrays because of C-FORTRAN storage order
     */
    for(u = 0; u < (unsigned)*rank ; u++)
      c_dims[u] =  (hsize_t)dims[((unsigned)*rank - u) - 1];

    if((c_type_id = H5Tarray_create2((hid_t)*base_id, (unsigned)*rank, c_dims)) < 0)
        goto DONE;

    *type_id = (hid_t_f)c_type_id;
    ret_value = 0;

DONE:
    return ret_value;
}


/****if* H5Tf/h5tenum_create_c
 * NAME
 *  h5tenum_create_c
 * PURPOSE
 *  Call H5Tenum_create to create a new enumeration datatype
 * INPUTS
 *  parent_id - Datatype identifier for the base datatype
 * OUTPUTS
 *  new_type_id - datatype identifier for the new
 *  enumeration datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Tuesday, February 15, 1999
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tenum_create_c ( hid_t_f *parent_id , hid_t_f *new_type_id)
/******/
{
  int ret_value = 0;
  hid_t c_new_type_id;

  c_new_type_id = H5Tenum_create((hid_t)*parent_id);
  if ( c_new_type_id < 0  ) ret_value = -1;

  *new_type_id = (hid_t_f)c_new_type_id;
  return ret_value;
}

/****if* H5Tf/h5tenum_insert_c
 * NAME
 *  h5tenum_insert_c
 * PURPOSE
 *  Call H5Tenum_insert to insert a new enumeration datatype member.
 * INPUTS
 *  type_id - identifier of the datatype
 *  name    - Name of  the new member
 *  namelen - length of the name
 *  value   - value of the new member
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *  'value' is no longer cast into an int. If the user passes in an 8 byte integer then
 *  it should not be cast to an int (which might be 4 bytes). Instead the value
 *  is written as the size of an int_f.
 * SOURCE
*/

int_f
h5tenum_insert_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value)
/******/
{
  int ret_value = -1;
  char* c_name;
  herr_t error;
  int_f c_value;

  c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
  if (c_name == NULL) return ret_value;

  c_value = *value;
  error = H5Tenum_insert((hid_t)*type_id, c_name, &c_value);

  HDfree(c_name);
  if(error < 0) return ret_value;

  ret_value = 0;
  return ret_value;
}


/****if* H5Tf/h5tenum_nameof_c
 * NAME
 *  h5tenum_nameof_c
 * PURPOSE
 *  Call H5Tenum_nameof to find the symbol name that corresponds to
 *  the specified value of the enumeration datatype type
 * INPUTS
 *  type_id - identifier of the datatype
 *  namelen - length of the name
 *  value - value of the enumeration datatype
 *  Output:      name  - Name of  the enumeration datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tenum_nameof_c(hid_t_f *type_id, int_f* value, _fcd name, size_t_f* namelen)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  char* c_name;
  size_t c_namelen;
  herr_t error;
  int_f c_value;
  c_value = *value;
  c_namelen = ((size_t)*namelen) +1;
  c_name = (char *)HDmalloc(sizeof(char)*c_namelen);
  c_type_id = (hid_t)*type_id;
  error = H5Tenum_nameof(c_type_id, &c_value, c_name, c_namelen);
  HD5packFstring(c_name, _fcdtocp(name), strlen(c_name));
  HDfree(c_name);

  if(error < 0) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tenum_valueof_c
 * NAME
 *  h5tenum_valueof_c
 * PURPOSE
 *  Call H5Tenum_valueof to find the value of that corresponds to
 *  the specified name of the enumeration datatype type
 * INPUTS
 *  type_id - identifier of the datatype
 *  name - Name of  the enumeration datatype
 *  namelen - length of name
 *  Output:      value  - value of the enumeration datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tenum_valueof_c(hid_t_f *type_id, _fcd name, int_f* namelen, int_f* value)
/******/
{
  int ret_value = -1;
  char* c_name;
  herr_t error;
  c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
  if (c_name == NULL) return ret_value;

  error = H5Tenum_valueof((hid_t)*type_id, c_name, value);
  HDfree(c_name);

  if(error < 0) return ret_value;
  ret_value = 0;
  return ret_value;
}


/****if* H5Tf/h5tget_member_value_c
 * NAME
 *  h5tget_member_value_c
 * PURPOSE
 *  Call H5Tget_member_value to get the value of an
 *  enumeration datatype member
 * INPUTS
 *  type_id - identifier of the datatype
 *  member_no - Number of the enumeration datatype member.
 *  Output:      value  - value of the enumeration datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Thursday, February 3, 2000
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_member_value_c(hid_t_f *type_id, int_f* member_no, int_f* value)
/******/
{
  int ret_value = -1;
  int c_value;
  herr_t error;

  error = H5Tget_member_value((hid_t)*type_id, (unsigned)*member_no, &c_value);
  if(error < 0) return ret_value;

  *value = (int_f)c_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tset_tag_c
 * NAME
 *  h5tset_tag_c
 * INPUTS
 *  type_id - identifier of the dataspace
 * PURPOSE
 *  Call H5Tset_tag to set an opaque datatype tag
 * INPUTS
 *  type_id - identifier of the dataspace
 *  tag -  Unique ASCII string with which the opaque
 *  datatype is to be tagged
 *  namelen - length of tag
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tset_tag_c(hid_t_f* type_id, _fcd tag, int_f* namelen)
/******/
{
  int ret_value = -1;
  herr_t status;
  char* c_tag;

  c_tag = (char *)HD5f2cstring(tag, (size_t)*namelen);

  status = H5Tset_tag((hid_t)*type_id, c_tag);
  HDfree(c_tag);
  if ( status < 0 ) return ret_value;

  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tget_tag_c
 * NAME
 *  h5tget_tag_c
 * PURPOSE
 *  Call H5Tset_tag to set an opaque datatype tag
 * INPUTS
 *  type_id - identifier of the datatype
 * OUTPUTS
 *  tag -  Unique ASCII string with which the opaque
 *  datatype is to be tagged
 *  taglen - length of tag
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  XIANGYANG SU
 *  Wednesday, January 26, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tget_tag_c(hid_t_f* type_id, _fcd tag, size_t_f* tag_size, int_f* taglen)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  char *c_tag;

  c_type_id = *type_id;
  c_tag = H5Tget_tag(c_type_id);
  if (c_tag == NULL ) return ret_value;

  HD5packFstring(c_tag, _fcdtocp(tag), (size_t)*tag_size);
  *taglen = (int_f)HDstrlen(c_tag);
  H5free_memory(c_tag);
  ret_value = 0;
  return ret_value;
}
/****if* H5Tf/h5tvlen_create_c
 * NAME
 *  h5tvlen_create_c
 * PURPOSE
 *  Call H5Tvlen_create to create VL dtatype
 * INPUTS
 *  type_id - identifier of the base datatype
 * OUTPUTS
 *  vltype_id - identifier of the VL datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Wednesday, October 23, 2002
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tvlen_create_c(hid_t_f* type_id, hid_t_f *vltype_id)
/******/
{
  int ret_value = -1;
  hid_t c_type_id;
  hid_t c_vltype_id;

  c_type_id = (hid_t)*type_id;
  c_vltype_id = H5Tvlen_create(c_type_id);
  if (c_vltype_id < 0 ) return ret_value;
  *vltype_id = (hid_t_f)c_vltype_id;
  ret_value = 0;
  return ret_value;
}
/****if* H5Tf/h5tis_variable_str_c
 * NAME
 *  h5tis_variable_str_c
 * PURPOSE
 *  Call H5Tis_variable_str to detrmine if the datatype
 *  is a variable string.
 * INPUTS
 *  type_id - identifier of the dataspace
 * OUTPUTS
 *  flag - 0 if not VL str, 1 if is not
 *  and negative on failure.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Wednesday, March 12 , 2003
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tis_variable_str_c ( hid_t_f *type_id , int_f *flag )
/******/
{
  int ret_value = 0;
  hid_t c_type_id;
  htri_t status;

  c_type_id = (hid_t)*type_id;
  status = H5Tis_variable_str(c_type_id);
  *flag = (int_f)status;
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}
/****if* H5Tf/h5tget_member_class_c
 * NAME
 *  h5tget_member_class_c
 * PURPOSE
 *  Call H5Tget_member_class to detrmine ithe class of the compound
 *		datatype member
 * INPUTS
 *  type_id - identifier of the dataspace
 *  member_no - member's index
 * OUTPUTS
 *  class - member's class
 *  and negative on failure.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Wednesday, April 6, 2005
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_member_class_c ( hid_t_f *type_id ,  int_f *member_no, int_f *cls )
/******/
{
  int ret_value = 0;
  hid_t c_type_id;
  unsigned c_member_no;
  H5T_class_t c_class;

  c_type_id = (hid_t)*type_id;
  c_member_no = (unsigned)*member_no;
  c_class = H5Tget_member_class(c_type_id, c_member_no);

  if ( c_class == H5T_NO_CLASS  ) ret_value = -1;
  *cls = (int_f)c_class;
  return ret_value;
}

/****if* H5Tf/h5tcommit_anon_c
 * NAME
 *  h5tcommit_anon_c
 * PURPOSE
 *  Call H5Tcommit_anon
 * INPUTS
 *  loc_id - file or group identifier
 *  dtype_id - dataset identifier
 *  tcpl_id - Datatype creation property list
 *  tapl_id - Datatype access property list
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  February 25, 2008
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tcommit_anon_c(hid_t_f *loc_id, hid_t_f *dtype_id,
		  hid_t_f *tcpl_id, hid_t_f *tapl_id)
/******/
{
  int ret_value = -1;

  /* Call H5Tcommit_anon function */
  if(H5Tcommit_anon((hid_t)*loc_id, (hid_t)*dtype_id, (hid_t)*tcpl_id, (hid_t)*tapl_id) < 0)
    goto done;

  ret_value = 0;

 done:
  return ret_value;
}

/****if* H5Tf/h5tcommitted_c
 * NAME
 *  h5tcommitted_c
 * PURPOSE
 *  Call H5Tcommitted
 *  dtype_id - dataset identifier
 * RETURNS
 *  a positive value, for TRUE, if the datatype has been committed,
 *  or 0 (zero), for FALSE, if the datatype has not been committed.
 *		Otherwise returns a negative value.
 * AUTHOR
 *  M. Scot Breitenfeld
 *  February 25, 2008
 * HISTORY
 *
 * SOURCE
*/
int_f
h5tcommitted_c(hid_t_f *dtype_id)
/******/
{
  int_f ret_value;

  /* Call H5Tcommitted function */

  ret_value=(int_f)H5Tcommitted((hid_t)*dtype_id);

  return ret_value;

}

/****if* H5Tf/h5tdecode_c
 * NAME
 *  h5tdecode_c
 * PURPOSE
 *  Call H5Tdecode
 * INPUTS
 *
 *		buf     - Buffer for the data space object to be decoded.
 * OUTPUTS
 *
 *  obj_id  - Object_id (non-negative)
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  April 9, 2008
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tdecode_c ( _fcd buf, hid_t_f *obj_id )
/******/
{
  int ret_value = -1;
  unsigned char *c_buf = NULL;  /* Buffer to hold C string */
  hid_t c_obj_id;

  /*
   * Call H5Tdecode function.
   */

  c_buf = (unsigned char*)buf;

  c_obj_id = H5Tdecode(c_buf);
  if(c_obj_id < 0)
    return ret_value;

  *obj_id = (hid_t_f)c_obj_id;
  ret_value = 0;

  return ret_value;
}

/****if* H5Tf/h5tencode_c
 * NAME
 *  h5tencode_c
 * PURPOSE
 *  Call H5Tencode
 * INPUTS
 *
 *  obj_id - Identifier of the object to be encoded.
 *		 buf - Buffer for the object to be encoded into.
 *  nalloc - The size of the allocated buffer.
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  April 9, 2008
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tencode_c (_fcd buf, hid_t_f *obj_id, size_t_f *nalloc )
/******/
{
  int ret_value = -1;
  unsigned char *c_buf = NULL;          /* Buffer to hold C string */
  size_t c_size;

  /* return just the size of the allocated buffer;
   * equivalent to C routine for which 'name' is set equal to NULL
   */

  if (*nalloc == 0) {

    if(H5Tencode((hid_t)*obj_id, c_buf, &c_size) < 0)
      return ret_value;

    *nalloc = (size_t_f)c_size;

    ret_value = 0;
    return ret_value;
  }

  /*
   * Allocate buffer
   */
  c_size = (size_t)*nalloc;
  if(NULL == (c_buf = (unsigned char *)HDmalloc(c_size)))
    return ret_value;

  /*
   * Call H5Tencode function.
   */
  if(H5Tencode((hid_t)*obj_id, c_buf, &c_size) < 0)
    return ret_value;

  /* copy the C buffer to the FORTRAN buffer.
   * Can not use HD5packFstring because we don't want to
   * eliminate the NUL terminator or pad remaining space
   * with blanks.
   */

  HDmemcpy(_fcdtocp(buf),(char *)c_buf,c_size);

  ret_value = 0;
  if(c_buf)
      HDfree(c_buf);
  return ret_value;
}

/****if* H5Tf/h5tget_create_plist_c
 * NAME
 *  h5tget_create_plist_c
 * PURPOSE
 *  Call H5Tget_create_plist
 * INPUTS
 *  dtype_id          - Datatype identifier
 * OUTPUTS
 *  dtpl_id           - Datatype property list identifier
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  April 9, 2008
 * HISTORY
 * N/A
 * SOURCE
*/

int_f
h5tget_create_plist_c ( hid_t_f *dtype_id,  hid_t_f *dtpl_id)
/******/
{
    int_f ret_value=-1;          /* Return value */

    if ((*dtpl_id = (hid_t_f)H5Tget_create_plist((hid_t)*dtype_id)) < 0)
        return ret_value;

    ret_value = 0;
    return ret_value;
}

/****if* H5Tf/h5tcompiler_conv_c
 * NAME
 *  h5tcompiler_conv_c
 * PURPOSE
 *  Call H5Tcompiler_conv
 * INPUTS
 *
 *  src_id - Identifier for the source datatype.
 *  dst_id - Identifier for the destination datatype.
 * OUTPUTS
 *  c_flag - flag; TRUE for compiler conversion, FALSE for library conversion
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M.Scot Breitenfeld
 *  April 9, 2008
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tcompiler_conv_c ( hid_t_f *src_id, hid_t_f *dst_id, int_f *c_flag)
/******/
{
  int ret_value = -1;
  htri_t status;

  status = H5Tcompiler_conv( (hid_t)*src_id , (hid_t)*dst_id);
  if ( status < 0  ) return ret_value;
  *c_flag = (int_f)status;
  ret_value = 0;
  return ret_value;
}
/****if* H5Tf/h5tget_native_type_c
 * NAME
 *  h5tget_native_type_c
 * PURPOSE
 *  Call H5Tget_native_type
 * INPUTS
 *
 *  dtype_id         - Datatype identifier for the dataset datatype.
 *  direction        - Direction of search.
 * OUTPUTS
 *  native_dtype_id  - The native datatype identifier for the specified dataset datatype
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  June 18, 2008
 * HISTORY
 *
 * SOURCE
*/

int_f
h5tget_native_type_c(hid_t_f *dtype_id, int_f *direction, hid_t_f *native_dtype_id)
/******/
{
  int ret_value = -1;
  hid_t status;

  status = H5Tget_native_type( (hid_t)*dtype_id, (H5T_direction_t)*direction);
  if ( status < 0  ) return ret_value;
  *native_dtype_id = (hid_t_f)status;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tconvert_c
 * NAME
 *  h5tconvert_c
 * PURPOSE
 *  Call H5Tconvert
 * INPUTS
 *
 *  src_id     - Identifier for the source datatype.
 *  dst_id     - Identifier for the destination datatype.
 *  nelmts     - Size of array buf.
 *  buf        - Array containing pre-conversion values.
 *  background - Optional background buffer.
 *  plist_id   - Dataset transfer property list identifier.
 *
 * OUTPUTS
 *  buf        - Array containing post-conversion values.
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  December 8, 2008
 *
 * SOURCE
*/
int_f
h5tconvert_c(hid_t_f *src_id, hid_t_f *dst_id, size_t_f *nelmts, void *buf, void *background, hid_t_f *plist_id)
/******/
{
  int ret_value = -1;
  hid_t status;

  status = H5Tconvert( (hid_t)*src_id, (hid_t)*dst_id, (size_t)*nelmts, buf, background, (hid_t)*plist_id );
  if ( status < 0  ) return ret_value;
  ret_value = 0;
  return ret_value;
}

/****if* H5Tf/h5tenum_insert_ptr_c
 * NAME
 *  /h5tenum_insert_ptr_c
 * PURPOSE
 *  Calls H5Tenum_insert
 * INPUTS
 *  type_id  - Datatype identifier for the enumeration datatype.
 *  name     - Datatype identifier.
 *  value    - Pointer to the value of the new member.
 *
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  M. Scot Breitenfeld
 *  February 6, 2015
 *
 * SOURCE
*/
int_f
h5tenum_insert_ptr_c(hid_t_f *type_id, _fcd name, int_f* namelen, void *value)
/******/
{
  int ret_value = -1;
  hid_t status;
  char *c_name;

  /*
   * Convert FORTRAN name to C name
   */
  c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
  if (c_name == NULL) return ret_value;

  status = H5Tenum_insert( (hid_t)*type_id, c_name, value);

  HDfree(c_name);
  if ( status < 0  ) return ret_value;

  ret_value = 0;
  return ret_value;
}


