/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This files contains C stubs for H5A Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5acreate_c
 * Purpose:     Call H5Acreate to create an attribute
 * Inputs:      obj_id - object identifier
 *              name - name of the attribute
 *              namelen - name length
 *              type_id - datatype identifier
 *              space_id - dataspace identifier
 *              crt_pr  - identifier of creation property list
 * Outputs:     attr_id - attribute identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5acreate_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *attr_id)
{
    char *c_name=NULL;          /* Buffer to hold C string */
    int_f ret_value=0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
    if ((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

     /*
      * Call H5Acreate function.
      */
    if((*attr_id = (hid_t_f)H5Acreate((hid_t)*obj_id, c_name, (hid_t)*type_id, (hid_t)*space_id, (hid_t)*crt_prp))<0)
        HGOTO_DONE(FAIL);

done:
    if(c_name) HDfree(c_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aopen_name _c
 * Purpose:     Call H5Aopen_name to open an attribute
 * Inputs:      obj_id - object identifier
 *              name - name of the attribute
 *              namelen - name length
 * Outputs:     attr_id - dataset identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_name_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen, hid_t_f *attr_id)
{
    char *c_name=NULL;          /* Buffer to hold C string */
    int_f ret_value=0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
     if ((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

     /*
      * Call H5Aopen function.
      */
     if ((*attr_id = (hid_t_f)H5Aopen_name((hid_t)*obj_id, c_name)) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_name) HDfree(c_name);
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5awritec_c
 * Purpose:     Call h5awrite_c to write a character  attribute
 * Inputs:      attr_id - dataset identifier
 *              mem_type_id - memory datatype identifier
 *              buf      - character data buffer
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday , August 12, 1999
 * Modifications: dims paramete added.
 *                April 4, 2001
 *---------------------------------------------------------------------------*/
int_f
nh5awritec_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}
int_f
nh5awritec_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5awritec_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}



/*----------------------------------------------------------------------------
 * Name:        h5awrite_c
 * Purpose:     Call H5Awrite to write a attribute
 * Inputs:      attr_id - attribute identifier
 *              mem_type_id - memory datatype identifier
 *              buf      - data buffer
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications: dims parameter added
 *                                           April 4, 2001
 *                Added nh5awrite_integer(real,double)_s,1-7 functions to eliminate
 *                complains about wrong parameters types in h5awrite_c function
 *                called by Fortran routines.
 *                                           October 9, 2006 EIP
 *---------------------------------------------------------------------------*/
int_f
nh5awrite_integer_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_integer_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_real_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_double_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
     /*
      * Call h5awrite_c  function.
      */
     return nh5awrite_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5awrite_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED *dims)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Awrite function.
      */
     if (H5Awrite((hid_t)*attr_id, (hid_t)*mem_type_id, buf) < 0)
        HGOTO_DONE(FAIL);

done:
     return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5areadc_c
 * Purpose:     Call h5aread_c to read character  attribute
 * Inputs:      dset_id - dataset identifier
 *              mem_type_id - memory datatype identifier
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Outputs:     buf      - character data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications: dims parameter added.
 *                April 4, 2001
 *                Added nh5areadc_s,1-7 functions to eliminate
 *                complains about wrong parameters types in h5awrite_c function
 *                called by Fortran routines.
 *                                           October 9, 2006 EIP
 *---------------------------------------------------------------------------*/
int_f
nh5areadc_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}
int_f
nh5areadc_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}

int_f
nh5areadc_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}
int_f
nh5areadc_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf, void *dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, _fcdtocp(buf), dims);
}



/*----------------------------------------------------------------------------
 * Name:        h5aread_c
 * Purpose:     Call H5Aread to read an attribute
 * Inputs:      dset_id - dataset identifier
 *              mem_type_id - memory datatype identifier
 *              dims     - array to store dimensions sizes of buf; used only
 *                         by Fortran routine.
 * Outputs:     buf      - data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications: dims paramete added.
 *                April 4, 2001
 *                Added nh5aread_integer(real,double)_s,1-7 functions to eliminate
 *                complains about wrong parameters types in h5awrite_c function
 *                called by Fortran routines.
 *                                           October 9, 2006 EIP
 *---------------------------------------------------------------------------*/
int_f
nh5aread_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aread function.
      */
     if (H5Aread((hid_t)*attr_id, (hid_t)*mem_type_id, buf) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

int_f
nh5aread_integer_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_integer_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_real_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_s_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_1_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_2_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_3_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_4_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_5_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_6_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}

int_f
nh5aread_double_7_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf, void UNUSED * dims)
{
     /*
      * Call h5aread_c  function.
      */
     return nh5aread_c(attr_id, mem_type_id, buf, dims);
}


/*----------------------------------------------------------------------------
 * Name:        h5aclose_c
 * Purpose:     Call H5Aclose to close an attribute
 * Inputs:      attr_id - identifier of an attribute to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5aclose_c ( hid_t_f *attr_id )
{
    int_f ret_value=0;          /* Return value */

    if (H5Aclose((hid_t)*attr_id) < 0)
        HGOTO_DONE(FAIL);

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5adelete_c
 * Purpose:     Call H5Adelete to delete an attribute
 * Inputs:      obj_id - object identifier
 *              name - name of the attribute
 *              namelen - name length
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5adelete_c (hid_t_f *obj_id, _fcd name, size_t_f *namelen)
{
    char *c_name=NULL;          /* Buffer to hold C string */
    int_f ret_value=0;          /* Return value */

     /*
      * Convert FORTRAN name to C name
      */
     if ((c_name = HD5f2cstring(name, (size_t)*namelen)) == NULL)
        HGOTO_DONE(FAIL);

     /*
      * Call H5Adelete function.
      */
     if (H5Adelete((hid_t)*obj_id, c_name) < 0)
         HGOTO_DONE(FAIL);

done:
    if(c_name) HDfree(c_name);
    return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5aopen_idx_c
 * Purpose:     Call H5Aopen_idx to open an attribute
 * Inputs:      obj_id - object identifier
 *              idx    - attribute index ( zero based)
 * Outputs:     attr_id - attribute identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aopen_idx_c (hid_t_f *obj_id, int_f *idx, hid_t_f *attr_id)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aopen_idx function.
      */
     if ((*attr_id = (hid_t_f)H5Aopen_idx((hid_t)*obj_id, (unsigned)*idx)) < 0)
        HGOTO_DONE(FAIL);

done:
     return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5aget_space_c
 * Purpose:     Call H5Aget_space to get attribute's dataspace
 * Inputs:      attr_id - attribute identifier
 * Outputs:     space_id - dataspace identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_space_c (hid_t_f *attr_id, hid_t_f *space_id)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aget_space function.
      */
     if ((*space_id = (hid_t_f)H5Aget_space((hid_t)*attr_id)) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_type_c
 * Purpose:     Call H5Aget_space to get attribute's datatype
 * Inputs:      attr_id - attribute identifier
 * Outputs:     type_id - datatype identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_type_c (hid_t_f *attr_id, hid_t_f *type_id)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aget_type function.
      */
     if ((*type_id = (hid_t_f)H5Aget_type((hid_t)*attr_id)) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_num_attrs_c
 * Purpose:     Call H5Aget_num_attrs to determine number of
 *              attributes of an object
 * Inputs:      obj_id - object identifier
 *              attr_num - number of attributes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_num_attrs_c (hid_t_f *obj_id, int_f *attr_num)
{
    int_f ret_value=0;          /* Return value */

     /*
      * Call H5Aget_num_attrs function.
      */
     if ((*attr_num = (int_f)H5Aget_num_attrs((hid_t)*obj_id)) < 0)
         HGOTO_DONE(FAIL);

done:
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aget_name_c
 * Purpose:     Call H5Aget_name to get attribute's name
 * Inputs:      attr_id - attribute identifier
 *              bufsize -size of the buffer
 * Outputs:     buf - buffer to hold the name
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aget_name_c(hid_t_f *attr_id, size_t_f *bufsize, _fcd buf)
{
    char *c_buf=NULL;           /* Buffer to hold C string */
    int_f ret_value=0;          /* Return value */

     /*
      * Allocate buffer to hold name of an attribute
      */
     if ((c_buf = HDmalloc((size_t)*bufsize +1)) == NULL)
         HGOTO_DONE(FAIL);

     /*
      * Call H5Aget_name function
      */
     if ((ret_value = (int_f)H5Aget_name((hid_t)*attr_id, (size_t)*bufsize, c_buf)) < 0)
         HGOTO_DONE(FAIL);

     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
      HD5packFstring(c_buf, _fcdtocp(buf), (size_t)*bufsize);

done:
      if(c_buf) HDfree(c_buf);
      return ret_value;
}
