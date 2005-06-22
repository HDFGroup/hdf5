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

/* This files contains C stubs for H5D Fortran APIs */

#include "H5LT.h"
#include "H5LTf90proto.h"



/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_c
 *
 * Purpose: Call H5LTmake_dataset 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltmake_dataset_c (hid_t_f *loc_id, 
                     int_f *namelen,
                     _fcd name, 
                     int_f *rank, 
                     hsize_t_f *dims,
                     hid_t_f *type_id, 
                     void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 hid_t   c_type_id;
 char    *c_name;
 int     c_namelen;
 hsize_t *c_dims;
 int     i;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_dims =  malloc(sizeof(hsize_t) * (*rank ));
 if (!c_dims) return ret_value;

/*
 * Transpose dimension arrays because of C-FORTRAN storage order
 */
 for (i = 0; i < *rank ; i++) {
  c_dims[i] =  dims[*rank - i - 1];
 }

/*
 * Call H5LTmake_dataset function.
 */
 c_loc_id = (hid_t)*loc_id;
 c_type_id = (hid_t)*type_id;

 ret = H5LTmake_dataset(c_loc_id, c_name, *rank, c_dims, c_type_id, buf );

 free (c_dims);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}      


/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_c
 *
 * Purpose: Call H5LTmake_dataset 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltread_dataset_c (hid_t_f *loc_id, 
                     int_f *namelen,
                     _fcd name, 
                     hid_t_f *type_id, 
                     void *buf,
                     hsize_t_f *dims)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 hid_t   c_type_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

/*
 * Call H5LTread_dataset function.
 */
 c_loc_id = (hid_t)*loc_id;
 c_type_id = (hid_t)*type_id;

 ret = H5LTread_dataset(c_loc_id, c_name, c_type_id, buf );

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}   


/*-------------------------------------------------------------------------
 * Function: H5LTmake_dataset_string_c
 *
 * Purpose: Call H5LTmake_dataset 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltmake_dataset_string_c (hid_t_f *loc_id, 
                            int_f *namelen,
                            _fcd name, 
                            char *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

/*
 * Call H5LTmake_dataset_string function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTmake_dataset_string(c_loc_id,c_name,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}  


/*-------------------------------------------------------------------------
 * Function: H5LTread_dataset_string_c
 *
 * Purpose: Call H5LTread_dataset_string 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltread_dataset_string_c (hid_t_f *loc_id, 
                            int_f *namelen,
                            _fcd name, 
                            char *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

/*
 * Call H5LTread_dataset_string function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTread_dataset_string(c_loc_id,c_name,buf);

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}   
    


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_int_c
 *
 * Purpose: Call H5LTset_attribute_int 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltset_attribute_int_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          size_t_f *size,
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 char    *c_attrname;
 int     c_namelen;
 int     c_attrnamelen;
 size_t  c_size;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTset_attribute_int function.
 */
 c_loc_id = (hid_t)*loc_id;
 c_size   = (size_t)*size;

 if (sizeof(int_f) == sizeof(int)) 
     ret = H5LTset_attribute_int(c_loc_id,c_name,c_attrname,buf,c_size);
 else if (sizeof(int_f) == sizeof(long))
    ret = H5LTset_attribute_long(c_loc_id,c_name,c_attrname,buf,c_size);
 else if (sizeof(int_f) == sizeof(long long))
    ret = H5LTset_attribute_long_long(c_loc_id,c_name,c_attrname,buf,c_size);
 else
    return ret_value;

 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}  

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_float_c
 *
 * Purpose: Call H5LTset_attribute_float
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltset_attribute_float_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          size_t_f *size,
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 char    *c_attrname;
 int     c_namelen;
 int     c_attrnamelen;
 size_t  c_size;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = *attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTset_attribute_float function.
 */
 c_loc_id = (hid_t)*loc_id;
 c_size   = (size_t)*size;

 ret = H5LTset_attribute_float(c_loc_id,c_name,c_attrname,buf,c_size);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}  


/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_double_c
 *
 * Purpose: Call H5LTset_attribute_double
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltset_attribute_double_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          size_t_f *size,
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 char    *c_attrname;
 int     c_namelen;
 int     c_attrnamelen;
 size_t  c_size;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = *attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTset_attribute_double function.
 */
 c_loc_id = (hid_t)*loc_id;
 c_size   = (size_t)*size;

 ret = H5LTset_attribute_double(c_loc_id,c_name,c_attrname,buf,c_size);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}  

/*-------------------------------------------------------------------------
 * Function: H5LTset_attribute_string_c
 *
 * Purpose: Call H5LTset_attribute_string
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltset_attribute_string_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 char    *c_attrname;
 int     c_namelen;
 int     c_attrnamelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = *namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = *attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTset_attribute_string function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTset_attribute_string(c_loc_id,c_name,c_attrname,buf);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}  

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_int_c
 *
 * Purpose: Call H5LTget_attribute_int 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_attribute_int_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_attrname;
 int     c_attrnamelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTget_attribute_int function.
 */
 c_loc_id = (hid_t)*loc_id;
 
 if(sizeof(int_f) == sizeof(int))
    ret = H5LTget_attribute_int(c_loc_id,c_name,c_attrname,buf);
 else if (sizeof(int_f) == sizeof(long))
    ret = H5LTget_attribute_long(c_loc_id,c_name,c_attrname,buf);
 else if (sizeof(int_f) == sizeof(long long))
    ret = H5LTget_attribute_long_long(c_loc_id,c_name,c_attrname,buf);
 else
    return ret_value;
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}                


/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_float_c
 *
 * Purpose: Call H5LTget_attribute_float 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_attribute_float_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_attrname;
 int     c_attrnamelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTget_attribute_int function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_attribute_float(c_loc_id,c_name,c_attrname,buf);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}                

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_double_c
 *
 * Purpose: Call H5LTget_attribute_double 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_attribute_double_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_attrname;
 int     c_attrnamelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTget_attribute_int function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_attribute_double(c_loc_id,c_name,c_attrname,buf);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}                

/*-------------------------------------------------------------------------
 * Function: H5LTget_attribute_string_c
 *
 * Purpose: Call H5LTget_attribute_string 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_attribute_string_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          void *buf)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 char    *c_attrname;
 int     c_attrnamelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTget_attribute_int function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_attribute_string(c_loc_id,c_name,c_attrname,buf);
 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}                


/*-------------------------------------------------------------------------
 * Function: H5LTget_dataset_ndims_c
 *
 * Purpose: Call H5LTget_dataset_ndims 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_dataset_ndims_c(hid_t_f *loc_id, 
                         int_f *namelen,
                         _fcd name, 
                         int_f *rank)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;
 int     c_rank;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

/*
 * Call H5LTget_dataset_ndims function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_dataset_ndims(c_loc_id, c_name, &c_rank);
 
 if (ret < 0) return ret_value;
 *rank = (int_f)c_rank;
 ret_value = 0;
 return ret_value;
}         


/*-------------------------------------------------------------------------
 * Function: h5ltfind_dataset_c
 *
 * Purpose: Call H5LTfind_dataset
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltfind_dataset_c(hid_t_f *loc_id, 
                    int_f *namelen,
                    _fcd name)
{
 hid_t   c_loc_id;
 char    *c_name;
 int     c_namelen;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return -1;

/*
 * Call H5LTget_dataset_ndims function.
 */
 c_loc_id = (hid_t)*loc_id;

 return( H5LTfind_dataset(c_loc_id, c_name));
 
}         

/*-------------------------------------------------------------------------
 * Function: h5ltget_dataset_info_c
 *
 * Purpose: Call H5LTget_dataset_info 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_dataset_info_c(hid_t_f *loc_id, 
                        int_f *namelen,
                        _fcd name,
                        hsize_t_f *dims,
                        int_f *type_class,
                        size_t_f *type_size)
{
 int          ret_value = -1;
 herr_t       ret;
 hid_t        c_loc_id;
 char         *c_name;
 int          c_namelen;
 H5T_class_t  c_classtype;
 size_t       c_type_size;
 hsize_t      c_dims[32];
 int          i;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

/*
 * Call H5LTget_dataset_ndims function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_dataset_info(c_loc_id, c_name, c_dims, &c_classtype, &c_type_size);

 *type_class = c_classtype;
 *type_size = (size_t_f)c_type_size;
 for (i = 0; i < 32 ; i++) {
  dims[i] = (hsize_t_f) c_dims[i];
 }

 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}         

/*-------------------------------------------------------------------------
 * Function: h5ltget_attribute_ndims_c
 *
 * Purpose: Call H5LTget_attribute_ndims 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_attribute_ndims_c(hid_t_f *loc_id, 
                          int_f *namelen,
                          _fcd dsetname, 
                          int_f *attrnamelen,
                          _fcd attrname, 
                          int_f *rank)
{
 int     ret_value = -1;
 herr_t  ret;
 hid_t   c_loc_id;
 char    *c_name;
 char    *c_attrname;
 int     c_namelen;
 int     c_attrnamelen;
 int     c_rank;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen =(int) *namelen;
 c_name = (char *)HD5f2cstring(dsetname, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTset_attribute_ndims function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_attribute_ndims(c_loc_id,c_name,c_attrname,&c_rank);
 
 if (ret < 0) return ret_value;
 *rank = (int_f)c_rank;
 ret_value = 0;
 return ret_value;
}  


/*-------------------------------------------------------------------------
 * Function: h5ltget_attribute_info_c
 *
 * Purpose: Call H5LTget_attribute_info 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: September 09, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
nh5ltget_attribute_info_c(hid_t_f *loc_id, 
                        int_f *namelen,
                        _fcd name,
                        int_f *attrnamelen,
                        _fcd attrname, 
                        hsize_t_f *dims,
                        int_f *type_class,
                        size_t_f *type_size)
{
 int          ret_value = -1;
 herr_t       ret;
 hid_t        c_loc_id;
 char         *c_name;
 char         *c_attrname;
 int          c_namelen;
 int          c_attrnamelen;
 H5T_class_t  c_classtype;
 size_t       c_type_size;
 hsize_t      c_dims[32];
 int          i;

/*
 * Convert FORTRAN name to C name
 */
 c_namelen = (int)*namelen;
 c_name = (char *)HD5f2cstring(name, c_namelen); 
 if (c_name == NULL) return ret_value;

 c_attrnamelen = (int)*attrnamelen;
 c_attrname = (char *)HD5f2cstring(attrname, c_attrnamelen); 
 if (c_attrname == NULL) return ret_value;

/*
 * Call H5LTget_dataset_ndims function.
 */
 c_loc_id = (hid_t)*loc_id;

 ret = H5LTget_attribute_info(c_loc_id,c_name,c_attrname,c_dims,&c_classtype,&c_type_size);

 *type_class = c_classtype;
 *type_size = (size_t_f)c_type_size;
 for (i = 0; i < 32 ; i++) {
  dims[i] = (hsize_t_f) c_dims[i];
 }

 
 if (ret < 0) return ret_value;
 ret_value = 0;
 return ret_value;
}         
