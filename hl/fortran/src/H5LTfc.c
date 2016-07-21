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

/* This files contains C stubs for H5D Fortran APIs */

#include "H5LTprivate.h"
#include "H5LTf90proto.h"
#include "H5Eprivate.h"

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
h5ltmake_dataset_c (hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     int_f *rank,
                     hsize_t_f *dims,
                     hid_t_f *type_id,
                     void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    char    *c_name = NULL;
    hsize_t *c_dims = NULL;
    int     i;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_dims =  (hsize_t *)HDmalloc(sizeof(hsize_t) * ( (size_t)*rank ));
    if (c_dims == NULL)
        goto done;
    /*
    * transpose dimension arrays because of C-FORTRAN storage order
    */
    for (i = 0; i < *rank ; i++)
    {
        c_dims[i] =  dims[*rank - i - 1];
    }

    /*
    * call H5LTmake_dataset function.
    */

    ret = H5LTmake_dataset((hid_t)*loc_id, c_name, (int)*rank, c_dims, (hid_t)*type_id, buf );
    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);
    if(c_dims!=NULL)
        HDfree(c_dims);
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
h5ltread_dataset_c (hid_t_f *loc_id,
                     size_t_f *namelen,
                     _fcd name,
                     hid_t_f *type_id,
                     void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    hid_t   c_type_id;
    char    *c_name = NULL;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
    * call H5LTread_dataset function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_type_id = (hid_t)*type_id;

    ret = H5LTread_dataset(c_loc_id, c_name, c_type_id, buf );

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);

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
h5ltmake_dataset_string_c (hid_t_f *loc_id,
                            size_t_f *namelen,
                            _fcd name,
                            size_t_f *buflen,
                            char *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_buf = NULL;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_buf = (char *)HD5f2cstring(buf, (size_t)*buflen);
    if (c_buf == NULL)
        goto done;

    /*
    * call H5LTmake_dataset_string function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTmake_dataset_string(c_loc_id,c_name,c_buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);
    if(c_buf!=NULL)
        HDfree(c_buf);

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
h5ltread_dataset_string_c (hid_t_f *loc_id,
                            size_t_f *namelen,
                            _fcd name,
                            char *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
    * call H5LTread_dataset_string function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTread_dataset_string(c_loc_id,c_name,buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);

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
h5ltset_attribute_c(hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd dsetname,
                         size_t_f *attrnamelen,
                         _fcd attrname,
                         size_t_f *size,
		         void *buf, char *dtype, size_t_f *sizeof_val)
{
    int     ret_value = -1;
    herr_t  ret = SUCCEED;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    char    *c_buf = NULL;
    size_t  c_size;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(dsetname, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_attrname = (char *)HD5f2cstring(attrname, (size_t)*attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTset_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;
    c_size   = (size_t)*size;

    if( HDstrncmp(dtype,"I",1) == 0 ) {
      if ((size_t)*sizeof_val == sizeof(int))
        ret = H5LT_set_attribute_numerical(c_loc_id,c_name,c_attrname, c_size, H5T_NATIVE_INT, (const int *)buf);
      else if ((size_t)*sizeof_val == sizeof(long))
	ret = H5LT_set_attribute_numerical(c_loc_id,c_name,c_attrname, c_size, H5T_NATIVE_LONG, (const long *)buf);
#if H5_SIZEOF_LONG != H5_SIZEOF_LONG_LONG
      else if ((size_t)*sizeof_val == sizeof(long long))
        ret = H5LT_set_attribute_numerical(c_loc_id,c_name,c_attrname, c_size, H5T_NATIVE_LLONG, (const long long *)buf);
#endif /* H5_SIZEOF_LONG != H5_SIZEOF_LONG_LONG */
      else
        goto done;
    } else if ( HDstrncmp(dtype,"R",1) == 0 ) {
      if((size_t)*sizeof_val == sizeof(float))
	ret = H5LT_set_attribute_numerical(c_loc_id,c_name,c_attrname, c_size, H5T_NATIVE_FLOAT, (const float *)buf);
      else if((size_t)*sizeof_val == sizeof(double))
	ret = H5LT_set_attribute_numerical(c_loc_id,c_name,c_attrname, c_size, H5T_NATIVE_DOUBLE,  (const double *)buf);
#if H5_SIZEOF_LONG_DOUBLE !=0
      else if((size_t)*sizeof_val == sizeof(long double))
	ret = H5LT_set_attribute_numerical(c_loc_id,c_name,c_attrname, c_size, H5T_NATIVE_LDOUBLE, (const long double *)buf);
#endif
      else
        goto done;
    } else if ( HDstrncmp(dtype,"C",1) == 0 ) {

      c_buf = (char *)HD5f2cstring((_fcd)buf, c_size);
      if (c_buf == NULL)
        goto done;

      ret = H5LTset_attribute_string(c_loc_id,c_name,c_attrname,c_buf);
    }
    if (ret < 0)
      goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);
     if(c_attrname!=NULL)
        HDfree(c_attrname);
    if(c_buf!=NULL)
        HDfree(c_buf);

    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: H5LTget_attribute_c
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
h5ltget_attribute_c(hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd dsetname,
                         size_t_f *attrnamelen,
                         _fcd attrname,
		         void *buf, char *dtype, size_t_f *sizeof_val)
{
    int     ret_value = -1;
    herr_t  ret = SUCCEED;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(dsetname, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_attrname = (char *)HD5f2cstring(attrname, (size_t)*attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTget_attribute_int function.
    */
    c_loc_id = (hid_t)*loc_id;

    if( HDstrncmp(dtype,"I",1) == 0) {
      if((size_t)*sizeof_val == sizeof(int))
	ret = H5LTget_attribute(c_loc_id,c_name,c_attrname,H5T_NATIVE_INT,buf);
      else if ((size_t)*sizeof_val == sizeof(long))
	ret = H5LTget_attribute(c_loc_id,c_name,c_attrname,H5T_NATIVE_LONG,buf);
#if H5_SIZEOF_LONG != H5_SIZEOF_LONG_LONG
      else if ((size_t)*sizeof_val == sizeof(long long))
	ret = H5LTget_attribute(c_loc_id,c_name,c_attrname,H5T_NATIVE_LLONG,buf);
#endif /* H5_SIZEOF_LONG != H5_SIZEOF_LONG_LONG */
      else
        goto done;
    } else if ( HDstrncmp(dtype,"R",1) == 0 ) {
      if((size_t)*sizeof_val == sizeof(float))
	ret = H5LTget_attribute(c_loc_id,c_name,c_attrname,H5T_NATIVE_FLOAT,buf);
      else if((size_t)*sizeof_val == sizeof(double))
	ret = H5LTget_attribute(c_loc_id,c_name,c_attrname,H5T_NATIVE_DOUBLE,buf);
#if H5_SIZEOF_LONG_DOUBLE !=0
      else if((size_t)*sizeof_val == sizeof(long double))
	ret = H5LTget_attribute(c_loc_id,c_name,c_attrname,H5T_NATIVE_LDOUBLE,buf);
#endif
      else
        goto done;
    }
 
    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);
    if(c_attrname!=NULL)
        HDfree(c_attrname);

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
h5ltget_attribute_string_c(hid_t_f *loc_id,
                            size_t_f *namelen,
                            _fcd dsetname,
                            size_t_f *attrnamelen,
                            _fcd attrname,
                            _fcd buf, size_t_f *buf_size)
{
    int     ret_value = -1;
    herr_t  ret;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    char    *c_buf = NULL;

    /*
    * Convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(dsetname, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_attrname = (char *)HD5f2cstring(attrname, (size_t)*attrnamelen);
    if (c_attrname == NULL)
        goto done;
    /*
     * Allocate buffer to hold C attribute string
     */
    if ((c_buf = (char *)HDmalloc((size_t)*buf_size + 1)) == NULL)
      goto done;

    /*
     * Call H5LTget_attribute_int function.
     */
    ret = H5LTget_attribute_string((hid_t)*loc_id,c_name,c_attrname,c_buf);
    if (ret < 0)
        goto done;

    /*
     * Convert C name to FORTRAN and place it in the given buffer
     */
    HD5packFstring(c_buf, _fcdtocp(buf), (size_t)*buf_size); 

    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);
    if(c_attrname!=NULL)
        HDfree(c_attrname);
    if(c_buf!=NULL)
        HDfree(c_buf);

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
h5ltget_dataset_ndims_c(hid_t_f *loc_id,
                         size_t_f *namelen,
                         _fcd name,
                         int_f *rank)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    int     c_rank;

    /*
    * Convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
    * Call H5LTget_dataset_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_dataset_ndims(c_loc_id, c_name, &c_rank);

    if (ret < 0)
        goto done;

    *rank = (int_f)c_rank;
    ret_value = 0;


done:
    if(c_name!=NULL)
        HDfree(c_name);

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
h5ltfind_dataset_c(hid_t_f *loc_id,
                    size_t_f *namelen,
                    _fcd name)
{
    hid_t   c_loc_id;
    char    *c_name = NULL;
    herr_t  ret;

    /*
    * Convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL) return -1;

    /*
    * Call H5LTget_dataset_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTfind_dataset(c_loc_id, c_name);

    if(c_name!=NULL)
       HDfree(c_name);

    return ret;

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
h5ltget_dataset_info_c(hid_t_f *loc_id,
                        size_t_f *namelen,
                        _fcd name,
                        hsize_t_f *dims,
                        int_f *type_class,
                        size_t_f *type_size)
{
    int          ret_value = -1;
    herr_t       ret;
    hid_t        c_loc_id;
    char         *c_name = NULL;
    H5T_class_t  c_classtype;
    size_t       c_type_size;
    hsize_t      c_dims[32];
    int          i;
    int          c_rank;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
    * call H5LTget_dataset_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_dataset_info(c_loc_id, c_name, c_dims, &c_classtype, &c_type_size);
    if (ret < 0)
        goto done;

    *type_class = c_classtype;
    *type_size = (size_t_f)c_type_size;

    /*
    * transpose dimension arrays because of C-FORTRAN storage order
    */

    ret = H5LTget_dataset_ndims(c_loc_id, c_name, &c_rank);
    if (ret < 0)
        goto done;

    for (i = 0; i < c_rank ; i++)
    {
        dims[i] = (hsize_t_f) c_dims[c_rank - i - 1];
    }


    ret_value = 0;

done:
    if(c_name!=NULL)
        HDfree(c_name);

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
h5ltget_attribute_ndims_c(hid_t_f *loc_id,
                           size_t_f *namelen,
                           _fcd dsetname,
                           size_t_f *attrnamelen,
                           _fcd attrname,
                           int_f *rank)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char    *c_name = NULL;
    char    *c_attrname = NULL;
    int     c_rank;

    /*
    * Convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(dsetname, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_attrname = (char *)HD5f2cstring(attrname, (size_t)*attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * Call H5LTset_attribute_ndims function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_ndims(c_loc_id,c_name,c_attrname,&c_rank);

    if (ret < 0)
        goto done;

    *rank = (int_f)c_rank;
    ret_value = 0;


done:
    if(c_name!=NULL)
        HDfree(c_name);
    if(c_attrname!=NULL)
        HDfree(c_attrname);

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
h5ltget_attribute_info_c(hid_t_f *loc_id,
                          size_t_f *namelen,
                          _fcd name,
                          size_t_f *attrnamelen,
                          _fcd attrname,
                          hsize_t_f *dims,
                          int_f *type_class,
                          size_t_f *type_size)
{
    int          ret_value = -1;
    herr_t       ret;
    hid_t        c_loc_id;
    char         *c_name = NULL;
    char         *c_attrname = NULL;
    H5T_class_t  c_classtype;
    size_t       c_type_size;
    hsize_t      c_dims[32];
    int          i;
    int          c_rank;

    /*
    * convert FORTRAN name to C name
    */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_attrname = (char *)HD5f2cstring(attrname, (size_t)*attrnamelen);
    if (c_attrname == NULL)
        goto done;

    /*
    * call H5LTget_attribute_info function.
    */
    c_loc_id = (hid_t)*loc_id;

    ret = H5LTget_attribute_info(c_loc_id,c_name,c_attrname,c_dims,&c_classtype,&c_type_size);
    if (ret < 0)
        goto done;

    *type_class = c_classtype;
    *type_size = (size_t_f)c_type_size;

    /*
    * transpose dimension arrays because of C-FORTRAN storage order
    */

    ret = H5LTget_attribute_ndims(c_loc_id,c_name,c_attrname,&c_rank);
    if (ret < 0)
        goto done;

    for (i = 0; i < c_rank ; i++)
    {
        dims[i] = (hsize_t_f) c_dims[c_rank - i - 1];
    }

    ret_value = 0;


done:
    if(c_name!=NULL)
        HDfree(c_name);
    if(c_attrname!=NULL)
        HDfree(c_attrname);


    return ret_value;
}

/*-------------------------------------------------------------------------
* Function: h5ltpath_valid_c
*
* Purpose: Calls h5ltpath_valid
*
* Return: Success: 0, Failure: -1
*
* Programmer: M. Scot Breitenfeld
*
* Date: February 18, 2012
*
* Comments:
*
* Modifications:
*
*
*-------------------------------------------------------------------------
*/

int_f
h5ltpath_valid_c(hid_t_f *loc_id, 
                  _fcd path, 
                  size_t_f *pathlen, 
                  int_f *check_object_valid_c)
{
    htri_t ret = -1;
    char *c_path = NULL;
    hbool_t check_object_valid;

    /*
     * convert FORTRAN name to C name
     */
    if( NULL == (c_path = (char *)HD5f2cstring(path, (size_t)*pathlen)))
      goto done;
    
    check_object_valid = FALSE;
    if(*check_object_valid_c == 1)
      check_object_valid = TRUE;

    /*
     * call H5LTpath_valid function.
     */
    ret = H5LTpath_valid( (hid_t)*loc_id, c_path, check_object_valid );

done:
    if(c_path != NULL)
      HDfree(c_path);

    return (int_f)ret;
}
