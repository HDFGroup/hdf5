#include "H5f90.h"

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
nh5acreate_c (hid_t_f *obj_id, _fcd name, int_f *namelen, hid_t_f *type_id, hid_t_f *space_id, hid_t_f *crt_prp,  hid_t_f *attr_id)
{
     int ret_value = -1;
     char *c_name;
     int c_namelen;
     hid_t c_obj_id;
     hid_t c_type_id;
     hid_t c_space_id;
     hid_t c_attr_id;
     hid_t c_crt_prp;
     /*
      * Define creation property
      */
     c_crt_prp = (hid_t)*crt_prp;
/*
     if ( H5P_DEFAULT_F == c_crt_prp ) c_crt_prp = H5P_DEFAULT;
*/

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Acreate function.
      */
     c_obj_id = *obj_id;
     c_type_id = *type_id;
     c_space_id = *space_id; 
     c_attr_id = H5Acreate(c_obj_id, c_name, c_type_id, c_space_id, c_crt_prp);


     if (c_attr_id < 0) return ret_value;
     *attr_id = (hid_t_f)c_attr_id;
     HDfree(c_name);
     ret_value = 0;
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
nh5aopen_name_c (hid_t_f *obj_id, _fcd name, int_f *namelen, hid_t_f *attr_id)
{
     int ret_value = -1;
     char *c_name;
     int c_namelen;
     hid_t c_obj_id;
     hid_t c_attr_id;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;
     /*
      * Call H5Aopen function.
      */
     c_obj_id = *obj_id;
     c_attr_id = H5Aopen_name(c_obj_id, c_name);

     if (c_attr_id < 0) return ret_value;
     *attr_id = (hid_t_f)c_attr_id;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5awritec_c
 * Purpose:     Call h5awrite_c to write a character  attribute
 * Inputs:      attr_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 *              buf      - character data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday , August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5awritec_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf)
{
     int ret_value = -1;
     
     /*
      * Call h5awrite_c  function.
      */
     ret_value = nh5awrite_c(attr_id, mem_type_id, _fcdtocp(buf));

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5awrite_c
 * Purpose:     Call H5Awrite to write a attribute 
 * Inputs:      attr_id - attribute identifier 
 *              mem_type_id - memory datatype identifier
 *              buf      - data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5awrite_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_attr_id;
     hid_t c_mem_type_id;

     /*
      * Call H5Awrite function.
      */
     c_attr_id = *attr_id;
     c_mem_type_id = *mem_type_id;
     ret = H5Awrite(c_attr_id, c_mem_type_id, buf);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      


/*----------------------------------------------------------------------------
 * Name:        h5areadc_c
 * Purpose:     Call h5aread_c to read character  attribute
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 * Outputs:     buf      - character data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5areadc_c (hid_t_f *attr_id, hid_t_f *mem_type_id, _fcd buf)
{
     int ret_value = -1;
     
     /*
      * Call h5aread_c  function.
      */
     ret_value = nh5aread_c(attr_id, mem_type_id, (_fcdtocp(buf)));

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5aread_c
 * Purpose:     Call H5Araed to read an attribute 
 * Inputs:      dset_id - dataset identifier 
 *              mem_type_id - memory datatype identifier
 * Outputs:     buf      - data buffer
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, August 12, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5aread_c (hid_t_f *attr_id, hid_t_f *mem_type_id, void *buf)
{
     int ret_value = -1;
     herr_t ret;
     hid_t c_attr_id;
     hid_t c_mem_type_id;

     /*
      * Call H5Aread function.
      */
     c_attr_id = *attr_id;
     c_mem_type_id = *mem_type_id;
     ret = H5Aread(c_attr_id, c_mem_type_id, buf);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
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
  int ret_value = 0;
  hid_t c_attr_id;
  c_attr_id = *attr_id;
  if ( H5Aclose(c_attr_id) < 0  ) ret_value = -1;
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
nh5adelete_c (hid_t_f *obj_id, _fcd name, int_f *namelen)
{
     int ret_value = -1;
     herr_t status;
     hid_t c_obj_id;
     char *c_name;
     int c_namelen;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Adelete function.
      */
     c_obj_id = *obj_id;
     status = H5Adelete(c_obj_id, c_name);

     if (status < 0) return ret_value;
     HDfree(c_name);
     ret_value = 0;
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
     int ret_value = -1;
     hid_t c_obj_id;
     hid_t c_attr_id;
     unsigned c_idx;
     c_idx = (unsigned)*idx;

     /*
      * Call H5Aopen_idx function.
      */
     c_obj_id = *obj_id;
     c_attr_id = H5Aopen_idx(c_obj_id, c_idx);

     if (c_attr_id < 0) return ret_value;
     *attr_id = (hid_t_f)c_attr_id;
     ret_value = 0;
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
     int ret_value = -1;
     hid_t c_attr_id;
     hid_t c_space_id;

     /*
      * Call H5Aget_space function.
      */
     c_attr_id = *attr_id;
     c_space_id = H5Aget_space(c_attr_id);

     if (c_space_id < 0) return ret_value;
     *space_id = (hid_t_f)c_space_id;
     ret_value = 0;
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
     int ret_value = -1;
     hid_t c_attr_id;
     hid_t c_type_id;

     /*
      * Call H5Aget_type function.
      */
     c_attr_id = *attr_id;
     c_type_id = H5Aget_type(c_attr_id);

     if (c_type_id < 0) return ret_value;
     *type_id = (hid_t_f)c_type_id;
     ret_value = 0;
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
     int ret_value = -1;
     hid_t c_obj_id;
     int c_attr_num;

     /*
      * Call H5Aget_num_attrs function.
      */
     c_obj_id = *obj_id;
     c_attr_num = H5Aget_num_attrs(c_obj_id);

     if (c_attr_num < 0) return ret_value;
     *attr_num = c_attr_num;
     ret_value = 0;
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
     int ret_value = -1;
     hid_t c_attr_id;
     ssize_t c_size;
     size_t c_bufsize;
     char *c_buf =NULL;

     /*
      * Allocate buffer to hold name of an attribute
      */
     c_bufsize = *bufsize;
     c_buf = (char *)HDmalloc((int)c_bufsize +1);
     if (c_buf == NULL) return ret_value; 
 
     /*
      * Call H5Aget_name function
      */
     c_attr_id = *attr_id;
     c_size = H5Aget_name(c_attr_id, c_bufsize, c_buf);
     if (c_size < 0) return ret_value;

     /*
      * Convert C name to FORTRAN and place it in the given buffer
      */
      HDpackFstring(c_buf, _fcdtocp(buf), (int)c_bufsize);
      HDfree(c_buf);
      ret_value = (int_f)c_size;
      return ret_value;
}
