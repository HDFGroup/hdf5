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

/* This file contains C stubs for H5P Fortran APIs */


#include "H5f90.h"


/*----------------------------------------------------------------------------
 * Name:        h5pcreate_c
 * Purpose:     Call H5Pcreate to create a property list 
 * Inputs:      classtype - type of the property list 
 * Outputs:     prp_id - identifier of the created property list 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pcreate_c ( int_f *classtype, hid_t_f *prp_id )
{
  H5P_class_t c_classtype;
  int CASE;
  int ret_value = 0;
  hid_t c_prp_id;
  /*CASE = (int)*classtype; */
  c_classtype = (H5P_class_t)*classtype;
/*

  switch (CASE) {

     case (H5P_FILE_CREATE_F):
       c_classtype = H5P_FILE_CREATE;
       break;

     case(H5P_FILE_ACCESS_F): 
       c_classtype = H5P_FILE_ACCESS;
       break;

     case(H5P_DATASET_CREATE_F):
       c_classtype = H5P_DATASET_CREATE;
       break;

     case(H5P_DATASET_XFER_F):
       c_classtype = H5P_DATASET_XFER;
       break;

     case(H5P_MOUNT_F):
       c_classtype = H5P_MOUNT;
       break;

     default:
       ret_value = -1;
       return ret_value;
 }
*/
   c_prp_id = H5Pcreate(c_classtype); 

  if ( c_prp_id  < 0  ) ret_value = -1;
  *prp_id = (hid_t_f)c_prp_id;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pclose_c
 * Purpose:     Call H5Pclose to close property lis 
 * Inputs:      prp_id - identifier of the property list to be closed
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pclose_c ( hid_t_f *prp_id )
{
  int ret_value = 0;
  hid_t c_prp_id;

  c_prp_id = *prp_id;
  if ( H5Pclose(c_prp_id) < 0  ) ret_value = -1;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pcopy_c
 * Purpose:     Call H5Pcopy to copy property list 
 * Inputs:      prp_id - identifier of the property list to be copied 
 * Outputs:     new_prp_id - identifier of the new property list 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pcopy_c ( hid_t_f *prp_id , hid_t_f *new_prp_id)
{
  int ret_value = 0;
  hid_t c_prp_id;
  hid_t c_new_prp_id;

  c_prp_id = *prp_id;
  c_new_prp_id = H5Tcopy(c_prp_id); 
  if ( c_new_prp_id < 0  ) ret_value = -1;
  *new_prp_id = (hid_t_f)c_new_prp_id;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pget_class_c
 * Purpose:     Call H5Pget_class to determine property list class
 * Inputs:      prp_id - identifier of the dataspace 
 * Outputs:     classtype - class type; possible values are:
 *              H5P_NO_CLASS_F       -1 
 *              H5P_FILE_CREATE_F     0
 *              H5P_FILE_ACCESS_F     1
 *              H5P_DATASET_CREATE_F  2
 *              H5P_DATASET_XFER_F    3
 *              H5P_MOUNT_F           4
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pget_class_c ( hid_t_f *prp_id , int_f *classtype)
{
  int ret_value = 0;
  hid_t c_prp_id;
  H5P_class_t c_classtype; 

  c_prp_id = *prp_id;
  c_classtype = H5Pget_class(c_prp_id);
  if (c_classtype == H5P_NO_CLASS ) {
 /*     *classtype = H5P_NO_CLASS_F; */
      *classtype = H5P_NO_CLASS; 
       ret_value = -1;
       return ret_value;
  }
  *classtype = (int_f)c_classtype;
/*
  if (c_classtype == H5P_FILE_CREATE)    *classtype = H5P_FILE_CREATE_F;
  if (c_classtype == H5P_FILE_ACCESS)    *classtype = H5P_FILE_ACCESS_F; 
  if (c_classtype == H5P_DATASET_CREATE) *classtype = H5P_DATASET_CREATE_F; 
  if (c_classtype == H5P_DATASET_XFER)   *classtype = H5P_DATASET_XFER_F; 
  if (c_classtype == H5P_MOUNT_F)        *classtype = H5P_MOUNT_F; 
*/
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_preserve_c
 * Purpose:     Call H5Pset_preserve to set  transfer property for compound 
 *              datatype
 * Inputs:      prp_id - property list identifier 
 *              flag - TRUE/FALSE flag 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, February 17, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pset_preserve_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = 0;
  hid_t c_prp_id;
  herr_t status;
  hbool_t c_flag = 0;

  if (*flag > 0) c_flag = 1;
  c_prp_id = *prp_id;
  status = H5Pset_preserve(c_prp_id, c_flag);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5pget_preserve_c
 * Purpose:     Call H5Pget_preserve to set  transfer property for compound 
 *              datatype
 * Inputs:      prp_id - property list identifier 
 * Outputs:     flag - TRUE/FALSE flag 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Thursday, February 17, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pget_preserve_c ( hid_t_f *prp_id , int_f *flag)
{
  int ret_value = 0;
  hid_t c_prp_id;
  herr_t status;
  int c_flag; 

  c_prp_id = *prp_id;
  c_flag = H5Pget_preserve(c_prp_id);
  if ( c_flag < 0  ) ret_value = -1;
  *flag = (int_f)c_flag;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_deflate_c
 * Purpose:     Call H5Pset_deflate to set deflate level 
 * Inputs:      prp_id - property list identifier 
 *              level - level of deflation 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pset_deflate_c ( hid_t_f *prp_id , int_f *level)
{
  int ret_value = 0;
  hid_t c_prp_id;
  int c_level;
  herr_t status;

  c_prp_id = *prp_id;
  c_level = *level;
  status = H5Pset_deflate(c_prp_id, c_level);
  if ( status < 0  ) ret_value = -1;
  return ret_value;
}



/*----------------------------------------------------------------------------
 * Name:        h5pset_chunk_c
 * Purpose:     Call H5Pset_chunk to set the sizes of chunks for a chunked
 *              layout dataset 
 * Inputs:      prp_id - property list identifier 
 *              rank - number of dimensions of each chunk 
 *              dims - array of the size of each chunk 
 * Returns:     0 on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pset_chunk_c ( hid_t_f *prp_id, int_f *rank, hsize_t_f *dims )
{
  int ret_value = -1;
  hid_t c_prp_id;
  int c_rank;
  hsize_t *c_dims;
  herr_t status;
  int i;

  c_dims =  malloc(sizeof(hsize_t) * (*rank ));
  if (!c_dims) return ret_value;

  /*
   * Transpose dimension arrays because of C-FORTRAN storage order
   */
  for (i = 0; i < *rank ; i++) {
       c_dims[i] =  dims[*rank - i - 1];
  }

  c_prp_id = *prp_id;
  c_rank = *rank;
  status = H5Pset_chunk(c_prp_id, c_rank, c_dims);
  if (status < 0) return ret_value;
  ret_value = 0;
  HDfree (c_dims);
  return ret_value;
}  


/*----------------------------------------------------------------------------
 * Name:        h5pget_chunk_c
 * Purpose:     Call H5Pget_chunk to get the sizes of chunks for a chunked
 *              layout dataset  for at list max_rank number of dimensions
 * Inputs:      prp_id - property list identifier 
 *              max rank - maximum number of dimensions to return  
 *              dims - array of the size of each chunk 
 * Returns:     number of chunk's dimnesion on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f 
nh5pget_chunk_c ( hid_t_f *prp_id, int_f *max_rank, hsize_t_f *dims )
{
  int ret_value = -1;
  hid_t c_prp_id;
  hsize_t *c_dims;
  int rank;
  int c_max_rank;
  int i;

  c_dims =  malloc(sizeof(hsize_t) * (*max_rank ));
  if (!c_dims) return ret_value;

  c_prp_id = *prp_id;
  c_max_rank = *max_rank;
  rank = H5Pget_chunk(c_prp_id, c_max_rank, c_dims);

  /*
   * Transpose dimension arrays because of C-FORTRAN storage order
   */
  for (i = 0; i < *max_rank ; i++) {
       dims[*max_rank - i - 1] = c_dims[i];
  }
  HDfree (c_dims);
  if (rank < 0) return ret_value;
  ret_value = (int_f)rank;
  return ret_value;
}  



/*----------------------------------------------------------------------------
 * Name:        h5pset_fill_valuec_c
 * Purpose:     Call h5pset_fill_value_c to a character fill value
 * Inputs:      prp_id - property list identifier 
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue  - character value 
 * Returns:     0 on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue)
{
     int ret_value = -1;
     
     /*
      * Call h5pset_fill_value_c  function.
      */
     ret_value = nh5pset_fill_value_c(prp_id, type_id, _fcdtocp(fillvalue));

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fill_value_c
 * Purpose:     Call H5Pset_fill_value to set a fillvalue for a dataset 
 * Inputs:      prp_id - property list identifier 
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue - fillvalue 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     int ret_value = -1;
     hid_t c_prp_id;
     hid_t c_type_id;
     herr_t ret;

     /*
      * Call H5Pset_fill_value function.
      */
     c_prp_id = *prp_id;
     c_type_id = *type_id;
     ret = H5Pset_fill_value(c_prp_id, c_type_id, fillvalue);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      


/*----------------------------------------------------------------------------
 * Name:        h5pget_fill_valuec_c
 * Purpose:     Call h5pget_fill_value_c to a character fill value
 * Inputs:      prp_id - property list identifier 
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue  - character value 
 * Returns:     0 on success, -1 on failure
 *              Saturday, August 14, 1999
 * Programmer:  Elena Pourmal
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fill_valuec_c (hid_t_f *prp_id, hid_t_f *type_id, _fcd fillvalue)
{
     int ret_value = -1;
     
     /*
      * Call h5pget_fill_value_c  function.
      */
     ret_value = nh5pset_fill_value_c(prp_id, type_id, _fcdtocp(fillvalue));

     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fill_value_c
 * Purpose:     Call H5Pget_fill_value to set a fillvalue for a dataset 
 * Inputs:      prp_id - property list identifier 
 *              type_id - datatype identifier (fill value is of type type_id)
 *              fillvalue - fillvalue 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Saturday, August 14, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fill_value_c (hid_t_f *prp_id, hid_t_f *type_id, void *fillvalue)
{
     int ret_value = -1;
     hid_t c_prp_id;
     hid_t c_type_id;
     herr_t ret;

     /*
      * Call H5Pget_fill_value function.
      */
     c_prp_id = *prp_id;
     c_type_id = *type_id;
     ret = H5Pget_fill_value(c_prp_id, c_type_id, fillvalue);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_version_c
 * Purpose:     Call H5Pget_version to get the version information
 *              of various objects for a file creation property list  
 * Inputs:      prp_id - property list identifier 
 * Outputs:     boot - array to put boot block version number
 *              freelist - array to put global freelist version number
 *              stab - array to put symbol table version number
 *              shhdr - array to put shared object header version number
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications: Removed extra length parameters EP 7/6/00
 *---------------------------------------------------------------------------*/
int_f
nh5pget_version_c (hid_t_f *prp_id, int_f * boot,int_f * freelist, int_f * stab, int_f *shhdr)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int i;
     herr_t ret;
     int c_boot;
     int c_freelist;
     int c_stab;
     int c_shhdr;

     /*
      * Call H5Pget_version function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_version(c_prp_id, &c_boot, &c_freelist, &c_stab, &c_shhdr);
         *boot = (int_f)c_boot;
         *freelist = (int_f)c_freelist;
         *stab = (int_f)c_stab;
         *shhdr = (int_f)c_shhdr;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_userblock_c
 * Purpose:     Call H5Pget_userblock to get the size of a user block in 
 *              a file creation property list
 * Inputs:      prp_id - property list identifier 
 * Outputs      size - Size of the user-block in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_userblock_c (hid_t_f *prp_id, hsize_t_f * size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int i;
     herr_t ret;
     hsize_t c_size;

     /*
      * Call H5Pget_userblock function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_userblock(c_prp_id, &c_size);

         *size = (hsize_t_f)c_size;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pset_userblock_c
 * Purpose:     Call H5Pset_userblock to set the size of a user block in 
 *              a file creation property list
 * Inputs:      prp_id - property list identifier 
 *              size - Size of the user-block in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_userblock_c (hid_t_f *prp_id, hsize_t_f * size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_size;
     c_size = (hsize_t)*size;

     /*
      * Call H5Pset_userblock function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_userblock(c_prp_id, c_size);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_sizes_c
 * Purpose:     Call H5Pget_sizes to get the size of the offsets 
 *              and lengths used in an HDF5 file 
 * Inputs:      prp_id - property list identifier 
 * Outputs      sizeof_addr - Size of an object offset in bytes
 *              sizeof_size - Size of an object length in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications: Deleted extra length parameters. EP 6/7/00
 *---------------------------------------------------------------------------*/
int_f
nh5pget_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int i;
     herr_t ret;
     size_t c_sizeof_addr;
     size_t c_sizeof_size;

     /*
      * Call H5Pget_sizes function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_sizes(c_prp_id, &c_sizeof_addr, &c_sizeof_size);

         *sizeof_addr = (size_t_f)c_sizeof_addr;
         *sizeof_size = (size_t_f)c_sizeof_size;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pset_sizes_c
 * Purpose:     Call H5Pset_sizes to set the size of the offsets
 * Inputs:      prp_id - property list identifier 
 *              sizeof_addr - Size of an object offset in bytes
 *              sizeof_size - Size of an object length in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_sizes_c (hid_t_f *prp_id, size_t_f * sizeof_addr, size_t_f * sizeof_size)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     size_t c_addr, c_size;
     c_addr = (size_t)*sizeof_addr;
     c_size = (size_t)*sizeof_size;

     /*
      * Call H5Pset_sizes function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_sizes(c_prp_id, c_addr, c_size);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pset_sym_k_c
 * Purpose:     Call H5Pset_sym_k to set the size of parameters used 
 *              to control the symbol table node 
 * Inputs:      prp_id - property list identifier
 *              ik - Symbol table tree rank
 *              lk - Symbol table node size 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_ik;
     int c_lk;
     herr_t ret;

     /*
      * Call H5Pset_sym_k function.
      */
     c_prp_id = *prp_id;
     c_ik = *ik;
     c_lk = *lk;
     ret = H5Pset_sym_k(c_prp_id, c_ik, c_lk);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_sym_k_c
 * Purpose:     Call H5Pget_sym_k to get the size of parameters used 
 *              to control the symbol table node 
 * Inputs:      prp_id - property list identifier
 * Outputs:     ik - Symbol table tree rank
 *              lk - Symbol table node size 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_sym_k_c (hid_t_f *prp_id, int_f* ik, int_f* lk)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int i;
     herr_t ret;
     int c_ik;
     int c_lk;

     /*
      * Call H5Pget_sym_k function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_sym_k(c_prp_id, &c_ik, &c_lk);
     *ik = (int_f)c_ik;
     *lk = (int_f)c_lk;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_istore_k_c
 * Purpose:     Call H5Pset_istore_k to set the size of the parameter
 *              used to control the B-trees for indexing chunked datasets
 * Inputs:      prp_id - property list identifier
 *              ik - Symbol table tree rank
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_istore_k_c (hid_t_f *prp_id, int_f* ik)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_ik;
     herr_t ret;

     /*
      * Call H5Pset_istore_k function.
      */
     c_prp_id = *prp_id;
     c_ik = *ik;
     ret = H5Pset_istore_k(c_prp_id, c_ik);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_istore_k_c
 * Purpose:     Call H5Pget_istore_k to get the size of parameters used 
 *              to control the B-trees for indexing chunked datasets  
 * Inputs:      prp_id - property list identifier
 * Outputs:     ik - Symbol table tree rank
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_istore_k_c (hid_t_f *prp_id, int_f* ik)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     int c_ik;

     /*
      * Call H5Pget_istore_k function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_istore_k(c_prp_id, &c_ik);
     *ik = (int_f)c_ik;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_driver_c
 * Purpose:     Call H5Pget_driver to get low-level file driver identifier 
 * Inputs:      prp_id - property list identifier
 * Outputs:     driver - low-level file driver identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_driver_c (hid_t_f *prp_id, hid_t_f* driver)
{
     int ret_value = -1;
     /*
     hid_t c_prp_id;
     hid_t c_driver;
     */

     /*
      * Call H5Pget_driver function.
      */
     /*
     c_prp_id = *prp_id;
     c_driver = H5Pget_driver(c_prp_id);
     *driver = (hid_t_f) c_driver;
     if (c_driver < 0) return ret_value;
     */
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_stdio_c
 * Purpose:     Call H5Pset_stdio to set the low level file driver to 
 *              use the functions declared in the stdio.h
 * Inputs:      prp_id - property list identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 7, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_stdio_c (hid_t_f *prp_id)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pset_fapl_stdio function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_fapl_stdio(c_prp_id); 
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 
#ifdef NO_SUCH_F90_FUNCTION
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_stdio_c
 * Purpose:     Call H5Pget_fapl_stdio to determine whther the low level file driver 
 *              uses the functions declared in the stdio.h
 * Inputs:      prp_id - property list identifier
 * Outputs:     io - value indicates whether the file driver uses
 *                   the functions declared in the stdio.h
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_stdio_c (hid_t_f *prp_id, int_f* io)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pget_fapl_stdio function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_stdio(c_prp_id); 
     if (ret < 0) return ret_value;
     *io = (int_f)ret;
     ret_value = 0;
     return ret_value;
}

#endif /*NO_SUCH_F90_FUNCTION*/

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_sec2_c
 * Purpose:     Call H5Pset_fapl_sec2 to set the low level file driver to 
 *              use the functions declared in the  unistd.h
 * Inputs:      prp_id - property list identifier
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_sec2_c (hid_t_f *prp_id)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pset_fapl_sec2 function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_fapl_sec2(c_prp_id); 
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

#ifdef NO_SUCH_F90_FUNCTION
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_sec2_c
 * Purpose:     Call H5Pget_fapl_stdio to determine whther the low level file driver 
 *              uses the functions declared in the  unistd.h
 * Inputs:      prp_id - property list identifier
 * Outputs:     sec2 - value indicates whether the file driver uses
 *                   the functions declared in the  unistd.h
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_sec2_c (hid_t_f *prp_id, int_f* sec2)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     /*
      * Call H5Pget_fapl_sec2 function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_sec2(c_prp_id); 
     if (ret < 0) return ret_value;
     *sec2 = (int_f)ret;
     ret_value = 0;
     return ret_value;
}
#endif /*NO_SUCH_F90_FUNCTION*/

/*----------------------------------------------------------------------------
 * Name:        h5pset_alignment_c
 * Purpose:     Call H5Pset_alignment to set alignment properties of
 *              a file access property list
 * Inputs:      prp_id - property list identifier
 *              threshold - Threshold value
 *              alignment - Alignment value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_alignment_c (hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_threshold, c_alignment;
     c_threshold = (hsize_t)*threshold;
     c_alignment = (hsize_t)* alignment;
     /*
      * Call H5Pset_alignment function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pset_alignment(c_prp_id, c_threshold, c_alignment);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_alignment_c
 * Purpose:     Call H5Pget_alignment to get alignment properties of
 *              a file access property list
 * Inputs:      prp_id - property list identifier
 *              threshold - Threshold value
 *              alignment - Alignment value
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_alignment_c (hid_t_f *prp_id, hsize_t_f* threshold, hsize_t_f* alignment)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_threshold, c_alignment;
     /*
      * Call H5Pget_alignment function.
      */
     c_prp_id = (hid_t)*prp_id;
     ret = H5Pget_alignment(c_prp_id, &c_threshold, &c_alignment);
     if (ret < 0) return ret_value;
     *threshold = (hsize_t_f)c_threshold;
     *alignment = (hsize_t_f)c_alignment;
     
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_core_c
 * Purpose:     Call H5Pset_fapl_core to set the low-level file driver
 *              to use malloc() and free()
 * Inputs:      prp_id - property list identifier
 *              increment - File block size in bytes
 *              flag - Boolean flag indicating whether to write the 
 *              file contents to disk when the file is closed. 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     size_t c_increment;
     hbool_t c_backing_store;
     c_increment = (size_t)*increment;
     c_backing_store = (hbool_t)*flag;
     
     /*
      * Call H5Pset_fapl_core function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_fapl_core(c_prp_id, c_increment, c_backing_store); 
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_core_c
 * Purpose:     Call H5Pget_fapl_core to determine whether the file access 
 *              property list is set to the core drive
 * Inputs:      prp_id - property list identifier
 * Outputs      increment - File block size in bytes
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_core_c (hid_t_f *prp_id, size_t_f* increment, int_f *flag)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     size_t c_increment = 0;
     hbool_t c_backing_store; 
     *flag = 0; 
     /*
      * Call H5Pset_fapl_core function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_core(c_prp_id, &c_increment, &c_backing_store); 
     if (ret < 0) return ret_value;
     *increment = (size_t_f)c_increment;
     if(c_backing_store  > 0) *flag = 1;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_family_c
 * Purpose:     Call H5Pset_fapl_family to set the file access properties list 
 *              to the family driver
 * Inputs:      prp_id - property list identifier
 *              memb_size -  Logical size, in bytes, of each family member.
 *              memb_plist - Identifier of the file access property list
 *                           for each member of the family
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_family_c(hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist )
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     hsize_t c_memb_size;
     hid_t c_memb_plist;
     c_memb_size =(hsize_t) *memb_size;
     c_memb_plist =(hid_t) *memb_plist;
     /*
      * Call H5Pset_fapl_family function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_fapl_family(c_prp_id, c_memb_size, c_memb_plist); 
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_family_c
 * Purpose:     Call H5Pget_fapl_family to determine whether the file access 
 *              property list is set to the family driver
 * Inputs:      prp_id - property list identifier
 *              memb_size -  Logical size, in bytes, of each family member.
 *              memb_plist - Identifier of the file access property list
 *                           for each member of the family
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_family_c(hid_t_f *prp_id, hsize_t_f* memb_size, hid_t_f* memb_plist)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     hsize_t c_memb_size = 0;
     hid_t c_memb_plist = -1;
     /*
      * Call H5Pget_fapl_family function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_family(c_prp_id, &c_memb_size, &c_memb_plist); 
     if (ret < 0) return ret_value;
     *memb_size = (hsize_t_f)c_memb_size;
     *memb_plist = (hid_t_f)c_memb_plist;
    
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_cache_c
 * Purpose:     Call H5Pset_cache to set he number of elements in 
 *              the meta data cache and the total number of bytes in
 *              the raw data chunk cache
 * Inputs:      prp_id - property list identifier
 *              mdc_nelmts - Number of elements (objects) in the
 *                           meta data cache
 *              rdcc_nbytes - Total size of the raw data chunk cache, in bytes
 *              rdcc_w0 - Preemption policy
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed the type of the rdcc_w0 parameter to be real_f EP 7/7/00
 *                instead of double
 *---------------------------------------------------------------------------*/
int_f
nh5pset_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, int_f* rdcc_nelmts,  size_t_f* rdcc_nbytes , real_f* rdcc_w0 )
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     int c_mdc_nelmts;
     int c_rdcc_nelmts;
     size_t c_rdcc_nbytes;
     double c_rdcc_w0;
     c_rdcc_nbytes =(size_t) *rdcc_nbytes;
     c_rdcc_w0 = (double)*rdcc_w0;

     /*
      * Call H5Pset_cache function.
      */
     c_prp_id = *prp_id;
     c_mdc_nelmts = *mdc_nelmts;
     c_rdcc_nelmts = *rdcc_nelmts;
     ret = H5Pset_cache(c_prp_id, c_mdc_nelmts, c_rdcc_nelmts, c_rdcc_nbytes, c_rdcc_w0  );
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_cache_c
 * Purpose:     Call H5Pget_cache to get he number of elements in 
 *              the meta data cache and the total number of bytes in
 *              the raw data chunk cache
 * Inputs:      prp_id - property list identifier
 * Outputs:     mdc_nelmts - Number of elements (objects) in the
 *                           meta data cache
 *              rdcc_nelmts - Number of elements in the raw data chunk 
 *              rdcc_nbytes - Total size of the raw data chunk cache, in bytes
 *              rdcc_w0 - Preemption policy
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed type of the rdcc_w0 parameter to be real_f instead of double
 *---------------------------------------------------------------------------*/
int_f
nh5pget_cache_c(hid_t_f *prp_id, int_f* mdc_nelmts, int_f* rdcc_nelmts, size_t_f* rdcc_nbytes , real_f* rdcc_w0)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     int c_mdc_nelmts, c_rdcc_nelmts; 
     size_t c_rdcc_nbytes;
     hid_t c_memb_plist;
     double c_rdcc_w0; 
     /*
      * Call H5Pget_cache function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_cache(c_prp_id, &c_mdc_nelmts, &c_rdcc_nelmts, &c_rdcc_nbytes, &c_rdcc_w0);
     if (ret < 0) return ret_value;
     *mdc_nelmts = (int_f)c_mdc_nelmts;
     *rdcc_nelmts = (int_f)c_rdcc_nelmts;
     *rdcc_nbytes = (size_t_f)c_rdcc_nbytes;
     *rdcc_w0 = (real_f)c_rdcc_w0;
    
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_split_c
 * Purpose:     Call H5Pset_fapl_split to set he low-level driver to split meta data 
 *              from raw data
 * Inputs:      prp_id - property list identifier
 *              meta_len - Length of meta_ext
 *              meta_ext - Name of the extension for the metafile filename.                 
 *              meta_plist - Identifier of the meta file access property list
 *              raw_len - Length of raw _ext
 *              raw_ext - Name of the extension for the raw file filename.                 
 *              raw_plist - Identifier of the raw  file access property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9, 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_fapl_split_c(hid_t_f *prp_id, int_f* meta_len, _fcd meta_ext, hid_t_f* meta_plist, int_f* raw_len, _fcd raw_ext, hid_t_f * raw_plist)
{
     int ret_value = -1;
     hid_t c_prp_id;
     hid_t c_meta_plist;
     hid_t c_raw_plist; 
     herr_t ret = -1;
     char* c_meta_ext;
     char* c_raw_ext;
     c_meta_ext = (char *)HD5f2cstring(meta_ext, (int)*meta_len); 
     if (c_meta_ext == NULL) return ret_value;
     c_raw_ext = (char *)HD5f2cstring(raw_ext, (int)*raw_len); 
     if (c_raw_ext == NULL) return ret_value;

     /*
      * Call H5Pset_fapl_split function.
      */
     c_prp_id = *prp_id;
     c_meta_plist = *meta_plist;
     c_raw_plist = *raw_plist; 
     ret = H5Pset_fapl_split(c_prp_id, c_meta_ext, c_meta_plist, c_raw_ext, c_raw_plist );
     HDfree(c_meta_ext);
     HDfree(c_raw_ext);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}


#ifdef NO_SUCH_F90_FUNCTION
/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_split_c
 * Purpose:     Call H5Pget_fapl_split to determine whether the file access 
 *              property list is set to the split driver
 * Inputs:      prp_id - property list identifier
 *              meta_ext_size - Number of characters of the meta file extension
 *                              to be copied to the meta_ext buffer
 *              raw_ext_size - Number of characters of the raw file extension
 *                              to be copied to the raw_ext buffer
 *Outputs:      meta_ext - Name of the extension for the metafile filename.                 
 *              meta_plist - Identifier of the meta file access property list
 *              raw_ext - Name of the extension for the raw file filename.                 
 *              raw_plist - Identifier of the raw  file access property list
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              March 9 , 2001
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_fapl_split_c(hid_t_f *prp_id, size_t_f* meta_ext_size , _fcd meta_ext, hid_t_f* meta_plist, size_t_f* raw_ext_size, _fcd raw_ext, hid_t_f * raw_plist)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret = -1;
     size_t c_meta_ext_size, c_raw_ext_size; 
     hid_t c_meta_plist = -1;
     hid_t c_raw_plist = -1; 

     char* c_meta_ext = NULL;
     char* c_raw_ext  = NULL;

     c_meta_ext_size = (size_t) *meta_ext_size;
     c_raw_ext_size = (size_t) *raw_ext_size;
     c_meta_ext = (char*)malloc(sizeof(char)*c_meta_ext_size);
     c_raw_ext = (char*)malloc(sizeof(char)*c_raw_ext_size);
     if(c_meta_ext == NULL || c_raw_ext == NULL) return ret_value;

     /*
      * Call H5Pget_fapl_split function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_split(c_prp_id, c_meta_ext_size, c_meta_ext,&c_meta_plist, c_raw_ext_size, c_raw_ext, &c_raw_plist ); 

     if (ret < 0) return ret_value;
     *meta_plist = c_meta_plist;
     *raw_plist = c_raw_plist;
     HD5packFstring(c_meta_ext, _fcdtocp(meta_ext), strlen(c_meta_ext));  
     HD5packFstring(c_raw_ext, _fcdtocp(raw_ext), strlen(c_raw_ext));  
    
     ret_value = 0;
     return ret_value;
} 
#endif /*NO_SUCH_F90_FUNCTION*/

/*----------------------------------------------------------------------------
 * Name:        h5pset_gc_references_c
 * Purpose:     Call H5Pset_gc_references to set garbage 
 *              collecting references flag
 * Inputs:      prp_id - property list identifier
 *              gc_reference - flag for garbage collecting references 
 *                             for the file
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_gc_references_c (hid_t_f *prp_id, int_f* gc_references)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     unsigned c_gc_references;
     c_gc_references = (unsigned)*gc_references;

     /*
      * Call H5Pset_gc_references function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_gc_references(c_prp_id, c_gc_references);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_gc_references_c
 * Purpose:     Call H5Pget_gc_references to set garbage 
 *              collecting references flag
 * Inputs:      prp_id - property list identifier
 * Outputs      gc_reference - flag for garbage collecting references 
 *                             for the file
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_gc_references_c (hid_t_f *prp_id, int_f* gc_references)
{
     int ret_value = -1;
     hid_t c_prp_id;
     unsigned c_gc_references;
     herr_t ret;
     /*
      * Call H5Pget_gc_references function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_gc_references(c_prp_id, &c_gc_references);
     if (ret < 0) return ret_value;
     *gc_references = (int_f)c_gc_references;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_layout_c
 * Purpose:     Call H5Pset_layout to the type of storage used
 *              store the raw data for a dataset
 * Inputs:      prp_id - property list identifier
 *              layout - Type of storage layout for raw data.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_layout_c (hid_t_f *prp_id, int_f* layout)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     H5D_layout_t c_layout;
     c_layout = (H5D_layout_t)*layout;
     /*
      * Call H5Pset_layout function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_layout(c_prp_id, c_layout);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}      

/*----------------------------------------------------------------------------
 * Name:        h5pget_layout_c
 * Purpose:     Call H5Pget_layout to the type of storage used
 *              store the raw data for a dataset
 * Inputs:      prp_id - property list identifier
 * Outputs:     layout - Type of storage layout for raw data.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_layout_c (hid_t_f *prp_id, int_f* layout)
{
     int ret_value = -1;
     hid_t c_prp_id;
     H5D_layout_t c_layout;
     /*
      * Call H5Pget_layout function.
      */
     c_prp_id = *prp_id;
     c_layout = H5Pget_layout(c_prp_id);
     if (c_layout < 0) return ret_value;
     *layout = (int_f)c_layout;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_filter_c
 * Purpose:     Call H5Pset_filter to add a filter to the filter pipeline.
 * Inputs:      prp_id - property list identifier 
 *              filter - Filter to be added to the pipeline.
 *              flags - Bit vector specifying certain general
 *                      properties of the filter.
 *              cd_nelmts - Number of elements in cd_values.
 *              cd_values - Auxiliary data for the filter.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_filter_c (hid_t_f *prp_id, int_f* filter, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values )
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     size_t c_cd_nelmts;
     unsigned int c_flags;
     H5Z_filter_t c_filter;
     unsigned int * c_cd_values;
     int i;
     
     c_filter = (H5Z_filter_t)*filter;
     c_flags = (unsigned)*flags;
     c_cd_nelmts = (size_t)*cd_nelmts;
     c_cd_values = (unsigned int*)malloc(sizeof(unsigned int)*((int)c_cd_nelmts));
     if (!c_cd_values) return ret_value;
     for (i = 0; i < c_cd_nelmts; i++)
          c_cd_values[i] = (unsigned int)cd_values[i];

     /*
      * Call H5Pset_filter function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_filter(c_prp_id, c_filter, c_flags, c_cd_nelmts,c_cd_values );

     if (ret < 0) return ret_value;
     ret_value = 0;
     free(c_cd_values);
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_nfilters_c
 * Purpose:     Call H5Pget_nfilters to get the number of filters
 *              in the pipeline
 * Inputs:      prp_id - property list identifier
 * Outputs:     nfilters - number of filters defined in the filter pipline
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_nfilters_c (hid_t_f *prp_id, int_f* nfilters)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_nfilters;
     /*
      * Call H5Pget_nfilters function.
      */
     c_prp_id = *prp_id;
     c_nfilters = H5Pget_nfilters(c_prp_id);
     if (c_nfilters < 0) return ret_value;
     *nfilters = (int_f)c_nfilters;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_filter_c
 * Purpose:     Call H5Pget_filter to get information about a filter 
 *              in a pipeline
 * Inputs:      prp_id - property list identifier
 *              filter_number - Sequence number within the filter 
 *                              pipeline of the filter for which 
 *                              information is sought.
 *              namelen - Anticipated number of characters in name.
 *Outputs:      flags - Bit vector specifying certain general 
 *                      properties of the filter.
 *              cd_nelmts - Number of elements in cd_value
 *              cd_values - Auxiliary data for the filter. 
 *              name - Name of the filter
 *              filter_id - filter identification number
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_filter_c(hid_t_f *prp_id, int_f* filter_number, int_f* flags, size_t_f* cd_nelmts, int_f* cd_values, size_t_f *namelen, _fcd name, int_f* filter_id)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_filter_number;
     unsigned int  c_flags;
     size_t c_cd_nelmts, c_namelen;
     H5Z_filter_t c_filter;
     unsigned int * c_cd_values;
     char* c_name;
     int i;

     c_cd_nelmts = (size_t)*cd_nelmts;
     c_namelen = (size_t)*namelen;
     c_name = (char*)malloc(sizeof(char)*c_namelen);
     if (!c_name) return ret_value;

     c_cd_values = (unsigned int*)malloc(sizeof(unsigned int)*((int)c_cd_nelmts));
     if (!c_cd_values) return ret_value;
 
     /*
      * Call H5Pget_filter function.
      */
     c_prp_id = *prp_id;
     c_filter_number = *filter_number;
     c_filter = H5Pget_filter(c_prp_id, c_filter_number, &c_flags, &c_cd_nelmts, c_cd_values, c_namelen, c_name);

     if (c_filter < 0) return ret_value;

     *filter_id = c_filter;
     *cd_nelmts = c_cd_nelmts;
     *flags = c_flags;
     HD5packFstring(c_name, _fcdtocp(name), strlen(c_name)); 
 
     for (i = 0; i < c_cd_nelmts; i++)
          cd_values[i] = (int_f)c_cd_values[i];
    
     HDfree(c_name);
     HDfree(c_cd_values);
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_external_c
 * Purpose:     Call H5Pset_external to add an external file to the
 *              list of external files.
 * Inputs:      prp_id - property list identifier 
 *              name - Name of an external file
 *              namelen - length of name
 *              offset - Offset, in bytes, from the beginning of the file
 *                       to the location in the file where the data starts. 
 *              bytes - Number of bytes reserved in the file for the data. 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_external_c (hid_t_f *prp_id, _fcd name, int_f* namelen, int_f* offset, hsize_t_f*bytes)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     hsize_t c_bytes;
     char* c_name;
     int c_namelen;
     off_t c_offset;

     c_bytes = (hsize_t) *bytes;
     c_offset = (off_t)*offset;
     
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, c_namelen); 
     if (c_name == NULL) return ret_value;

     /*
      * Call H5Pset_external function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_external(c_prp_id, c_name, c_offset, c_bytes);
     HDfree(c_name);

     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_external_count_c
 * Purpose:     Call H5Pget_external_count to get the number of external
 *              files for the specified dataset.
 * Inputs:      prp_id - property list identifier
 * Outputs:     count - number of external files
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_external_count_c (hid_t_f *prp_id, int_f* count)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_count;
     /*
      * Call H5Pget_external_count function.
      */
     c_prp_id = *prp_id;
     c_count = H5Pget_external_count(c_prp_id);
     if (c_count < 0) return ret_value;
     *count = (int_f)c_count;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_external_c
 * Purpose:     Call H5Pget_external to get nformation about an external file. 
 * Inputs:      prp_id - property list identifier 
 *              name_size - length of name
 *              idx - External file index. 
 *Outputs:      name - Name of an external file
 *              offset - Offset, in bytes, from the beginning of the file
 *                       to the location in the file where the data starts. 
 *              bytes - Number of bytes reserved in the file for the data. 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, February 23, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_external_c(hid_t_f *prp_id,int*idx, size_t_f* name_size, _fcd name, int_f* offset, hsize_t_f*bytes)
{
     int ret_value = -1;
     hid_t c_prp_id;
     int c_idx;
     herr_t status;
     size_t c_namelen;
     char* c_name;
     int i;
     off_t c_offset;
     hsize_t size;

     c_namelen = (size_t)*name_size;
     /*
      * Allocate memory to store the name of the external file.
      */
     if(c_namelen) c_name = (char*) HDmalloc(c_namelen + 1);
 
     /*
      * Call H5Pget_external function.
      */
     c_prp_id = *prp_id;
     c_idx = *idx;
     status = H5Pget_external(c_prp_id, c_idx, c_namelen, c_name, &c_offset, &size );

     if (status < 0) {
                      HDfree(c_name);
                      return ret_value;
     }

     *offset = (int_f)c_offset;
     *bytes = (hsize_t_f)size;
     HD5packFstring(c_name, _fcdtocp(name), strlen(c_name)); 
    
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_hyper_cache_c
 * Purpose:     Call H5Pset__hyper_cache to indicate whether to 
 *              cache hyperslab blocks during I/O. 
 * Inputs:      prp_id - property list identifier
 *              cache - 
 *              limit - Maximum size of the hyperslab block to cache.
 *                      0 (zero) indicates no limit.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pset_hyper_cache_c(hid_t_f *prp_id, int_f* cache, int_f* limit)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     unsigned c_cache, c_limit;
     c_cache = (unsigned) *cache;
     c_limit = (unsigned) *limit;

     /*
      * Call H5Pset_hyper_cache function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_hyper_cache(c_prp_id, c_cache, c_limit);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_hyper_cache_c
 * Purpose:     Call H5Pget_hyper_cache to get information regarding 
 *              the caching of hyperslab blocks 
 * Inputs:      prp_id - property list identifier
 *              cache - 
 *              limit - Maximum size of the hyperslab block to cache.
 *                      0 (zero) indicates no limit.
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5pget_hyper_cache_c(hid_t_f *prp_id, int_f* cache, int_f* limit)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     unsigned c_cache, c_limit;
     /*
      * Call H5Pget__hyper_cache function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_hyper_cache(c_prp_id, &c_cache, &c_limit);
     if (ret < 0) return ret_value;
     *cache = (int_f)c_cache;
     *limit = (int_f)c_limit;   
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pset_btree_ratios_c
 * Purpose:     Call H5Pset_btree_ratios to set B-tree split ratios for B-tree split ratios for a dataset transfer property list. a 
 *              dataset transfer property list. 
 * Inputs:      prp_id - property list identifier
 *              left - The B-tree split ratio for left-most nodes. 
 *              middle - The B-tree split ratio for all other nodes 
 *              right - The B-tree split ratio for right-most nodes 
 *                      and lone nodes. 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed the type of the last three parameters from double to real_f
 *---------------------------------------------------------------------------*/
int_f
nh5pset_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     double c_left;
     double c_middle;
     double c_right;
     c_left = (double)*left;
     c_middle = (double)*middle;
     c_right = (double)*right;

     /*
      * Call H5Pset_btree_ratios function.
      */
     c_prp_id = *prp_id;
     ret = H5Pset_btree_ratios(c_prp_id, c_left, c_middle, c_right);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 

/*----------------------------------------------------------------------------
 * Name:        h5pget_btree_ratios_c
 * Purpose:     Call H5Pget_btree_ratios to Gets B-tree split ratios 
 *              for a dataset transfer property list. 
 * Inputs:      prp_id - property list identifier
 *              left - The B-tree split ratio for left-most nodes. 
 *              middle - The B-tree split ratio for all other nodes 
 *              right - The B-tree split ratio for right-most nodes 
 *                      and lone nodes. 
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Friday, February 25, 2000
 * Modifications: Changed the type of the last three parameters from double to real_f
 *---------------------------------------------------------------------------*/
int_f
nh5pget_btree_ratios_c(hid_t_f *prp_id, real_f* left, real_f* middle, real_f* right)
{
     int ret_value = -1;
     hid_t c_prp_id;
     herr_t ret;
     double c_left, c_right, c_middle;

     /*
      * Call H5Pget_btree_ratios function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_btree_ratios(c_prp_id, &c_left, &c_middle, &c_right);
     *left = (real_f)c_left;
     *middle = (real_f)c_middle;
     *right = (real_f)c_right;
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
} 
