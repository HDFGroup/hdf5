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

/* This file contains C stubs to the Fortran Library initialization APIs */


#include "H5f90.h"

/*---------------------------------------------------------------------------
 * Name:              h5init_types_c
 * Purpose:           Initialize predefined datatypes in Fortran
 * Inputs:            types - array with the predefined Native Fortran 
 *                            type, its element and length must be the
 *                            same as the types array defined in the 
 *                            H5f90global.f90
 *                    floatingtypes - array with the predefined Floating Fortran 
 *                                    type, its element and length must be the
 *                                    same as the floatingtypes array defined in the 
 *                                    H5f90global.f90
 *                    integertypes - array with the predefined Integer Fortran 
 *                                   type, its element and length must be the
 *                                   same as the integertypes array defined in the 
 *                                   H5f90global.f90
 * Outputs:           None 
 * Returns:           0 on success, -1 on failure
 * Programmer:        Elena Pourmal  
 *                    Tuesday, August 3, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5init_types_c( hid_t_f * types, hid_t_f * floatingtypes, hid_t_f * integertypes )
{

    int ret_value = -1;
    hid_t c_type_id;
    if ((types[0] = (hid_t_f)H5Tcopy(H5T_NATIVE_INT)) < 0) return ret_value;
#if defined(_UNICOS)
    if ((types[1] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
#else
    if ((types[1] = (hid_t_f)H5Tcopy(H5T_NATIVE_FLOAT)) < 0) return ret_value;
#endif
    if ((types[2] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
/*
    if ((types[3] = H5Tcopy(H5T_NATIVE_UINT8)) < 0) return ret_value;
*/
    if ((c_type_id = H5Tcopy(H5T_FORTRAN_S1)) < 0) return ret_value;
    if(H5Tset_size(c_type_id, 1) < 0) return ret_value;
    if(H5Tset_strpad(c_type_id, H5T_STR_SPACEPAD) < 0) return ret_value;
    types[3] = (hid_t_f)c_type_id;



/*
    if ((types[3] = H5Tcopy(H5T_C_S1)) < 0) return ret_value;
    if(H5Tset_strpad(types[3],H5T_STR_NULLTERM) < 0) return ret_value;
    if(H5Tset_size(types[3],1) < 0) return ret_value;
*/


/*    if ((types[3] = H5Tcopy(H5T_STD_I8BE)) < 0) return ret_value;
*/
    if ((types[4] = (hid_t_f)H5Tcopy(H5T_STD_REF_OBJ)) < 0) return ret_value;
    if ((types[5] = (hid_t_f)H5Tcopy(H5T_STD_REF_DSETREG)) < 0) return ret_value;

    if ((floatingtypes[0] = (hid_t_f)H5Tcopy(H5T_IEEE_F32BE)) < 0) return ret_value;
    if ((floatingtypes[1] = (hid_t_f)H5Tcopy(H5T_IEEE_F32LE)) < 0) return ret_value;
    if ((floatingtypes[2] = (hid_t_f)H5Tcopy(H5T_IEEE_F64BE)) < 0) return ret_value;
    if ((floatingtypes[3] = (hid_t_f)H5Tcopy(H5T_IEEE_F64LE)) < 0) return ret_value;

    if ((integertypes[0] = (hid_t_f)H5Tcopy(H5T_STD_I8BE)) < 0) return ret_value;
    if ((integertypes[1] = (hid_t_f)H5Tcopy(H5T_STD_I8LE)) < 0) return ret_value;
    if ((integertypes[2] = (hid_t_f)H5Tcopy(H5T_STD_I16BE)) < 0) return ret_value;
    if ((integertypes[3] = (hid_t_f)H5Tcopy(H5T_STD_I16LE)) < 0) return ret_value;
    if ((integertypes[4] = (hid_t_f)H5Tcopy(H5T_STD_I32BE)) < 0) return ret_value;
    if ((integertypes[5] = (hid_t_f)H5Tcopy(H5T_STD_I32LE)) < 0) return ret_value;
    if ((integertypes[6] = (hid_t_f)H5Tcopy(H5T_STD_I64BE)) < 0) return ret_value;
    if ((integertypes[7] = (hid_t_f)H5Tcopy(H5T_STD_I64LE)) < 0) return ret_value;
    if ((integertypes[8] = (hid_t_f)H5Tcopy(H5T_STD_U8BE)) < 0) return ret_value;
    if ((integertypes[9] = (hid_t_f)H5Tcopy(H5T_STD_U8LE)) < 0) return ret_value;
    if ((integertypes[10] = (hid_t_f)H5Tcopy(H5T_STD_U16BE)) < 0) return ret_value;
    if ((integertypes[11] = (hid_t_f)H5Tcopy(H5T_STD_U16LE)) < 0) return ret_value;
    if ((integertypes[12] = (hid_t_f)H5Tcopy(H5T_STD_U32BE)) < 0) return ret_value;
    if ((integertypes[13] = (hid_t_f)H5Tcopy(H5T_STD_U32LE)) < 0) return ret_value;
    if ((integertypes[14] = (hid_t_f)H5Tcopy(H5T_STD_U64BE)) < 0) return ret_value;
    if ((integertypes[15] = (hid_t_f)H5Tcopy(H5T_STD_U64LE)) < 0) return ret_value;

    ret_value = 0; 
    return ret_value;
}
    
/*---------------------------------------------------------------------------
 * Name:              h5close_types_c
 * Purpose:           Closes predefined datatype in Fortran
 * Inputs:            types - array with the predefined Native Fortran 
 *                            type, its element and length must be the
 *                            same as the types array defined in the 
 *                            H5f90global.f90
 *                    lentypes - length of the types array, which must be the
 *                               same as the length of types array defined 
 *                               in the H5f90global.f90
 *                    floatingtypes - array with the predefined Floating Fortran 
 *                                    type, its element and length must be the
 *                                    same as the floatingtypes array defined in the 
 *                                    H5f90global.f90
 *                    floatinglen - length of the floatingtypes array, which must be the
 *                                  same as the length of floatingtypes array defined 
 *                                  in the H5f90global.f90
 *                    integertypes - array with the predefined Integer Fortran 
 *                                   type, its element and length must be the
 *                                   same as the integertypes array defined in the 
 *                                   H5f90global.f90
 *                    integerlen - length of the floatingtypes array, which must be the
 *                                 same as the length of floatingtypes array defined 
 *                                 in the H5f90global.f90
 * Outputs:           None 
 * Returns:           0 on success, -1 on failure
 * Programmer:        Elena Pourmal  
 *                    Tuesday, August 3, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5close_types_c( hid_t_f * types, int_f *lentypes, 
                  hid_t_f * floatingtypes, int_f* floatinglen,
                  hid_t_f * integertypes,  int_f * integerlen )
{

    int ret_value = -1;
    hid_t c_type_id;
    herr_t err;
    int i;
    for (i = 0; i < *lentypes; i++) {
    c_type_id = types[i];
    if ( (err = H5Tclose(c_type_id)) < 0) return ret_value;
    }
    for (i = 0; i < *floatinglen; i++) {
    c_type_id = floatingtypes[i];
    if ( (err = H5Tclose(c_type_id)) < 0) return ret_value;
    }
    for (i = 0; i < *integerlen; i++) {
    c_type_id = integertypes[i];
    if ( (err = H5Tclose(c_type_id)) < 0) return ret_value;
    }
    ret_value = 0; 
    return ret_value;
}    
/*---------------------------------------------------------------------------
 * Name:              h5init_flags_c
 * Purpose:           Initialize Fortran flags
 * Inputs:            h5d_flags    - H5D inteface flags 
 *                    h5e_flags    - H5E interface flags
 *                    h5f_flags    - H5F interface flags
 *                    h5fd_flags    - H5FD interface flags
 *                    h5g_flags    - H5G interface flags
 *                    h5i_flags    - H5I interface flags
 *                    h5p_flags    - H5P interface flags
 *                    h5r_flags    - H5R interface flags
 *                    h5s_flags    - H5S interface flags
 *                    h5t_flags    - H5T interface flags
 * Outputs:           None 
 * Returns:           0 on success, -1 on failure
 * Programmer:        Elena Pourmal  
 *                    Tuesday, August 3, 1999
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5init_flags_c( int_f *h5d_flags, int_f *h5e_flags, int_f *h5f_flags,
                 int_f *h5fd_flags, int_f *h5g_flags, int_f *h5i_flags,
                 int_f *h5p_flags, int_f *h5r_flags, int_f *h5s_flags,
                 int_f *h5t_flags)
{
    int ret_value = -1;
/*
 *  H5D flags
 */
    h5d_flags[0] = H5D_COMPACT;
    h5d_flags[1] = H5D_CONTIGUOUS;
    h5d_flags[2] = H5D_CHUNKED;

/*
 *  H5E flags
 */
    
      h5e_flags[0] = H5E_NONE_MAJOR;
      h5e_flags[1] = H5E_ARGS;
      h5e_flags[2] = H5E_RESOURCE;
      h5e_flags[3] = H5E_INTERNAL;
      h5e_flags[4] = H5E_FILE;
      h5e_flags[5] = H5E_IO;
      h5e_flags[6] = H5E_FUNC;
      h5e_flags[7] = H5E_ATOM;
      h5e_flags[8] = H5E_CACHE;
      h5e_flags[9] = H5E_BTREE;
      h5e_flags[10] = H5E_SYM;
      h5e_flags[11] = H5E_HEAP;
      h5e_flags[12] = H5E_OHDR;
      h5e_flags[13] = H5E_DATATYPE;
      h5e_flags[14] = H5E_DATASPACE;
      h5e_flags[15] = H5E_DATASET;
      h5e_flags[16] = H5E_STORAGE;
      h5e_flags[17] = H5E_PLIST;
      h5e_flags[18] = H5E_ATTR;
      h5e_flags[19] = H5E_PLINE;
      h5e_flags[20] = H5E_EFL;
      h5e_flags[21] = H5E_REFERENCE;
      h5e_flags[22] = H5E_VFL;
      h5e_flags[23] = H5E_TBBT;
    
/*
 *  H5F flags
 */
      h5f_flags[0] = (int_f)H5F_ACC_RDWR; 
      h5f_flags[1] = (int_f)H5F_ACC_RDONLY;
      h5f_flags[2] = (int_f)H5F_ACC_TRUNC;
      h5f_flags[3] = (int_f)H5F_ACC_EXCL;
      h5f_flags[4] = (int_f)H5F_ACC_DEBUG;
      h5f_flags[5] = (int_f)H5F_SCOPE_LOCAL;
      h5f_flags[6] = (int_f)H5F_SCOPE_GLOBAL;

/*
 *  H5FD flags
 */
      h5fd_flags[0] = H5FD_MPIO_INDEPENDENT;
      h5fd_flags[1] = H5FD_MPIO_COLLECTIVE;

/*
 *  H5G flags
 */

      h5g_flags[0] = H5G_UNKNOWN;
      h5g_flags[1] = H5G_LINK;
      h5g_flags[2] = H5G_GROUP;
      h5g_flags[3] = H5G_DATASET;
      h5g_flags[4] = H5G_TYPE;
      h5g_flags[5] = H5G_LINK_ERROR;
      h5g_flags[6] = H5G_LINK_HARD;
      h5g_flags[7] = H5G_LINK_SOFT;

/*
 *  H5I flags
 */

      h5i_flags[0] = H5I_FILE;
      h5i_flags[1] = H5I_GROUP;
      h5i_flags[2] = H5I_DATATYPE;
      h5i_flags[3] = H5I_DATASPACE;
      h5i_flags[4] = H5I_DATASET;
      h5i_flags[5] = H5I_ATTR;
      h5i_flags[6] = H5I_BADID;

/*
 *  H5P flags
 */

      h5p_flags[0] = H5P_FILE_CREATE; 
      h5p_flags[1] = H5P_FILE_ACCESS; 
      h5p_flags[2] = H5P_DATASET_CREATE;
      h5p_flags[3] = H5P_DATASET_XFER;
      h5p_flags[4] = H5P_MOUNT;
      h5p_flags[5] = H5P_DEFAULT;

/*
 *  H5R flags
 */

      h5r_flags[0] = H5R_OBJECT;
      h5r_flags[1] = H5R_DATASET_REGION;

/*
 *  H5S flags
 */


      h5s_flags[0] = H5S_SCALAR; 
      h5s_flags[1] = H5S_SIMPLE; 
      h5s_flags[2] = H5S_SELECT_SET;
      h5s_flags[3] = H5S_SELECT_OR;
      h5s_flags[4] = (int_f)H5S_UNLIMITED;
      h5s_flags[5] = H5S_ALL;

/*
 *  H5T flags
 */


      h5t_flags[0] = H5T_NO_CLASS; 
      h5t_flags[1] = H5T_INTEGER; 
      h5t_flags[2] = H5T_FLOAT;  
      h5t_flags[3] = H5T_TIME; 
      h5t_flags[4] = H5T_STRING; 
      h5t_flags[5] = H5T_BITFIELD;
      h5t_flags[6] = H5T_OPAQUE;
      h5t_flags[7] = H5T_COMPOUND; 
      h5t_flags[8] = H5T_REFERENCE;
      h5t_flags[9] = H5T_ENUM; 
      h5t_flags[10] = H5T_ORDER_LE; 
      h5t_flags[11] = H5T_ORDER_BE;
      h5t_flags[12] = H5T_ORDER_VAX;
      h5t_flags[13] = H5T_PAD_ZERO;
      h5t_flags[14] = H5T_PAD_ONE;
      h5t_flags[15] = H5T_PAD_BACKGROUND;
      h5t_flags[16] = H5T_PAD_ERROR;    
      h5t_flags[17] = H5T_SGN_NONE;   
      h5t_flags[18] = H5T_SGN_2;     
      h5t_flags[19] = H5T_SGN_ERROR;
      h5t_flags[20] = H5T_NORM_IMPLIED;
      h5t_flags[21] = H5T_NORM_MSBSET;
      h5t_flags[22] = H5T_NORM_NONE; 
      h5t_flags[23] = H5T_CSET_ASCII;
      h5t_flags[24] = H5T_STR_NULLTERM;
      h5t_flags[25] = H5T_STR_NULLPAD;
      h5t_flags[26] = H5T_STR_SPACEPAD;
      h5t_flags[27] = H5T_STR_ERROR;

    ret_value = 0; 
    return ret_value;
}
/*---------------------------------------------------------------------------
 * Name:              h5open_c
 * Purpose:           Calls H5open call to initialize C HDF5 library
 * Returns:           0 on success, -1 on failure
 * Programmer:        Elena Pourmal  
 *                    Friday, November 17, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5open_c()
{

    int ret_value = -1;
    if (H5open() < 0) return ret_value;  
    ret_value = 0; 
    return ret_value;
}    
/*---------------------------------------------------------------------------
 * Name:              h5close_c
 * Purpose:           Calls H5close call to close C HDF5 library
 * Returns:           0 on success, -1 on failure
 * Programmer:        Elena Pourmal  
 *                    Friday, November 17, 2000 
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5close_c()
{

    int ret_value = -1;
    if (H5close() < 0) return ret_value;  
    ret_value = 0; 
    return ret_value;
}    
