/****ih* H5_f/H5_f
 * PURPOSE
 *  This file contains C stubs for H5 Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
*/

#include "H5f90.h"
#include "H5fort_type_defines.h"

int IntKinds_SizeOf[] = H5_FORTRAN_INTEGER_KINDS_SIZEOF;


/****if* H5_f/h5init_types_c
 * NAME
 *  h5init_types_c
 * PURPOSE
 *  Initialize predefined datatypes in Fortran
 * INPUTS
 *  types - array with the predefined Native Fortran
 *          type, its element and length must be the
 *          same as the types array defined in the
 *          H5f90global.F90
 *  floatingtypes - array with the predefined Floating Fortran
 *                  type, its element and length must be the
 *                  same as the floatingtypes array defined in the
 *                  H5f90global.F90
 *  integertypes - array with the predefined Integer Fortran
 *                 type, its element and length must be the
 *                 same as the integertypes array defined in the
 *                 H5f90global.F90
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, August 3, 1999
 * SOURCE
*/
int_f
h5init_types_c( hid_t_f * types, hid_t_f * floatingtypes, hid_t_f * integertypes )
/******/
{
    int ret_value = -1;
    hid_t c_type_id;
    size_t tmp_val;
    int i;

    /* Fortran INTEGER may not be the same as C; do all checking to find
       an appropriate size
    */

    /*
     * Find the HDF5 type of the Fortran Integer KIND.
     */

    /* Initialized INTEGER KIND types to default to native integer */
    for(i=0;i<5;i++) {
      if ((types[i] = (hid_t_f)H5Tcopy (H5T_NATIVE_INT)) < 0) return ret_value;
    }

    for(i=0;i<H5_FORTRAN_NUM_INTEGER_KINDS;i++) {
      if ( IntKinds_SizeOf[i] == sizeof(char)) {
	if ((types[i] = (hid_t_f)H5Tcopy(H5T_NATIVE_CHAR)) < 0) return ret_value;
      } /*end if */
      else if ( IntKinds_SizeOf[i] == sizeof(short)) {
	if ((types[i] = (hid_t_f)H5Tcopy(H5T_NATIVE_SHORT)) < 0) return ret_value;
      } /*end if */
      else if ( IntKinds_SizeOf[i] == sizeof(int)) {
	if ((types[i] = (hid_t_f)H5Tcopy(H5T_NATIVE_INT)) < 0) return ret_value;
      } /*end if */
      else if ( IntKinds_SizeOf[i] == sizeof(long long)) {
	if ((types[i] = (hid_t_f)H5Tcopy(H5T_NATIVE_LLONG)) < 0) return ret_value;
      } /*end if */
      else {
	if ((types[i] = (hid_t_f)H5Tcopy (H5T_NATIVE_INT)) < 0) return ret_value;
	if ( H5Tset_precision (types[i], 128) < 0) return ret_value;
      } /*end else */

    }

    if (sizeof(int_f) == sizeof(int)) {
      if ((types[5] = (hid_t_f)H5Tcopy(H5T_NATIVE_INT)) < 0) return ret_value;
    } /*end if */
    else if (sizeof(int_f) == sizeof(long)) {
      if ((types[5] = (hid_t_f)H5Tcopy(H5T_NATIVE_LONG)) < 0) return ret_value;
    } /*end if */
    else
      if (sizeof(int_f) == sizeof(long long)) {
	if ((types[5] = (hid_t_f)H5Tcopy(H5T_NATIVE_LLONG)) < 0) return ret_value;
    } /*end else */
    
    /* Find appropriate size to store Fortran REAL */
    if(sizeof(real_f)==sizeof(float)) {
      if ((types[6] = (hid_t_f)H5Tcopy(H5T_NATIVE_FLOAT)) < 0) return ret_value;
    } /* end if */
    else if(sizeof(real_f)==sizeof(double)){
      if ((types[6] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
    } /* end if */
#if H5_SIZEOF_LONG_DOUBLE!=0
    else if (sizeof(real_f) == sizeof(long double)) {
      if ((types[6] = (hid_t_f)H5Tcopy(H5T_NATIVE_LDOUBLE)) < 0) return ret_value;
    } /* end else */
#endif

    /* Find appropriate size to store Fortran DOUBLE */
    if(sizeof(double_f)==sizeof(double)) {
       if ((types[7] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
    }/*end if */
#if H5_SIZEOF_LONG_DOUBLE!=0
    else if(sizeof(double_f)==sizeof(long double)) {
       if ((types[7] = (hid_t_f)H5Tcopy(H5T_NATIVE_LDOUBLE)) < 0) return ret_value;
    }/*end else */
#endif
#ifdef H5_HAVE_FLOAT128
    else if(sizeof(double_f)==sizeof(__float128)) {
      if ((types[7] = H5Tcopy (H5T_NATIVE_FLOAT)) < 0) return ret_value;
      if ( H5Tset_precision (types[7], 128) < 0) return ret_value;
    }/*end else */
#endif

    if ((c_type_id = H5Tcopy(H5T_FORTRAN_S1)) < 0) return ret_value;
    tmp_val = 1;
    if(H5Tset_size(c_type_id, tmp_val) < 0) return ret_value;
    if(H5Tset_strpad(c_type_id, H5T_STR_SPACEPAD) < 0) return ret_value;
    types[8] = (hid_t_f)c_type_id;

    if ((types[9] = (hid_t_f)H5Tcopy(H5T_STD_REF_OBJ)) < 0) return ret_value;
    if ((types[10] = (hid_t_f)H5Tcopy(H5T_STD_REF_DSETREG)) < 0) return ret_value;

    /*
     * FIND H5T_NATIVE_REAL_C_FLOAT
     */
    if (sizeof(real_C_FLOAT_f) == sizeof(float)) {
      if ((types[11] = (hid_t_f)H5Tcopy(H5T_NATIVE_FLOAT)) < 0) return ret_value;
    } /*end if */
    else if (sizeof(real_C_FLOAT_f) == sizeof(double)) {
      if ((types[11] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
    } /*end if */
#if H5_SIZEOF_LONG_DOUBLE!=0
    else if (sizeof(real_C_FLOAT_f) == sizeof(long double)) {
	if ((types[11] = (hid_t_f)H5Tcopy(H5T_NATIVE_LDOUBLE)) < 0) return ret_value;
    } /*end else */
#endif
    /*
     * FIND H5T_NATIVE_REAL_C_DOUBLE
     */
    if (sizeof(real_C_DOUBLE_f) == sizeof(float)) {
      if ((types[12] = (hid_t_f)H5Tcopy(H5T_NATIVE_FLOAT)) < 0) return ret_value;
    } /*end if */
    else if (sizeof(real_C_DOUBLE_f) == sizeof(double)) {
      if ((types[12] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
    } /*end if */
#if H5_SIZEOF_LONG_DOUBLE!=0
    else if (sizeof(real_C_DOUBLE_f) == sizeof(long double)) {
	if ((types[12] = (hid_t_f)H5Tcopy(H5T_NATIVE_LDOUBLE)) < 0) return ret_value;
    } /*end else */
#endif
    /*
     * FIND H5T_NATIVE_REAL_C_LONG_DOUBLE
     */
#if H5_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE!=0
    if (sizeof(real_C_LONG_DOUBLE_f) == sizeof(float)) {
      if ((types[13] = (hid_t_f)H5Tcopy(H5T_NATIVE_FLOAT)) < 0) return ret_value;
    } /*end if */
    else if (sizeof(real_C_LONG_DOUBLE_f) == sizeof(double)) {
      if ((types[13] = (hid_t_f)H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
    } /*end if */
# if H5_FORTRAN_HAVE_C_LONG_DOUBLE!=0
    else if (sizeof(real_C_LONG_DOUBLE_f) == sizeof(long double)) {
      if ( H5_PAC_C_MAX_REAL_PRECISION >= H5_PAC_FC_MAX_REAL_PRECISION) {
	if ((types[13] = (hid_t_f)H5Tcopy(H5T_NATIVE_LDOUBLE)) < 0) return ret_value;
      }
      else {
	if ((types[13] = H5Tcopy (H5T_NATIVE_FLOAT)) < 0) return ret_value;
	if ( H5Tset_precision (types[13], 128) < 0) return ret_value;
      }
    }
# else
    if ((types[13] = H5Tcopy (H5T_NATIVE_FLOAT)) < 0) return ret_value;
    if ( H5Tset_precision (types[13], 64) < 0) return ret_value;
# endif
#else
    if ((types[13] = H5Tcopy(H5T_NATIVE_DOUBLE)) < 0) return ret_value;
#endif
    /*
     * FIND H5T_NATIVE_B_8
     */
    if ((types[14] = (hid_t_f)H5Tcopy(H5T_NATIVE_B8))  < 0) return ret_value;
    if ((types[15] = (hid_t_f)H5Tcopy(H5T_NATIVE_B16)) < 0) return ret_value;
    if ((types[16] = (hid_t_f)H5Tcopy(H5T_NATIVE_B32)) < 0) return ret_value;
    if ((types[17] = (hid_t_f)H5Tcopy(H5T_NATIVE_B64)) < 0) return ret_value;

    /*
     * FIND H5T_NATIVE_FLOAT_128
     */
    if ((types[18] = H5Tcopy (H5T_NATIVE_FLOAT)) < 0) return ret_value;
    if ( H5Tset_precision (types[18], 128) < 0) return ret_value;

    /*--------------------------------------------------------------------------------------*/

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
    if ((integertypes[17] = (hid_t_f)H5Tcopy(H5T_STD_B8BE)) < 0) return ret_value;
    if ((integertypes[18] = (hid_t_f)H5Tcopy(H5T_STD_B8LE)) < 0) return ret_value;
    if ((integertypes[19] = (hid_t_f)H5Tcopy(H5T_STD_B16BE)) < 0) return ret_value;
    if ((integertypes[20] = (hid_t_f)H5Tcopy(H5T_STD_B16LE)) < 0) return ret_value;
    if ((integertypes[21] = (hid_t_f)H5Tcopy(H5T_STD_B32BE)) < 0) return ret_value;
    if ((integertypes[22] = (hid_t_f)H5Tcopy(H5T_STD_B32LE)) < 0) return ret_value;
    if ((integertypes[23] = (hid_t_f)H5Tcopy(H5T_STD_B64BE)) < 0) return ret_value;
    if ((integertypes[24] = (hid_t_f)H5Tcopy(H5T_STD_B64LE)) < 0) return ret_value;
    if ((integertypes[25] = (hid_t_f)H5Tcopy(H5T_FORTRAN_S1)) < 0) return ret_value;
    if ((integertypes[26] = (hid_t_f)H5Tcopy(H5T_C_S1)) < 0) return ret_value;

/*
 *  Define Fortran H5T_STRING type to store non-fixed size strings
 */
    if ((c_type_id = H5Tcopy(H5T_C_S1)) < 0) return ret_value;
    if(H5Tset_size(c_type_id, H5T_VARIABLE) < 0) return ret_value;
    integertypes[16] = c_type_id;

    ret_value = 0;
    return ret_value;
}
/****if* H5_f/h5close_types_c
 * NAME
 *  h5close_types_c
 * PURPOSE
 *  Closes predefined datatype in Fortran
 * INPUTS
 *  types         - array with the predefined Native Fortran
 *  type, its element and length must be the
 *  same as the types array defined in the
 *                  H5f90global.F90
 *  lentypes      - length of the types array, which must be the
 *  same as the length of types array defined
 *  in the H5f90global.F90
 *  floatingtypes - array with the predefined Floating Fortran
 *  type, its element and length must be the
 *  same as the floatingtypes array defined in the
 *                  H5f90global.F90
 *  floatinglen   - length of the floatingtypes array, which must be the
 *  same as the length of floatingtypes array defined
 *  in the H5f90global.F90
 *  integertypes  - array with the predefined Integer Fortran
 *  type, its element and length must be the
 *  same as the integertypes array defined in the
 *                  H5f90global.F90
 *  integerlen    - length of the floatingtypes array, which must be the
 *  same as the length of floatingtypes array defined
 *  in the H5f90global.F90
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, August 3, 1999
 * SOURCE
 */
int_f
h5close_types_c( hid_t_f * types, int_f *lentypes,
                  hid_t_f * floatingtypes, int_f* floatinglen,
                  hid_t_f * integertypes,  int_f * integerlen )
/******/
{
    int ret_value = -1;
    hid_t c_type_id;
    int i;

    for (i = 0; i < *lentypes; i++) {
      c_type_id = types[i];
      if ( H5Tclose(c_type_id) < 0) return ret_value;
    }
    for (i = 0; i < *floatinglen; i++) {
      c_type_id = floatingtypes[i];
      if ( H5Tclose(c_type_id) < 0) return ret_value;
    }
    for (i = 0; i < *integerlen; i++) {
      c_type_id = integertypes[i];
      if ( H5Tclose(c_type_id) < 0) return ret_value;
    }
    ret_value = 0;
    return ret_value;
}
/****if* H5_f/h5init_flags_c
 * NAME
 *  h5init_flags_c
 * PURPOSE
 *  Initialize Fortran flags
 * INPUTS
 *  h5d_flags       - H5D interface flags
 *  h5d_size_flags  - H5D interface flags of type size_t
 *  h5e_flags       - H5E interface flags
 *  h5e_hid_flags   - H5E interface flags of type hid_t
 *  h5f_flags       - H5F interface flags
 *  h5fd_flags      - H5FD interface flags
 *  h5fd_hid_flags  - H5FD interface flags of type hid_t
 *  h5g_flags       - H5G interface flags
 *  h5i_flags       - H5I interface flags
 *  h5p_flags       - H5P interface flags
 *  h5p_flags_int   - H5P interface flags of type integer
 *  h5r_flags       - H5R interface flags
 *  h5s_flags       - H5S interface flags
 *  h5s_hid_flags   - H5S interface flags of type hid_t
 *  h5s_hsize_flags - H5S interface flags of type hsize_t
 *  h5t_flags       - H5T interface flags
 *  h5z_flags       - H5Z interface flags
 * OUTPUTS
 *  None
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, August 3, 1999
 * HISTORY
 *  Added Z flags. EIP,  March 12, 2003
 *  Added more FD flags and new H5LIB flags
 *  Added more FD flags for HDF5 file driver
 *           EIP, April 9, 2005
 *  Added Generic flags introduced in version 1.8
 *           MSB, January, 2008
 *  Added types in lines h5*_flags = ( )variable to match input
 *  Added E flags
 *           MSB, July 9, 2009
 *  Added type h5d_flags of type size_t
 *           MSB, Feb. 28, 2014
 *  Added type h5s_hid_flags of type hid_t
 *           MSB, Oct. 10, 2016
 * SOURCE
 */
int_f
h5init_flags_c( int_f *h5d_flags, size_t_f *h5d_size_flags, 
		int_f *h5e_flags, hid_t_f *h5e_hid_flags, int_f *h5f_flags,
                int_f *h5fd_flags, hid_t_f *h5fd_hid_flags,
                int_f *h5g_flags, int_f *h5i_flags, int_f *h5l_flags, int_f *h5o_flags,
                hid_t_f *h5p_flags, int_f *h5p_flags_int, int_f *h5r_flags, 
                int_f *h5s_flags, hid_t_f *h5s_hid_flags, hsize_t_f *h5s_hsize_flags, 
		int_f *h5t_flags, int_f *h5z_flags, int_f *h5_generic_flags,
                haddr_t_f *h5_haddr_generic_flags)
/******/
{
    int ret_value = -1;
/*
 *  H5D flags
 */
    h5d_size_flags[0] = (size_t_f)H5D_CHUNK_CACHE_NSLOTS_DEFAULT;
    h5d_size_flags[1] = (size_t_f)H5D_CHUNK_CACHE_NBYTES_DEFAULT;

    h5d_flags[0] = (int_f)H5D_COMPACT;
    h5d_flags[1] = (int_f)H5D_CONTIGUOUS;
    h5d_flags[2] = (int_f)H5D_CHUNKED;
    h5d_flags[3] = (int_f)H5D_ALLOC_TIME_ERROR;
    h5d_flags[4] = (int_f)H5D_ALLOC_TIME_DEFAULT;
    h5d_flags[5] = (int_f)H5D_ALLOC_TIME_EARLY;
    h5d_flags[6] = (int_f)H5D_ALLOC_TIME_LATE;
    h5d_flags[7] = (int_f)H5D_ALLOC_TIME_INCR;
    h5d_flags[8] = (int_f)H5D_SPACE_STATUS_ERROR;
    h5d_flags[9] = (int_f)H5D_SPACE_STATUS_NOT_ALLOCATED;
    h5d_flags[10] = (int_f)H5D_SPACE_STATUS_PART_ALLOCATED;
    h5d_flags[11] = (int_f)H5D_SPACE_STATUS_ALLOCATED;
    h5d_flags[12] = (int_f)H5D_FILL_TIME_ERROR;
    h5d_flags[13] = (int_f)H5D_FILL_TIME_ALLOC;
    h5d_flags[14] = (int_f)H5D_FILL_TIME_NEVER;
    h5d_flags[15] = (int_f)H5D_FILL_VALUE_ERROR;
    h5d_flags[16] = (int_f)H5D_FILL_VALUE_UNDEFINED;
    h5d_flags[17] = (int_f)H5D_FILL_VALUE_DEFAULT;
    h5d_flags[18] = (int_f)H5D_FILL_VALUE_USER_DEFINED;
    h5d_flags[19] = (int_f)H5D_CHUNK_CACHE_W0_DEFAULT;
    h5d_flags[20] = (int_f)H5D_MPIO_NO_COLLECTIVE;
    h5d_flags[21] = (int_f)H5D_MPIO_CHUNK_INDEPENDENT;
    h5d_flags[22] = (int_f)H5D_MPIO_CHUNK_COLLECTIVE;
    h5d_flags[23] = (int_f)H5D_MPIO_CHUNK_MIXED;
    h5d_flags[24] = (int_f)H5D_MPIO_CONTIGUOUS_COLLECTIVE;
    h5d_flags[25] = (int_f)H5D_VDS_ERROR;
    h5d_flags[26] = (int_f)H5D_VDS_FIRST_MISSING;
    h5d_flags[27] = (int_f)H5D_VDS_LAST_AVAILABLE;
    h5d_flags[28] = (int_f)H5D_VIRTUAL;

/*
 *  H5E flags
 */
    h5e_hid_flags[0] = (hid_t_f)H5E_DEFAULT;

    h5e_flags[0] = (int_f)H5E_MAJOR;
    h5e_flags[1] = (int_f)H5E_MINOR;
    h5e_flags[2] = (int_f)H5E_WALK_UPWARD;
    h5e_flags[3] = (int_f)H5E_WALK_DOWNWARD;

/*
 *  H5F flags
 *
 *  Note that H5F_ACC_DEBUG is deprecated (nonfunctional) but retained
 *  for backward compatibility since it's in the public API.
 */
    h5f_flags[0] = (int_f)H5F_ACC_RDWR;
    h5f_flags[1] = (int_f)H5F_ACC_RDONLY;
    h5f_flags[2] = (int_f)H5F_ACC_TRUNC;
    h5f_flags[3] = (int_f)H5F_ACC_EXCL;
#ifndef H5_NO_DEPRECATED_SYMBOLS
    h5f_flags[4] = (int_f)H5F_ACC_DEBUG;
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    h5f_flags[5] = (int_f)H5F_SCOPE_LOCAL;
    h5f_flags[6] = (int_f)H5F_SCOPE_GLOBAL;
    h5f_flags[7] = (int_f)H5F_CLOSE_DEFAULT;
    h5f_flags[8] = (int_f)H5F_CLOSE_WEAK;
    h5f_flags[9] = (int_f)H5F_CLOSE_SEMI;
    h5f_flags[10] = (int_f)H5F_CLOSE_STRONG;
    h5f_flags[11] = (int_f)H5F_OBJ_FILE;
    h5f_flags[12] = (int_f)H5F_OBJ_DATASET;
    h5f_flags[13] = (int_f)H5F_OBJ_GROUP;
    h5f_flags[14] = (int_f)H5F_OBJ_DATATYPE;
    h5f_flags[15] = (int_f)H5F_OBJ_ALL;
    h5f_flags[16] = (int_f)H5F_LIBVER_EARLIEST;
    h5f_flags[17] = (int_f)H5F_LIBVER_LATEST;
    h5f_flags[18] = (int_f)H5F_UNLIMITED;

/*
 *  H5FD flags
 */
    h5fd_flags[0] = (int_f)H5FD_MPIO_INDEPENDENT;
    h5fd_flags[1] = (int_f)H5FD_MPIO_COLLECTIVE;
    h5fd_flags[2] = (int_f)H5FD_MEM_NOLIST;
    h5fd_flags[3] = (int_f)H5FD_MEM_DEFAULT;
    h5fd_flags[4] = (int_f)H5FD_MEM_SUPER;
    h5fd_flags[5] = (int_f)H5FD_MEM_BTREE;
    h5fd_flags[6] = (int_f)H5FD_MEM_DRAW;
    h5fd_flags[7] = (int_f)H5FD_MEM_GHEAP;
    h5fd_flags[8] = (int_f)H5FD_MEM_LHEAP;
    h5fd_flags[9] = (int_f)H5FD_MEM_OHDR;
    h5fd_flags[10] = (int_f)H5FD_MEM_NTYPES;

/*
 *  H5FD flags of type hid_t
 */
    h5fd_hid_flags[0] = (hid_t_f)H5FD_CORE;
    h5fd_hid_flags[1] = (hid_t_f)H5FD_FAMILY;
    h5fd_hid_flags[2] = (hid_t_f)H5FD_LOG;
    h5fd_hid_flags[3] = (hid_t_f)H5FD_MPIO;
    h5fd_hid_flags[4] = (hid_t_f)H5FD_MULTI;
    h5fd_hid_flags[5] = (hid_t_f)H5FD_SEC2;
    h5fd_hid_flags[6] = (hid_t_f)H5FD_STDIO;

/*
 *  H5G flags
 */
    h5g_flags[0] = (int_f)H5O_TYPE_UNKNOWN; /* H5G_UNKNOWN is deprecated */
    h5g_flags[1] = (int_f)H5O_TYPE_GROUP; /* H5G_GROUP is deprecated  */
    h5g_flags[2] = (int_f)H5O_TYPE_DATASET; /* H5G_DATASET is deprecated  */
    h5g_flags[3] = (int_f)H5O_TYPE_NAMED_DATATYPE; /* H5G_TYPE is deprecated  */
    h5g_flags[4] = (int_f)H5L_SAME_LOC;
    h5g_flags[5] = (int_f)H5L_TYPE_ERROR;
    h5g_flags[6] = (int_f)H5L_TYPE_HARD;
    h5g_flags[7] = (int_f)H5L_TYPE_SOFT;

    h5g_flags[8] = (int_f)H5G_STORAGE_TYPE_UNKNOWN;
    h5g_flags[9] = (int_f)H5G_STORAGE_TYPE_SYMBOL_TABLE;
    h5g_flags[10] = (int_f)H5G_STORAGE_TYPE_COMPACT;
    h5g_flags[11] = (int_f)H5G_STORAGE_TYPE_DENSE;

/*
 *  H5I flags
 */
    h5i_flags[0] = (int_f)H5I_FILE;
    h5i_flags[1] = (int_f)H5I_GROUP;
    h5i_flags[2] = (int_f)H5I_DATATYPE;
    h5i_flags[3] = (int_f)H5I_DATASPACE;
    h5i_flags[4] = (int_f)H5I_DATASET;
    h5i_flags[5] = (int_f)H5I_ATTR;
    h5i_flags[6] = (int_f)H5I_BADID;
/*
 *  H5L flags
 */
    h5l_flags[0] = (int_f)H5L_TYPE_ERROR;
    h5l_flags[1] = (int_f)H5L_TYPE_HARD;
    h5l_flags[2] = (int_f)H5L_TYPE_SOFT;
    h5l_flags[3] = (int_f)H5L_TYPE_EXTERNAL;
    h5l_flags[4] = (int_f)H5L_SAME_LOC; /* Macro to indicate operation occurs on same location */
    h5l_flags[5] = (int_f)H5L_LINK_CLASS_T_VERS; /* Current version of the H5L_class_t struct */

/*
 *  H5O flags
 */

/* Flags for object copy (H5Ocopy) */
    h5o_flags[0] = (int_f)H5O_COPY_SHALLOW_HIERARCHY_FLAG; /* Copy only immediate members */
    h5o_flags[1] = (int_f)H5O_COPY_EXPAND_SOFT_LINK_FLAG; /* Expand soft links into new objects */
    h5o_flags[2] = (int_f)H5O_COPY_EXPAND_EXT_LINK_FLAG; /* Expand external links into new objects */
    h5o_flags[3] = (int_f)H5O_COPY_EXPAND_REFERENCE_FLAG; /* Copy objects that are pointed by references */
    h5o_flags[4] = (int_f)H5O_COPY_WITHOUT_ATTR_FLAG; /* Copy object without copying attributes */
    h5o_flags[5] = (int_f)H5O_COPY_PRESERVE_NULL_FLAG; /* Copy NULL messages (empty space) */
    h5o_flags[6] = (int_f)H5O_COPY_ALL; /* All object copying flags (for internal checking) */

/* Flags for shared message indexes.
 *  Pass these flags in using the mesg_type_flags parameter in
 * H5P_set_shared_mesg_index.
 * (Developers: These flags correspond to object header message type IDs,
 *  but we need to assign each kind of message to a different bit so that
 *  one index can hold multiple types.)
 */
      h5o_flags[7] = (int_f)H5O_SHMESG_NONE_FLAG;  /* No shared messages */
      h5o_flags[8] = (int_f)H5O_SHMESG_SDSPACE_FLAG; /* Simple Dataspace Message.  */
      h5o_flags[9] = (int_f)H5O_SHMESG_DTYPE_FLAG; /* Datatype Message.  */
      h5o_flags[10] = (int_f)H5O_SHMESG_FILL_FLAG; /* Fill Value Message. */
      h5o_flags[11] = (int_f)H5O_SHMESG_PLINE_FLAG; /* Filter pipeline message.  */
      h5o_flags[12] = (int_f)H5O_SHMESG_ATTR_FLAG; /* Attribute Message.  */
      h5o_flags[13] = (int_f)H5O_SHMESG_ALL_FLAG;

/* Object header status flag definitions */
      h5o_flags[14] = (int_f)H5O_HDR_CHUNK0_SIZE; /* 2-bit field indicating # of bytes to store the size of chunk 0's data */
      h5o_flags[15] = (int_f)H5O_HDR_ATTR_CRT_ORDER_TRACKED; /* Attribute creation order is tracked */
      h5o_flags[16] = (int_f)H5O_HDR_ATTR_CRT_ORDER_INDEXED; /* Attribute creation order has index */
      h5o_flags[17] = (int_f)H5O_HDR_ATTR_STORE_PHASE_CHANGE; /* Non-default attribute storage phase change values stored */
      h5o_flags[18] = (int_f)H5O_HDR_STORE_TIMES; /* Store access, modification, change & birth times for object */
      h5o_flags[19] = (int_f)H5O_HDR_ALL_FLAGS;

/* Maximum shared message values.  Number of indexes is 8 to allow room to add
 *  new types of messages.
 */
      h5o_flags[20] = (int_f)H5O_SHMESG_MAX_NINDEXES;
      h5o_flags[21] = (int_f)H5O_SHMESG_MAX_LIST_SIZE;

/* Types of objects in file */
      h5o_flags[22] = (int_f)H5O_TYPE_UNKNOWN;	      /* Unknown object type */
      h5o_flags[23] = (int_f)H5O_TYPE_GROUP;          /* Object is a group */
      h5o_flags[24] = (int_f)H5O_TYPE_DATASET;        /* Object is a dataset */
      h5o_flags[25] = (int_f)H5O_TYPE_NAMED_DATATYPE; /* Object is a named data type */
      h5o_flags[26] = (int_f)H5O_TYPE_NTYPES;         /* Number of different object types */

/* Flags for H5Oget_info.
 * These flags determine which fields will be filled in in the H5O_info_t
 * struct. 
 */
      h5o_flags[27] = (int_f)H5O_INFO_ALL; /* (H5O_INFO_BASIC|H5O_INFO_TIME|H5O_INFO_NUM_ATTRS|H5O_INFO_HDR|H5O_INFO_META_SIZE) */
      h5o_flags[28] = (int_f)H5O_INFO_BASIC; /* Fill in the fileno, addr, type, and rc fields */
      h5o_flags[29] = (int_f)H5O_INFO_TIME;  /* Fill in the atime, mtime, ctime, and btime fields */
      h5o_flags[30] = (int_f)H5O_INFO_NUM_ATTRS; /* Fill in the num_attrs field */
      h5o_flags[31] = (int_f)H5O_INFO_HDR;       /* Fill in the hdr field */
      h5o_flags[32] = (int_f)H5O_INFO_META_SIZE; /* Fill in the meta_size field */
/*
 *  H5P flags
 */
      h5p_flags[0] = (hid_t_f)H5P_FILE_CREATE;
      h5p_flags[1] = (hid_t_f)H5P_FILE_ACCESS;
      h5p_flags[2] = (hid_t_f)H5P_DATASET_CREATE;
      h5p_flags[3] = (hid_t_f)H5P_DATASET_XFER;
      h5p_flags[4] = (hid_t_f)H5P_FILE_MOUNT;
      h5p_flags[5] = (hid_t_f)H5P_DEFAULT;
      h5p_flags[6] = (hid_t_f)H5P_ROOT;
      h5p_flags[7] = (hid_t_f)H5P_OBJECT_CREATE;
      h5p_flags[8] = (hid_t_f)H5P_DATASET_ACCESS;
      h5p_flags[9] = (hid_t_f)H5P_GROUP_CREATE;
      h5p_flags[10] = (hid_t_f)H5P_GROUP_ACCESS;
      h5p_flags[11] = (hid_t_f)H5P_DATATYPE_CREATE;
      h5p_flags[12] = (hid_t_f)H5P_DATATYPE_ACCESS;
      h5p_flags[13] = (hid_t_f)H5P_STRING_CREATE;
      h5p_flags[14] = (hid_t_f)H5P_ATTRIBUTE_CREATE;
      h5p_flags[15] = (hid_t_f)H5P_OBJECT_COPY;
      h5p_flags[16] = (hid_t_f)H5P_LINK_CREATE;
      h5p_flags[17] = (hid_t_f)H5P_LINK_ACCESS;


/*
 *  H5P integer flags
 */
      h5p_flags_int[0] = (int_f)H5P_CRT_ORDER_INDEXED;
      h5p_flags_int[1] = (int_f)H5P_CRT_ORDER_TRACKED;

/*
 *  H5R flags
 */

      h5r_flags[0] = (int_f)H5R_OBJECT;
      h5r_flags[1] = (int_f)H5R_DATASET_REGION;

/*
 *  H5S flags
 */
      
      h5s_hid_flags[0] = (hid_t_f)H5S_ALL;

      h5s_hsize_flags[0] = (hsize_t_f)H5S_UNLIMITED;

      h5s_flags[0] = (int_f)H5S_SCALAR;
      h5s_flags[1] = (int_f)H5S_SIMPLE;
      h5s_flags[2] = (int_f)H5S_NULL;
      h5s_flags[3] = (int_f)H5S_SELECT_SET;
      h5s_flags[4] = (int_f)H5S_SELECT_OR;

      h5s_flags[5] = (int_f)H5S_SELECT_NOOP;
      h5s_flags[6] = (int_f)H5S_SELECT_AND;
      h5s_flags[7] = (int_f)H5S_SELECT_XOR;
      h5s_flags[8] = (int_f)H5S_SELECT_NOTB;
      h5s_flags[9] = (int_f)H5S_SELECT_NOTA;

      h5s_flags[10] = (int_f)H5S_SELECT_APPEND;
      h5s_flags[11] = (int_f)H5S_SELECT_PREPEND;
      h5s_flags[12] = (int_f)H5S_SELECT_INVALID;
      h5s_flags[13] = (int_f)H5S_SEL_ERROR;
      h5s_flags[14] = (int_f)H5S_SEL_NONE;

      h5s_flags[15] = (int_f)H5S_SEL_POINTS;
      h5s_flags[16] = (int_f)H5S_SEL_HYPERSLABS;
      h5s_flags[17] = (int_f)H5S_SEL_ALL;
/*
 *  H5T flags
 */
      h5t_flags[0] = (int_f)H5T_NO_CLASS;
      h5t_flags[1] = (int_f)H5T_INTEGER;
      h5t_flags[2] = (int_f)H5T_FLOAT;
      h5t_flags[3] = (int_f)H5T_TIME;
      h5t_flags[4] = (int_f)H5T_STRING;
      h5t_flags[5] = (int_f)H5T_BITFIELD;
      h5t_flags[6] = (int_f)H5T_OPAQUE;
      h5t_flags[7] = (int_f)H5T_COMPOUND;
      h5t_flags[8] = (int_f)H5T_REFERENCE;
      h5t_flags[9] = (int_f)H5T_ENUM;
      h5t_flags[10] = (int_f)H5T_ORDER_LE;
      h5t_flags[11] = (int_f)H5T_ORDER_BE;
      h5t_flags[12] = (int_f)H5T_ORDER_MIXED;
      h5t_flags[13] = (int_f)H5T_ORDER_VAX;
      h5t_flags[14] = (int_f)H5T_ORDER_NONE;
      h5t_flags[15] = (int_f)H5T_PAD_ZERO;
      h5t_flags[16] = (int_f)H5T_PAD_ONE;
      h5t_flags[17] = (int_f)H5T_PAD_BACKGROUND;
      h5t_flags[18] = (int_f)H5T_PAD_ERROR;
      h5t_flags[19] = (int_f)H5T_SGN_NONE;
      h5t_flags[20] = (int_f)H5T_SGN_2;
      h5t_flags[21] = (int_f)H5T_SGN_ERROR;
      h5t_flags[22] = (int_f)H5T_NORM_IMPLIED;
      h5t_flags[23] = (int_f)H5T_NORM_MSBSET;
      h5t_flags[24] = (int_f)H5T_NORM_NONE;
      h5t_flags[25] = (int_f)H5T_CSET_ASCII;
      h5t_flags[26] = (int_f)H5T_CSET_UTF8;
      h5t_flags[27] = (int_f)H5T_STR_NULLTERM;
      h5t_flags[28] = (int_f)H5T_STR_NULLPAD;
      h5t_flags[29] = (int_f)H5T_STR_SPACEPAD;
      h5t_flags[30] = (int_f)H5T_STR_ERROR;
      h5t_flags[31] = (int_f)H5T_VLEN;
      h5t_flags[32] = (int_f)H5T_ARRAY;
      h5t_flags[33] = (int_f)H5T_DIR_ASCEND;
      h5t_flags[34] = (int_f)H5T_DIR_DESCEND;
/*
 *  H5Z flags
 */
      h5z_flags[0] = (int_f)H5Z_FILTER_ERROR;
      h5z_flags[1] = (int_f)H5Z_FILTER_NONE;
      h5z_flags[2] = (int_f)H5Z_FILTER_DEFLATE;
      h5z_flags[3] = (int_f)H5Z_FILTER_SHUFFLE;
      h5z_flags[4] = (int_f)H5Z_FILTER_FLETCHER32;
      h5z_flags[5] = (int_f)H5Z_ERROR_EDC;
      h5z_flags[6] = (int_f)H5Z_DISABLE_EDC;
      h5z_flags[7] = (int_f)H5Z_ENABLE_EDC;
      h5z_flags[8] = (int_f)H5Z_NO_EDC;
      h5z_flags[9] = (int_f)H5Z_FILTER_SZIP;
      h5z_flags[10] = (int_f)H5Z_FLAG_OPTIONAL;
      h5z_flags[11] = (int_f)H5Z_FILTER_CONFIG_ENCODE_ENABLED;
      h5z_flags[12] = (int_f)H5Z_FILTER_CONFIG_DECODE_ENABLED;
      h5z_flags[13] = (int_f)H5Z_FILTER_ALL;
      h5z_flags[14] = (int_f)H5Z_FILTER_NBIT;
      h5z_flags[15] = (int_f)H5Z_FILTER_SCALEOFFSET;
      h5z_flags[16] = (int_f)H5Z_SO_FLOAT_DSCALE;
      h5z_flags[17] = (int_f)H5Z_SO_FLOAT_ESCALE;
      h5z_flags[18] = (int_f)H5Z_SO_INT;
      h5z_flags[19] = (int_f)H5Z_SO_INT_MINBITS_DEFAULT;

/*
 *  H5 Generic flags introduced in version 1.8
 */

      /* H5_index_t enum struct */

      h5_generic_flags[0] = (int_f)H5_INDEX_UNKNOWN;   /* Unknown index type			*/
      h5_generic_flags[1] = (int_f)H5_INDEX_NAME;      /* Index on names 			*/
      h5_generic_flags[2] = (int_f)H5_INDEX_CRT_ORDER; /* Index on creation order 		*/
      h5_generic_flags[3] = (int_f)H5_INDEX_N;         /* Index on creation order 		*/


      /* H5_iter_order_t enum struct */

      h5_generic_flags[4] = (int_f)H5_ITER_UNKNOWN;       /* Unknown order */
      h5_generic_flags[5] = (int_f)H5_ITER_INC;           /* Increasing order */
      h5_generic_flags[6] = (int_f)H5_ITER_DEC;           /* Decreasing order */
      h5_generic_flags[7] = (int_f)H5_ITER_NATIVE;        /* No particular order, whatever is fastest */
      h5_generic_flags[8] = (int_f)H5_ITER_N;		  /* Number of iteration orders */

      h5_haddr_generic_flags[0] = (haddr_t_f)HADDR_UNDEF; /* undefined address */

    ret_value = 0;
    return ret_value;
}

int_f
h5init1_flags_c(int_f *h5lib_flags)
/******/
{
    int ret_value = -1;
    unsigned prm_1 = H5_SZIP_EC_OPTION_MASK;
    unsigned prm_2 = H5_SZIP_NN_OPTION_MASK;
    h5lib_flags[0] = (int_f)prm_1;
    h5lib_flags[1] = (int_f)prm_2;
    ret_value = 0;
    return ret_value;
}

/****if* H5_f/h5open_c
 * NAME
 *  h5open_c
 * PURPOSE
 *  Calls H5open call to initialize C HDF5 library
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Friday, November 17, 2000
 *
 * SOURCE
 */
int_f
h5open_c(void)
/******/
{
    int ret_value = -1;

    if (H5open() < 0) return ret_value;
    ret_value = 0;
    return ret_value;
}
/****if* H5_f/h5close_c
 * NAME
 *  h5close_c
 * PURPOSE
 *  Calls H5close call to close C HDF5 library
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 * SOURCE
 */
int_f
h5close_c(void)
/******/
{
    int ret_value = -1;

    if (H5close() < 0) return ret_value;
    ret_value = 0;
    return ret_value;
}

/****if* H5_f/h5get_libversion_c
 * NAME
 *  h5get_libversion_c
 * PURPOSE
 *  Calls H5get_libversion function
 *		      to retrieve library version info.
 * INPUTS
 *
 *  None
 * OUTPUTS
 *
 *  majnum - the major version of the library
 *  minnum - the minor version of the library
 *  relnum - the release version of the library
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, September 24, 2002
 * SOURCE
 *
 */
int_f
h5get_libversion_c(int_f *majnum, int_f *minnum, int_f *relnum)
/******/
{

    int ret_value = -1;
    unsigned c_majnum, c_minnum, c_relnum;

    if (H5get_libversion(&c_majnum, &c_minnum, &c_relnum) < 0) return ret_value;

    *majnum = (int_f)c_majnum;
    *minnum = (int_f)c_minnum;
    *relnum = (int_f)c_relnum;
    ret_value = 0;
    return ret_value;
}

/****if* H5_f/h5check_version_c
 * NAME
 *  h5check_version_c
 * PURPOSE
 *  Calls H5check_version function
 *		      to verify library version info.
 * INPUTS
 *
 *  majnum - the major version of the library
 *  minnum - the minor version of the library
 *  relnum - the release version of the library
 * OUTPUTS
 *
 *  None
 * RETURNS
 *  0 on success, aborts on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, September 24, 2002
 * SOURCE
 */
int_f
h5check_version_c(int_f *majnum, int_f *minnum, int_f *relnum)
/******/
{
    int ret_value = -1;
    unsigned c_majnum, c_minnum, c_relnum;

    c_majnum = (unsigned) *majnum;
    c_minnum = (unsigned) *minnum;
    c_relnum = (unsigned) *relnum;

    H5check_version(c_majnum, c_minnum, c_relnum);

    ret_value = 0;
    return ret_value;
}

/****if* H5_f/h5garbage_collect_c
 * NAME
 *  h5garbage_collect_c
 * PURPOSE
 *  Calls H5garbage_collect to collect on all free-lists of all types
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, September 24, 2002
 * SOURCE
 */
int_f
h5garbage_collect_c(void)
/******/
{
    int ret_value = -1;

    if (H5garbage_collect() < 0) return ret_value;
    ret_value = 0;
    return ret_value;
}

/****if* H5_f/h5dont_atexit_c
 * NAME
 *  h5dont_atexit_c
 * PURPOSE
 *  Calls H5dont_atexit not to install atexit cleanup routine
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Tuesday, September 24, 2002
 * SOURCE
 */
int_f
h5dont_atexit_c(void)
/******/
{
    int ret_value = -1;

    if (H5dont_atexit() < 0) return ret_value;
    ret_value = 0;
    return ret_value;
}

