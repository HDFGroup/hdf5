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
