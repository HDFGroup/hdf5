#ifndef _H5f90_H
#define _H5f90_H 

#include <hdf5.h>
#include "H5f90i.h"
#include "H5f90proto.h"


/* Constants from the H5Ff.c and H5Fff.f90 files */


#define H5F_ACC_RDWR_F      1
#define H5F_ACC_RDONLY_F    2
#define H5F_ACC_TRUNC_F     3
#define H5F_ACC_EXCL_F      4
#define H5F_ACC_DEBUG_F     5
#define H5P_DEFAULT_F       6     /* Can Fortran program use combination 
                                     of those flags? */
#define H5F_SCOPE_LOCAL_F   0
#define H5F_SCOPE_GLOBAL_F  1

/* Constants used in the H5Gf.c and H5Gff.f90 files */

#define OBJECT_NAMELEN_DEFAULT_F -1
#define H5G_LINK_F                0
#define H5G_GROUP_F               1 
#define H5G_DATASET_F             2 
#define H5G_TYPE_F                3 
 

/* Constants used in H5Df.c and H5Dff.f90 files */

#define H5S_ALL_F    -2

/* Constants used in H5Rff.f90 and H5Rf.c files */

#define REF_OBJ_BUF_LEN_F    2
#define REF_REG_BUF_LEN_F    3 

/* Constants used in  H5Sf.c and H5Sff.f90 files */

#define H5S_NO_CLASS_F -1 
#define H5S_SCALAR_F    0
#define H5S_SIMPLE_F    1
#define H5S_SELECT_SET_F 0
#define H5S_SELECT_OR_F  1
 
/* Constants ised in H5Tf.c and H5Tff.f90 files */


#define  H5T_NO_CLASS_F  -1
#define  H5T_INTEGER_F    0 
#define  H5T_FLOAT_F      1
#define  H5T_TIME_F       2
#define  H5T_STRING_F     3
#define  H5T_BITFIELD_F   4
#define  H5T_OPAQUE_F     5
#define  H5T_COMPOUND_F   6
#define  H5T_REFERENCE_F  7
#define  H5T_ENUM_F       8

#define  H5T_ORDER_LE_F   0
#define  H5T_ORDER_BE_F   1
#define  H5T_ORDER_VAX_F  2 


/* Constants used in H5Pf.c and H5Pff.f90 files */


#define   H5P_NO_CLASS_F       -1 
#define   H5P_FILE_CREATE_F     0
#define   H5P_FILE_ACCESS_F     1
#define   H5P_DATASET_CREATE_F  2
#define   H5P_DATASET_XFER_F    3
#define   H5P_MOUNT_F           4

/* Constants used in H5Pf_parallel.c and H5Pff_parallel.f90 files */
#define H5D_XFER_INDEPENDENT_F   0
#define H5D_XFER_COLLECTIVE_F    1 
#define H5D_XFER_DFLT_F          2 

#endif /* _H5f90_H */
