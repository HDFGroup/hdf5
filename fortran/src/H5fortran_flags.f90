      MODULE H5FORTRAN_FLAGS

!H5F file interface related flags
       INTEGER, PARAMETER :: H5F_ACC_RDWR_F   = 1 , &
                              H5F_ACC_RDONLY_F = 2,  &
                              H5F_ACC_TRUNC_F  = 3,  &
                              H5F_ACC_EXCL_F   = 4,  &
                              H5F_ACC_DEBUG_F  = 5,  &
                              H5F_SCOPE_LOCAL_F  = 0, &
                              H5F_SCOPE_GLOBAL_F = 1

!H5G group interface related flags
       INTEGER, PARAMETER :: H5G_UNKNOWN_F = -1
       INTEGER, PARAMETER :: H5G_LINK_F = 0
       INTEGER, PARAMETER :: H5G_GROUP_F =1 
       INTEGER, PARAMETER :: H5G_DATASET_F =2 
       INTEGER, PARAMETER :: H5G_TYPE_F =3 

!H5P Property interface related flags

       INTEGER, PARAMETER ::  H5P_FILE_CREATE_F = 0
       INTEGER, PARAMETER ::  H5P_FILE_ACCESS_F = 1
       INTEGER, PARAMETER ::  H5P_DATASET_CREATE_F = 2
       INTEGER, PARAMETER ::  H5P_DATASET_XFER_F = 3
       INTEGER, PARAMETER ::  H5P_MOUNT_F = 4
       INTEGER, PARAMETER ::  H5P_DEFAULT_F = 6

!H5R Reference interface related flags
       INTEGER, PARAMETER :: H5R_OBJECT_F = 0 
       INTEGER, PARAMETER :: H5R_DATASET_REGION_F = -2 

!H5S Dataspace interface related flags
       INTEGER, PARAMETER :: H5S_SCALAR_F = 0
       INTEGER, PARAMETER :: H5S_SIMPLE_F = 1
       INTEGER, PARAMETER :: H5S_SELECT_SET_F = 0
       INTEGER, PARAMETER :: H5S_SELECT_OR_F =1 
       INTEGER, PARAMETER :: H5S_UNLIMITED_F = -1 
       INTEGER, PARAMETER :: H5S_ALL_F = -2 
!USED IN PROERTY INTERFACE
       INTEGER, PARAMETER :: H5D_COMPACT_F = 0

       INTEGER, PARAMETER :: H5D_CONTIGUOUS_F = 1
       INTEGER, PARAMETER :: H5D_CHUNKED_F = 2
       INTEGER, PARAMETER :: H5D_XFER_INDEPENDENT_F = 0
       INTEGER, PARAMETER :: H5D_XFER_COLLECTIVE_F = 1 
       INTEGER, PARAMETER :: H5D_XFER_DFLT_F = 2 

!H5T Data type interface related flags
        INTEGER, PARAMETER ::  H5T_NO_CLASS_F = -1
        INTEGER, PARAMETER ::  H5T_INTEGER_F = 0 
        INTEGER, PARAMETER ::  H5T_FLOAT_F  = 1
        INTEGER, PARAMETER ::  H5T_TIME_F = 2
        INTEGER, PARAMETER ::  H5T_STRING_F = 3
        INTEGER, PARAMETER ::  H5T_BITFIELD_F = 4
        INTEGER, PARAMETER ::  H5T_OPAQUE_F = 5
        INTEGER, PARAMETER ::  H5T_COMPOUND_F = 6
        INTEGER, PARAMETER ::  H5T_REFERENCE_F = 7
        INTEGER, PARAMETER ::  H5T_ENUM_F = 8
        INTEGER, PARAMETER ::  H5T_ORDER_LE_F = 0
        INTEGER, PARAMETER ::  H5T_ORDER_BE_F = 1
        INTEGER, PARAMETER ::  H5T_ORDER_VAX_F = 2 

        INTEGER, PARAMETER ::  H5T_PAD_ZERO_F = 0
        INTEGER, PARAMETER ::  H5T_PAD_ONE_F = 1
        INTEGER, PARAMETER ::  H5T_PAD_BACKGROUND_F = 2
        INTEGER, PARAMETER ::  H5T_PAD_ERROR_F      = -1 
        INTEGER, PARAMETER ::  H5T_PAD_NPAD_F      = 3 

        !Unsigned integer type
        INTEGER, PARAMETER ::  H5T_SGN_NONE_F      = 0 
        !Two's complement signed integer type 
        INTEGER, PARAMETER ::  H5T_SGN_2_F      = 1 

        INTEGER, PARAMETER ::  H5T_SGN_ERROR_F      = -1 
        !MSB of mantissa is not stored, always 1
         INTEGER, PARAMETER ::  H5T_NORM_IMPLIED_F      = 0 
        !MSB of mantissa is always 1 
         INTEGER, PARAMETER ::  H5T_NORM_MSBSET_F      = 1 
         !Mantissa is not normalized
         INTEGER, PARAMETER ::  H5T_NORM_NONE_F      = 2 
         !Character set is US ASCII 
         INTEGER, PARAMETER ::  H5T_CSET_ASCII_F      = 0
         !Pad with zeros (as C does) 
         INTEGER, PARAMETER ::  H5T_STR_NULL_F = 0
         !Pad with spaces (as FORTRAN does) 
         INTEGER, PARAMETER ::  H5T_STR_SPACE_F = 1

!H5P interface related fortran flags:
         !identifier of the low-level file driver. 
         INTEGER, PARAMETER ::  H5F_LOW_STDIO_F = 0
         INTEGER, PARAMETER ::  H5F_LOW_SEC2_F = 1
         INTEGER, PARAMETER ::  H5F_LOW_MPIO_F = 2
         INTEGER, PARAMETER ::  H5F_LOW_CORE_F = 3
         INTEGER, PARAMETER ::  H5F_LOW_SPLIT_F = 4
         INTEGER, PARAMETER ::  H5F_LOW_FAMILY_F = 5
         
!H5I interface related fortran flags:
         INTEGER, PARAMETER ::  H5I_FILE_F = 1
         INTEGER, PARAMETER ::  H5I_GROUP_F = 12
         INTEGER, PARAMETER ::  H5I_DATATYPE_F = 13
         INTEGER, PARAMETER ::  H5I_DATASPACE_F = 14
         INTEGER, PARAMETER ::  H5I_DATASET_F = 15
         INTEGER, PARAMETER ::  H5I_ATTR_F = 16
         INTEGER, PARAMETER ::  H5I_BADID_F = -1

!H5E interface related fortran flags: 
         !Turn on automatic printing of errors
         INTEGER, PARAMETER :: PRINTON = 1
         
         !Turn off automatic printing of errors  
         INTEGER, PARAMETER :: PRINTOFF = 0

         !Error flags same as H5E_major_t
          
         INTEGER, PARAMETER ::  H5E_NONE_MAJOR_F = 0  !special zero, no error
         INTEGER, PARAMETER ::  H5E_ARGS_F = 1        !invalid arguments to routine
         INTEGER, PARAMETER ::  H5E_RESOURCE_F = 2    !resource unavailable  
         INTEGER, PARAMETER ::  H5E_INTERNAL_F = 3    !Internal error (too specific to
                                                      !document in detail)
         INTEGER, PARAMETER ::  H5E_FILE_F = 4   !file Accessability
         INTEGER, PARAMETER ::  H5E_IO_F = 5     !Low-level I/O
         INTEGER, PARAMETER ::  H5E_FUNC_F = 6   !function Entry/Exit
         INTEGER, PARAMETER ::  H5E_ATOM_F = 7   !object Atom 
         INTEGER, PARAMETER ::  H5E_CACHE_F = 8  !object Cache
         INTEGER, PARAMETER ::  H5E_BTREE_F = 9  !B-Tree Node
         INTEGER, PARAMETER ::  H5E_SYM_F = 10   !symbol Table
         INTEGER, PARAMETER ::  H5E_HEAP_F = 11  !Heap
         INTEGER, PARAMETER ::  H5E_OHDR_F = 12  !object Header
         INTEGER, PARAMETER ::  H5E_DATATYPE_F = 13  !Datatype
         INTEGER, PARAMETER ::  H5E_DATASPACE_F = 14 ! Dataspace 
         INTEGER, PARAMETER ::  H5E_DATASET_F = 15   !Dataset  
         INTEGER, PARAMETER ::  H5E_STORAGE_F = 16   !data storage 
         INTEGER, PARAMETER ::  H5E_PLIST_F = 17  !Property lists 
         INTEGER, PARAMETER ::  H5E_ATTR_F = 18   !Attribute
         INTEGER, PARAMETER ::  H5E_PLINE_F = 19  !Data filters
         INTEGER, PARAMETER ::  H5E_EFL_F = 20  !External file list
         INTEGER, PARAMETER ::  H5E_RAGGED_F  = 21  !Ragged arrays
         INTEGER, PARAMETER ::  H5E_REFERENCE_F = 22 !References 

         
    END MODULE H5FORTRAN_FLAGS
