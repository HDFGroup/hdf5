! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!
! This file contains FORTRAN90 interfaces for H5DS functions
!

MODULE h5ds

  USE h5fortran_types
  USE hdf5

CONTAINS


!-------------------------------------------------------------------------
! Function: H5DSset_scale_f
!
! Purpose: Convert dataset dsid to a dimension scale, with optional name, dimname. 
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 17, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSset_scale_f( dsid, errcode, dimname)

    IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsset_scale_f
!DEC$endif
!

    INTEGER(hid_t),   INTENT(in) :: dsid               ! The dataset to be made a Dimension Scale
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: dimname  ! The dimension name
    INTEGER :: errcode                                 ! Error code

    INTEGER(SIZE_T) :: dimname_len                     ! length of dimname (if present)

    INTERFACE
       INTEGER FUNCTION H5DSset_scale_c(dsid, dimname, dimname_len )

         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSSET_SCALE_C'::h5dsset_scale_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dimname  
         INTEGER(hid_t),   INTENT(in) :: dsid     ! The dataset to be made a Dimension Scale
         CHARACTER(LEN=*), INTENT(in) :: dimname  ! The dimension name
         INTEGER(SIZE_T),  INTENT(in) :: dimname_len
       END FUNCTION H5DSset_scale_c
    END INTERFACE

    IF(PRESENT(dimname))THEN
       dimname_len = LEN(dimname)
       errcode = H5DSset_scale_c(dsid, dimname, dimname_len )
    ELSE
       errcode = H5DSset_scale_c(dsid, " ", INT(0,SIZE_T) )
    ENDIF

  END SUBROUTINE H5DSset_scale_f

!-------------------------------------------------------------------------
! Function: H5DSattach_scale_f
!
! Purpose: Attach dimension scale dsid to dimension idx of dataset did.
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 17, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSattach_scale_f( did, dsid, idx, errcode)

    IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsattach_scale_f
!DEC$endif
!
    INTEGER(hid_t), INTENT(in) :: did     ! the dataset
    INTEGER(hid_t), INTENT(in) :: dsid    ! the scale to be attached 
    INTEGER       , INTENT(in) :: idx     ! the dimension of did that dsid is associated with.
    INTEGER                    :: errcode ! error code
    INTEGER                    :: c_idx
    
    INTERFACE
       INTEGER FUNCTION  H5DSattach_scale_c(did, dsid, idx )
         
         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSATTACH_SCALE_C':: h5dsattach_scale_c
         !DEC$ENDIF
         INTEGER(hid_t), INTENT(in) :: did     ! the dataset
         INTEGER(hid_t), INTENT(in) :: dsid    ! the scale to be attached 
         INTEGER       , INTENT(in) :: idx     ! the dimension of did that dsid is associated with.
       END FUNCTION H5DSattach_scale_c
    END INTERFACE

    c_idx = idx -1 ! account for C-dimensions starting at 0 
    
    errcode = H5DSattach_scale_c( did, dsid, c_idx)
    
  END SUBROUTINE H5DSattach_scale_f
  
!-------------------------------------------------------------------------
! Function: H5DSdetach_scale_f
!
! Purpose: Detach dimension scale dsid from the dimension idx of Dataset did.
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 17, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSdetach_scale_f( did, dsid, idx, errcode)
    
    IMPLICIT NONE
    
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsdetach_scale_f
!DEC$endif
!
    INTEGER(hid_t), INTENT(in) :: did     ! the dataset
    INTEGER(hid_t), INTENT(in) :: dsid    ! the scale to be detached 
    INTEGER       , INTENT(in) :: idx     ! the dimension of did to detach
    INTEGER                    :: errcode ! error code
    INTEGER                    :: c_idx
    
    INTERFACE
       INTEGER FUNCTION  H5DSdetach_scale_c(did, dsid, idx )
         
         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSDETACH_SCALE_C':: h5dsdetach_scale_c
         !DEC$ENDIF
         INTEGER(hid_t), INTENT(in) :: did     ! the dataset
         INTEGER(hid_t), INTENT(in) :: dsid    ! the scale to be detached 
         INTEGER       , INTENT(in) :: idx     ! the dimension of did to detach
       END FUNCTION H5DSdetach_scale_c
    END INTERFACE

    c_idx = idx - 1 ! account for C-dimensions starting at 0 

    errcode = H5DSdetach_scale_c( did, dsid, c_idx)
    
  END SUBROUTINE H5DSdetach_scale_f


!-------------------------------------------------------------------------
! Function: H5DSis_attached_f
!
! Purpose: Report if dimension scale dsid is currently attached to dimension idx of dataset did. 
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 17, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSis_attached_f( did, dsid, idx, is_attached, errcode)
    
    IMPLICIT NONE
    
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsis_attached_f
!DEC$endif
!
    INTEGER(hid_t), INTENT(in)  :: did         ! the dataset
    INTEGER(hid_t), INTENT(in)  :: dsid        ! the scale to be attached
    INTEGER       , INTENT(in)  :: idx         ! the dimension of did that dsid is associated with
    LOGICAL       , INTENT(out) :: is_attached ! logical: dimension scale dsid is currently attached to 
                                               ! dimension idx of dataset did
    INTEGER                     :: errcode     ! error code
    INTEGER                     :: c_is_attached
    INTEGER                     :: c_idx
    
    INTERFACE
       INTEGER FUNCTION H5DSis_attached_c(did, dsid, idx, c_is_attached )
         
         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSIS_ATTACHED_C':: h5dsis_attached_c
         !DEC$ENDIF
         INTEGER(hid_t), INTENT(in)  :: did         ! the dataset
         INTEGER(hid_t), INTENT(in)  :: dsid        ! the scale to be detached 
         INTEGER       , INTENT(in)  :: idx         ! the dimension of did to detach
         INTEGER       , INTENT(out) :: c_is_attached ! dimension scale dsid is currently attached to 
       END FUNCTION H5DSis_attached_c
    END INTERFACE

    c_idx = idx - 1 ! account for C-dimensions starting at 0 
    
    errcode = H5DSis_attached_c(did, dsid, c_idx, c_is_attached)

    is_attached = .FALSE. ! default
    IF(c_is_attached.GT.0)THEN
       is_attached = .TRUE.
    ELSE IF(errcode.LT.0)THEN
       errcode = -1
    ENDIF
    
  END SUBROUTINE H5DSis_attached_f

!
! H5DSiterate_scales: Impliment in  F2003
!

!-------------------------------------------------------------------------
! Function: H5DSis_scale_f
!
! Purpose: Determines whether dset is a Dimension Scale. 
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 18, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSis_scale_f( did, is_scale, errcode)
    
    IMPLICIT NONE
    
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsis_scale_f
!DEC$endif
!
    INTEGER(hid_t), INTENT(in)  :: did         ! the data set to query
    LOGICAL       , INTENT(out) :: is_scale    ! logical:  
                                               ! .TRUE. if did is a Dimension Scale
    INTEGER                     :: errcode     ! error code
    INTEGER                     :: c_is_scale
    
    INTERFACE
       INTEGER FUNCTION  H5DSis_scale_c(did,c_is_scale)
         
         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSIS_SCALE_C':: h5dsis_scale_c
         !DEC$ENDIF
         INTEGER(hid_t), INTENT(in) :: did     !  the data set to query
         INTEGER, INTENT(out) :: c_is_scale
       END FUNCTION H5DSis_scale_c
    END INTERFACE
    
    errcode = H5DSis_scale_c(did, c_is_scale)

    is_scale = .FALSE. ! default
    IF(c_is_scale.GT.0)THEN
       is_scale = .TRUE.
    ELSE IF(errcode.LT.0)THEN
       errcode = -1
    ENDIF
    
  END SUBROUTINE H5DSis_scale_f

!-------------------------------------------------------------------------
! Function: H5DSset_label_f
!
! Purpose: Set label for the dimension idx of did to the value label
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 18, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSset_label_f( did, idx, label, errcode)

    IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsset_label_f
!DEC$endif
!

    INTEGER(hid_t),   INTENT(in) :: did    ! The dataset
    INTEGER       ,   INTENT(in) :: idx    ! The dimension
    CHARACTER(LEN=*), INTENT(in) :: label  ! The label
    INTEGER :: errcode                     ! Error code

    INTEGER(SIZE_T) :: label_len  ! Length of label
    INTEGER :: c_idx

    INTERFACE
       INTEGER FUNCTION H5DSset_label_c(did, idx, label, label_len)

         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSSET_LABEL_C'::h5dsset_label_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: label
         INTEGER(hid_t),   INTENT(in) :: did        ! The dataset
         INTEGER       ,   INTENT(in) :: idx        ! The dimension
         CHARACTER(LEN=*), INTENT(in) :: label      ! The label
         INTEGER(SIZE_T),  INTENT(in) :: label_len  ! Length of label
       END FUNCTION H5DSset_label_c
    END INTERFACE

    c_idx = idx - 1

    label_len = LEN(label)
    errcode = H5DSset_label_c(did, c_idx, label, label_len)

  END SUBROUTINE H5DSset_label_f

!-------------------------------------------------------------------------
! Function: H5DSget_label_f
!
! Purpose: Read the label for dimension idx of did into buffer label. 
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 18, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSget_label_f( did, idx, label, size, errcode)

    IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsget_label_f
!DEC$endif
!

    INTEGER(hid_t),   INTENT(in) :: did     ! The dataget
    INTEGER       ,   INTENT(in) :: idx     ! The dimension
    CHARACTER(LEN=*), INTENT(in) :: label   ! The label
    INTEGER(size_t) , INTENT(inout) :: size ! The length of the label buffer
    INTEGER :: errcode                      ! Error code
    INTEGER :: c_idx

    INTERFACE
       INTEGER FUNCTION H5DSget_label_c(did, idx, label, size)

         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSGET_LABEL_C'::h5dsget_label_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: label
         INTEGER(hid_t),   INTENT(in)    :: did        ! The dataget
         INTEGER       ,   INTENT(in)    :: idx        ! The dimension
         CHARACTER(LEN=*), INTENT(in)    :: label      ! The label
         INTEGER(SIZE_T),  INTENT(inout) :: size       ! Length of label
       END FUNCTION H5DSget_label_c
    END INTERFACE

    c_idx = idx - 1

    errcode = H5DSget_label_c(did, c_idx, label, size)

  END SUBROUTINE H5DSget_label_f


!-------------------------------------------------------------------------
! Function: H5DSget_scale_name_f
!
! Purpose: Read the name of scale did into buffer name.
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 18, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSget_scale_name_f(did, name, size, errcode)

    IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsget_scale_name_f
!DEC$endif
!

    INTEGER(hid_t),   INTENT(in) :: did     ! The dataget
    CHARACTER(LEN=*), INTENT(out) :: name   ! The name
    INTEGER(size_t) , INTENT(inout) :: size ! The length of the name buffer
    INTEGER :: errcode                      ! Error code

    INTERFACE
       INTEGER FUNCTION H5DSget_scale_name_c(did, name, size)

         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSGET_SCALE_NAME_C'::h5dsget_scale_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(hid_t),   INTENT(in)    :: did       ! The dataget
         CHARACTER(LEN=*), INTENT(out)   :: name      ! The name
         INTEGER(SIZE_T),  INTENT(inout) :: size      ! Length of name
       END FUNCTION H5DSget_scale_name_c
    END INTERFACE

    errcode = H5DSget_scale_name_c(did, name, size)

  END SUBROUTINE H5DSget_scale_name_f

!-------------------------------------------------------------------------
! Function: H5DSget_num_scales_f
!
! Purpose: Determines how many Dimension Scales are attached to dimension idx of did
!
! Return: Success: 0, Failure: -1
!
! Programmer: M. Scot Breitenfeld
!
! Date: April 18, 2011
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE H5DSget_num_scales_f( did, idx, num_scales, errcode)

    IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_HL_DLL)
!DEC$attributes dllexport :: h5dsget_num_scales_f
!DEC$endif
!
    INTEGER(hid_t), INTENT(in)  :: did         ! the dataset
    INTEGER       , INTENT(in)  :: idx         ! the dimension of did to query
    INTEGER       , INTENT(out) :: num_scales  ! the number of Dimension Scales associated with did
    INTEGER                     :: errcode     ! error code
    INTEGER                     :: c_idx
    
    INTERFACE
       INTEGER FUNCTION  H5DSget_num_scales_c(did, idx, num_scales)
         
         USE h5global
         IMPLICIT NONE
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSGET_NUM_SCALES_C':: h5dsget_num_scales_c
         !DEC$ENDIF
         INTEGER(hid_t), INTENT(in)  :: did        ! the dataset
         INTEGER       , INTENT(in)  :: idx        ! the dimension of did to query
         INTEGER       , INTENT(out) :: num_scales ! the number of Dimension Scales associated with did
       END FUNCTION H5DSget_num_scales_c
    END INTERFACE
    
    c_idx = idx - 1
    errcode = H5DSget_num_scales_c(did, c_idx, num_scales)
    
  END SUBROUTINE H5DSget_num_scales_f

END MODULE h5ds






