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
! This file contains FORTRAN interfaces for H5IM functions
!
! NOTES
!
!       _____ __  __ _____   ____  _____ _______       _   _ _______
!      |_   _|  \/  |  __ \ / __ \|  __ \__   __|/\   | \ | |__   __|
! ****   | | | \  / | |__) | |  | | |__) | | |  /  \  |  \| |  | |    ****
! ****   | | | |\/| |  ___/| |  | |  _  /  | | / /\ \ | . ` |  | |    ****
! ****  _| |_| |  | | |    | |__| | | \ \  | |/ ____ \| |\  |  | |    ****
!      |_____|_|  |_|_|     \____/|_|  \_\ |_/_/    \_\_| \_|  |_|
!                             
!  If you add a new function here then you MUST add the function name to the
!  Windows dll file 'hdf5_hl_fortrandll.def.in' in the hl/fortran/src directory.
!  This is needed for Windows based operating systems.
!

MODULE h5im
  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE hdf5
CONTAINS

!-------------------------------------------------------------------------
! Function: h5immake_image_8bit_f
!
! Purpose: Creates and writes an image an 8 bit image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5immake_image_8bit_f(loc_id,&
       dset_name,&
       width,&
       height,&
       buf,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), INTENT(in) :: width              ! width of image
    INTEGER(hsize_t), INTENT(in) :: height             ! height of image
    INTEGER, INTENT(in), DIMENSION(*) :: buf           ! buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length

    INTERFACE
       INTEGER FUNCTION h5immake_image_8bit_c(loc_id,namelen,dset_name,width,height,buf) &
            BIND(C,NAME='h5immake_image_8bit_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                              ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name   ! name of the dataset
         INTEGER(hsize_t), INTENT(in) :: width                   ! width of image
         INTEGER(hsize_t), INTENT(in) :: height                  ! height of image
         INTEGER         , INTENT(in), DIMENSION(*) :: buf       ! buffer
       END FUNCTION h5immake_image_8bit_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5immake_image_8bit_c(loc_id,namelen,dset_name,width,height,buf)
    
  END SUBROUTINE h5immake_image_8bit_f

!-------------------------------------------------------------------------
! Function: h5imread_image_f
!
! Purpose: Reads image data from disk.
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------
  SUBROUTINE h5imread_image_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE
   
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER, INTENT(inout), DIMENSION(*) :: buf        ! buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                         ! name length

    INTERFACE
       INTEGER FUNCTION h5imread_image_c(loc_id,namelen,dset_name,buf) &
            BIND(C,NAME='h5imread_image_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                              ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER, INTENT(inout), DIMENSION(*) :: buf             ! buffer
       END FUNCTION h5imread_image_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5imread_image_c(loc_id,namelen,dset_name,buf)
    
  END SUBROUTINE h5imread_image_f

!-------------------------------------------------------------------------
! Function: h5immake_image_24bit_f
!
! Purpose: Creates and writes an image a 24 bit image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5immake_image_24bit_f(loc_id,&
       dset_name,&
       width,&
       height,&
       il,&
       buf,&
       errcode )
    
    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), INTENT(in) :: width              ! width of image
    INTEGER(hsize_t), INTENT(in) :: height             ! height of image
    CHARACTER(len=*), INTENT(in) :: il                 ! interlace
    INTEGER, INTENT(in), DIMENSION(*) :: buf           ! buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: ILEN                                    ! name length
    
    INTERFACE
       INTEGER FUNCTION h5immake_image_24bit_c(loc_id,namelen,dset_name,ILEN,il,width,height,buf) &
            BIND(C,NAME='h5immake_image_24bit_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), INTENT(in) :: width                   ! width of image
         INTEGER(hsize_t), INTENT(in) :: height                  ! height of image
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: il                      ! interlace
         INTEGER, INTENT(in), DIMENSION(*) :: buf                ! buffer
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         INTEGER(size_t) :: ILEN                                         ! name length
         
       END FUNCTION h5immake_image_24bit_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    ILEN    = LEN(il)
    errcode = h5immake_image_24bit_c(loc_id,namelen,dset_name,ILEN,il,width,height,buf)
    
  END SUBROUTINE h5immake_image_24bit_f

!-------------------------------------------------------------------------
! Function: h5imget_image_info_f
!
! Purpose: Gets information about an image dataset (dimensions, interlace mode
!          and number of associated palettes).
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5imget_image_info_f(loc_id,&
       dset_name,&
       width,&
       height,&
       planes,&
       interlace,&
       npals,&
       errcode )
    
    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), INTENT(inout) :: width           ! width of image
    INTEGER(hsize_t), INTENT(inout) :: height          ! height of image
    INTEGER(hsize_t), INTENT(inout) :: planes          ! color planes
    INTEGER(hsize_t), INTENT(inout) :: npals           ! palettes
    CHARACTER(len=*), INTENT(inout) :: interlace       ! interlace
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: ILEN                                    ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imget_image_info_c(loc_id,namelen,dset_name,width,height,planes,npals,ILEN,interlace) &
            BIND(C,NAME='h5imget_image_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name          ! name of the dataset
         INTEGER(hsize_t), INTENT(inout) :: width           ! width of image
         INTEGER(hsize_t), INTENT(inout) :: height          ! height of image
         INTEGER(hsize_t), INTENT(inout) :: planes          ! color planes
         INTEGER(hsize_t), INTENT(inout) :: npals           ! palettes
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(inout) :: interlace       ! interlace
         INTEGER(size_t) :: namelen                                 ! name length
         INTEGER(size_t) :: ILEN                                    ! name length
       END FUNCTION h5imget_image_info_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    ILEN    = LEN(interlace)
    errcode = h5imget_image_info_c(loc_id,namelen,dset_name,width,height,planes,npals,ILEN,interlace)
    
  END SUBROUTINE h5imget_image_info_f

!-------------------------------------------------------------------------
! Function: h5imis_image_f
!
! Purpose: Inquires if a dataset is an image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  INTEGER FUNCTION h5imis_image_f(loc_id,&
       dset_name)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imis_image_c(loc_id,namelen,dset_name) &
            BIND(C,NAME='h5imis_image_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
       END FUNCTION h5imis_image_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5imis_image_c(loc_id,namelen,dset_name)
    h5imis_image_f = errcode
    
  END FUNCTION h5imis_image_f
  

!-------------------------------------------------------------------------
! Function: h5immake_palette_f
!
! Purpose: Creates and writes a palette
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5immake_palette_f(loc_id,&
       dset_name,&
       pal_dims,&
       buf,&
       errcode )
    
    IMPLICIT NONE
    
    INTEGER(hid_t),   INTENT(in) :: loc_id                 ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name              ! name of the dataset
    INTEGER(hsize_t), INTENT(in), DIMENSION(*) :: pal_dims ! dimensions
    INTEGER, INTENT(in), DIMENSION(*) :: buf               ! buffer
    INTEGER :: errcode                                     ! error code
    INTEGER(size_t) :: namelen                                     ! name length
    
    INTERFACE
       INTEGER FUNCTION h5immake_palette_c(loc_id,namelen,dset_name,pal_dims,buf) &
            BIND(C,NAME='h5immake_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), INTENT(in), DIMENSION(*) :: pal_dims  ! dimensions
         INTEGER, INTENT(in), DIMENSION(*) :: buf                ! buffer
       END FUNCTION h5immake_palette_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5immake_palette_c(loc_id,namelen,dset_name,pal_dims,buf)
    
  END SUBROUTINE h5immake_palette_f
  
!-------------------------------------------------------------------------
! Function: h5imlink_palette_f
!
! Purpose: This function attaches a palette to an existing image dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5imlink_palette_f(loc_id,&
       dset_name,&
       pal_name,&
       errcode )
    
    IMPLICIT NONE
    
    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: pal_name           ! palette name
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: ILEN                                    ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imlink_palette_c(loc_id,namelen,dset_name,ILEN,pal_name) &
            BIND(C,NAME='h5imlink_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name          ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: pal_name           ! palette name
         INTEGER(size_t) :: namelen                                 ! name length
         INTEGER(size_t) :: ILEN                                    ! name length
       END FUNCTION h5imlink_palette_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    ILEN    = LEN(pal_name)
    errcode = h5imlink_palette_c(loc_id,namelen,dset_name,ILEN,pal_name)
    
  END SUBROUTINE h5imlink_palette_f
  

!-------------------------------------------------------------------------
! Function: h5imunlink_palette_f
!
! Purpose: This function dettaches a palette to an existing image dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5imunlink_palette_f(loc_id,&
       dset_name,&
       pal_name,&
       errcode )
    
    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: pal_name           ! palette name
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    INTEGER(size_t) :: ILEN                                    ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imunlink_palette_c(loc_id,namelen,dset_name,ILEN,pal_name) &
            BIND(C,NAME='h5imunlink_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name          ! name of the dataset
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: pal_name           ! palette name
         INTEGER(size_t) :: namelen                                 ! name length
         INTEGER(size_t) :: ILEN                                    ! name length
       END FUNCTION h5imunlink_palette_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    ILEN    = LEN(pal_name)
    errcode = h5imunlink_palette_c(loc_id,namelen,dset_name,ILEN,pal_name)
    
  END SUBROUTINE h5imunlink_palette_f
  
!-------------------------------------------------------------------------
! Function: h5imget_npalettes_f
!
! Purpose: Gets the number of palettes associated to an image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5imget_npalettes_f(loc_id,&
       dset_name,&
       npals,&
       errcode )
    
    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), INTENT(inout) :: npals           ! palettes
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imget_npalettes_c(loc_id,namelen,dset_name,npals) &
            BIND(C,NAME='h5imget_npalettes_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name          ! name of the dataset
         INTEGER(hsize_t), INTENT(inout) :: npals           ! palettes
         INTEGER(size_t) :: namelen                                 ! name length
       END FUNCTION h5imget_npalettes_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5imget_npalettes_c(loc_id,namelen,dset_name,npals)
    
  END SUBROUTINE h5imget_npalettes_f


!-------------------------------------------------------------------------
! Function: h5imget_palette_info_f
!
! Purpose: Get palette information
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5imget_palette_info_f(loc_id,&
       dset_name,&
       pal_number,&
       dims,&
       errcode )
    
    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id                ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name             ! name of the dataset
    INTEGER, INTENT(in) :: pal_number                     ! palette number
    INTEGER(hsize_t), DIMENSION(*), INTENT(inout) :: dims ! dimensions
    INTEGER :: errcode                                    ! error code
    INTEGER(size_t) :: namelen                                    ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imget_palette_info_c(loc_id,namelen,dset_name,pal_number,dims) &
            BIND(C,NAME='h5imget_palette_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                ! file or group identifier
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name             ! name of the dataset
         INTEGER, INTENT(in) :: pal_number                     ! palette number
         INTEGER(hsize_t), DIMENSION(*), INTENT(inout) :: dims ! dimensions
         INTEGER(size_t) :: namelen                                    ! name length
       END FUNCTION h5imget_palette_info_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5imget_palette_info_c(loc_id,namelen,dset_name,pal_number,dims)
    
  END SUBROUTINE h5imget_palette_info_f

!-------------------------------------------------------------------------
! Function: h5imget_palette_f
!
! Purpose: Reads palette
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  SUBROUTINE h5imget_palette_f(loc_id,&
       dset_name,&
       pal_number,&
       buf,&
       errcode )
    
    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER, INTENT(in) :: pal_number                  ! palette number
    INTEGER, INTENT(inout), DIMENSION(*) :: buf        ! buffer
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imget_palette_c(loc_id,namelen,dset_name,pal_number,buf) &
            BIND(C,NAME='h5imget_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                                      ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER, INTENT(in) :: pal_number                       ! palette number
         INTEGER, INTENT(inout), DIMENSION(*) :: buf             ! buffer
       END FUNCTION h5imget_palette_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5imget_palette_c(loc_id,namelen,dset_name,pal_number,buf)
    
  END SUBROUTINE h5imget_palette_f


!-------------------------------------------------------------------------
! Function: h5imis_palette_f
!
! Purpose: Inquires if a dataset is a palette
!
! Return: true, false, fail
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

  INTEGER FUNCTION h5imis_palette_f(loc_id,&
                                  dset_name)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER :: errcode                                 ! error code
    INTEGER(size_t) :: namelen                                 ! name length
    
    INTERFACE
       INTEGER FUNCTION h5imis_palette_c(loc_id,namelen,dset_name) &
            BIND(C,NAME='h5imis_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(size_t) :: namelen                              ! length of name buffer
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name               ! name of the dataset
       END FUNCTION h5imis_palette_c
    END INTERFACE
    
    namelen = LEN(dset_name)
    errcode = h5imis_palette_c(loc_id,namelen,dset_name)
    h5imis_palette_f = errcode
    
  END FUNCTION h5imis_palette_f

END MODULE H5IM





