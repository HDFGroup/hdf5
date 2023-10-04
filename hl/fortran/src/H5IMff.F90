!> @defgroup FH5IM Fortran High Level Images (H5IM) Interface
!!
!! @see H5IM, C-HL API
!!
!! @see @ref H5IM_UG, User Guide
!!

!> @ingroup FH5IM
!!
!! @brief This module contains Fortran interfaces for H5IM.
!
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the COPYING file, which can be found at the root of the source code       *
!   distribution tree, or in https://www.hdfgroup.org/licenses.               *
!   If you do not have access to either file, you may request a copy from     *
!   help@hdfgroup.org.                                                        *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

MODULE H5IM
  USE, INTRINSIC :: ISO_C_BINDING
  USE h5fortran_types
  USE hdf5
CONTAINS

!>
!! \ingroup FH5IM
!!
!! \brief Creates and writes an image an 8 bit image
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset to create.
!! \param width	    The width of the image.
!! \param height    The height of the image
!! \param buf       Buffer with data to be written to the dataset
!! \param errcode   \fortran_error
!!
!! See C API: @ref H5IMmake_image_8bit()
!!
  SUBROUTINE h5immake_image_8bit_f(loc_id,&
       dset_name,&
       width,&
       height,&
       buf,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(in) :: width
    INTEGER(hsize_t), INTENT(in) :: height
    INTEGER, INTENT(in), DIMENSION(*) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5immake_image_8bit_c(loc_id,namelen,dset_name,width,height,buf) &
            BIND(C,NAME='h5immake_image_8bit_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(in) :: width
         INTEGER(hsize_t), INTENT(in) :: height
         INTEGER         , INTENT(in), DIMENSION(*) :: buf
       END FUNCTION h5immake_image_8bit_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5immake_image_8bit_c(loc_id,namelen,dset_name,width,height,buf)

  END SUBROUTINE h5immake_image_8bit_f

!>
!! \ingroup FH5IM
!!
!! \brief Reads image data from disk.
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset to create.
!! \param buf       Buffer with data to store the image.
!! \param errcode   \fortran_error
!!
!! See C API: @ref H5IMread_image()
!!
  SUBROUTINE h5imread_image_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER, INTENT(inout), DIMENSION(*) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5imread_image_c(loc_id,namelen,dset_name,buf) &
            BIND(C,NAME='h5imread_image_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER, INTENT(inout), DIMENSION(*) :: buf
       END FUNCTION h5imread_image_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5imread_image_c(loc_id,namelen,dset_name,buf)

  END SUBROUTINE h5imread_image_f

!>
!! \ingroup FH5IM
!!
!! \brief Creates and writes an image a 24 bit image.
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset to create.
!! \param width     The width of the image.
!! \param height    The height of the image.
!! \param il        String defining the interlace mode.
!! \param buf       Buffer with data to be written to the dataset.
!! \param errcode   \fortran_error
!!
!! See C API: @ref H5IMmake_image_24bit()
!!
  SUBROUTINE h5immake_image_24bit_f(loc_id, dset_name, width, height, il, buf, errcode)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(in) :: width
    INTEGER(hsize_t), INTENT(in) :: height
    CHARACTER(len=*), INTENT(in) :: il
    INTEGER, INTENT(in), DIMENSION(*) :: buf
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: ILEN    ! name length

    INTERFACE
       INTEGER FUNCTION h5immake_image_24bit_c(loc_id,namelen,dset_name,ILEN,il,width,height,buf) &
            BIND(C,NAME='h5immake_image_24bit_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(in) :: width
         INTEGER(hsize_t), INTENT(in) :: height
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: il
         INTEGER, INTENT(in), DIMENSION(*) :: buf
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: ILEN

       END FUNCTION h5immake_image_24bit_c
    END INTERFACE

    namelen = LEN(dset_name)
    ILEN    = LEN(il)
    errcode = h5immake_image_24bit_c(loc_id,namelen,dset_name,ILEN,il,width,height,buf)

  END SUBROUTINE h5immake_image_24bit_f

!>
!! \ingroup FH5IM
!!
!! \brief Gets information about an image dataset (dimensions, interlace mode and number of associated palettes).
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset.
!! \param width     The width of the image.
!! \param height    The height of the image.
!! \param planes    The number of color planes of the image.
!! \param interlace The interlace mode of the image.
!! \param npals     The number of palettes associated to the image.
!! \param errcode   \fortran_error
!!
!! See C API: @ref H5IMget_image_info()
!!
  SUBROUTINE h5imget_image_info_f(loc_id,&
       dset_name,&
       width,&
       height,&
       planes,&
       interlace,&
       npals,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER(hsize_t), INTENT(inout) :: width
    INTEGER(hsize_t), INTENT(inout) :: height
    INTEGER(hsize_t), INTENT(inout) :: planes
    INTEGER(hsize_t), INTENT(inout) :: npals
    CHARACTER(len=*), INTENT(inout) :: interlace
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: ILEN    ! name length

    INTERFACE
       INTEGER FUNCTION h5imget_image_info_c(loc_id,namelen,dset_name,width,height,planes,npals,ILEN,interlace) &
            BIND(C,NAME='h5imget_image_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
         INTEGER(hsize_t), INTENT(inout) :: width
         INTEGER(hsize_t), INTENT(inout) :: height
         INTEGER(hsize_t), INTENT(inout) :: planes
         INTEGER(hsize_t), INTENT(inout) :: npals
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(inout) :: interlace
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: ILEN
       END FUNCTION h5imget_image_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    ILEN    = LEN(interlace)
    errcode = h5imget_image_info_c(loc_id,namelen,dset_name,width,height,planes,npals,ILEN,interlace)

  END SUBROUTINE h5imget_image_info_f

!>
!! \ingroup FH5IM
!!
!! \brief Inquires if a dataset is an image.
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset.
!!
!! See C API: @ref H5IMis_image()
!!
  INTEGER FUNCTION h5imis_image_f(loc_id, dset_name)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER :: errcode         ! error code
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5imis_image_c(loc_id,namelen,dset_name) &
            BIND(C,NAME='h5imis_image_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
       END FUNCTION h5imis_image_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5imis_image_c(loc_id,namelen,dset_name)
    h5imis_image_f = errcode

  END FUNCTION h5imis_image_f

!>
!! \ingroup FH5IM
!!
!! \brief Creates and writes a palette
!!
!! \param loc_id   Location identifier. The identifier may be that of a file or group.
!! \param pal_name The name of the palette.
!! \param pal_dims An array of the size of the palette dimensions.
!! \param pal_data Buffer with data to be written to the dataset.
!! \param errcode  \fortran_error
!!
!! See C API: @ref H5IMmake_palette()
!!
  SUBROUTINE h5immake_palette_f(loc_id,&
       pal_name,&
       pal_dims,&
       pal_data,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: pal_name
    INTEGER(hsize_t), INTENT(in), DIMENSION(*) :: pal_dims
    INTEGER, INTENT(in), DIMENSION(*) :: pal_data
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5immake_palette_c(loc_id,namelen,pal_name,pal_dims,pal_data) &
            BIND(C,NAME='h5immake_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: pal_name
         INTEGER(hsize_t), INTENT(in), DIMENSION(*) :: pal_dims
         INTEGER, INTENT(in), DIMENSION(*) :: pal_data
       END FUNCTION h5immake_palette_c
    END INTERFACE

    namelen = LEN(pal_name)
    errcode = h5immake_palette_c(loc_id,namelen,pal_name,pal_dims,pal_data)

  END SUBROUTINE h5immake_palette_f

!>
!! \ingroup FH5IM
!!
!! \brief This function attaches a palette to an existing image dataset.
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param image_name The name of the dataset to attach the palette to.
!! \param pal_name   The name of the palette.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5IMlink_palette()
!!
  SUBROUTINE h5imlink_palette_f(loc_id,&
       image_name,&
       pal_name,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: image_name
    CHARACTER(len=*), INTENT(in) :: pal_name
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: ILEN    ! name length

    INTERFACE
       INTEGER FUNCTION h5imlink_palette_c(loc_id,namelen,image_name,ILEN,pal_name) &
            BIND(C,NAME='h5imlink_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: image_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: pal_name
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: ILEN
       END FUNCTION h5imlink_palette_c
    END INTERFACE

    namelen = LEN(image_name)
    ILEN    = LEN(pal_name)
    errcode = h5imlink_palette_c(loc_id,namelen,image_name,ILEN,pal_name)

  END SUBROUTINE h5imlink_palette_f

!>
!! \ingroup FH5IM
!!
!! \brief This function detaches a palette to an existing image dataset.
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param image_name The name of the image dataset.
!! \param pal_name   The name of the palette.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5IMunlink_palette()
!!
  SUBROUTINE h5imunlink_palette_f(loc_id,&
       image_name,&
       pal_name,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: image_name
    CHARACTER(len=*), INTENT(in) :: pal_name
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length
    INTEGER(size_t) :: ILEN    ! name length

    INTERFACE
       INTEGER FUNCTION h5imunlink_palette_c(loc_id,namelen,image_name,ILEN,pal_name) &
            BIND(C,NAME='h5imunlink_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: image_name
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: pal_name
         INTEGER(size_t) :: namelen
         INTEGER(size_t) :: ILEN
       END FUNCTION h5imunlink_palette_c
    END INTERFACE

    namelen = LEN(image_name)
    ILEN    = LEN(pal_name)
    errcode = h5imunlink_palette_c(loc_id,namelen,image_name,ILEN,pal_name)

  END SUBROUTINE h5imunlink_palette_f

!>
!! \ingroup FH5IM
!!
!! \brief Gets the number of palettes associated to an image.
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param image_name The name of the image dataset.
!! \param npals      The number of palettes.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5IMget_npalettes()
!!
  SUBROUTINE h5imget_npalettes_f(loc_id,&
       image_name,&
       npals,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: image_name
    INTEGER(hsize_t), INTENT(inout) :: npals
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5imget_npalettes_c(loc_id,namelen,image_name,npals) &
            BIND(C,NAME='h5imget_npalettes_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: image_name
         INTEGER(hsize_t), INTENT(inout) :: npals
         INTEGER(size_t) :: namelen
       END FUNCTION h5imget_npalettes_c
    END INTERFACE

    namelen = LEN(image_name)
    errcode = h5imget_npalettes_c(loc_id,namelen,image_name,npals)

  END SUBROUTINE h5imget_npalettes_f

!>
!! \ingroup FH5IM
!!
!! \brief Gets information about a palette dataset (dimensions).
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param image_name The name of the image dataset.
!! \param pal_number The zero based index that identifies the palette.
!! \param pal_dims   The dimensions of the palette dataset.
!! \param  errcode   \fortran_error
!!
!! See C API: @ref H5IMget_palette_info()
!!
  SUBROUTINE h5imget_palette_info_f(loc_id,&
       image_name,&
       pal_number,&
       pal_dims,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: image_name
    INTEGER, INTENT(in) :: pal_number
    INTEGER(hsize_t), DIMENSION(*), INTENT(inout) :: pal_dims
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5imget_palette_info_c(loc_id,namelen,image_name,pal_number,pal_dims) &
            BIND(C,NAME='h5imget_palette_info_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T, HSIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: image_name
         INTEGER, INTENT(in) :: pal_number
         INTEGER(hsize_t), DIMENSION(*), INTENT(inout) :: pal_dims
         INTEGER(size_t) :: namelen
       END FUNCTION h5imget_palette_info_c
    END INTERFACE

    namelen = LEN(image_name)
    errcode = h5imget_palette_info_c(loc_id,namelen,image_name,pal_number,pal_dims)

  END SUBROUTINE h5imget_palette_info_f

!>
!! \ingroup FH5IM
!!
!! \brief Gets the palette dataset
!!
!! \param loc_id     Location identifier. The identifier may be that of a file or group.
!! \param image_name The name of the image dataset.
!! \param pal_number The zero based index that identifies the palette.
!! \param pal_data   The palette dataset.
!! \param errcode    \fortran_error
!!
!! See C API: @ref H5IMget_palette_info()
!!
  SUBROUTINE h5imget_palette_f(loc_id,&
       image_name,&
       pal_number,&
       pal_data,&
       errcode )

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: image_name
    INTEGER, INTENT(in) :: pal_number
    INTEGER, INTENT(inout), DIMENSION(*) :: pal_data
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! length of name buffer

    INTERFACE
       INTEGER FUNCTION h5imget_palette_c(loc_id,namelen,image_name,pal_number,pal_data) &
            BIND(C,NAME='h5imget_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: image_name
         INTEGER, INTENT(in) :: pal_number
         INTEGER, INTENT(inout), DIMENSION(*) :: pal_data
       END FUNCTION h5imget_palette_c
    END INTERFACE

    namelen = LEN(image_name)
    errcode = h5imget_palette_c(loc_id,namelen,image_name,pal_number,pal_data)

  END SUBROUTINE h5imget_palette_f

!>
!! \ingroup FH5IM
!!
!! \brief Inquires if a dataset is a palette. Returns zero (false), a positive (true) or a negative (failure) value.
!!
!! \param loc_id    Location identifier. The identifier may be that of a file or group.
!! \param dset_name The name of the dataset.
!!
!! See C API: @ref H5IMis_palette()
!!
  INTEGER FUNCTION h5imis_palette_f(loc_id, dset_name)

    IMPLICIT NONE

    INTEGER(hid_t),   INTENT(in) :: loc_id
    CHARACTER(len=*), INTENT(in) :: dset_name
    INTEGER :: errcode
    INTEGER(size_t) :: namelen ! name length

    INTERFACE
       INTEGER FUNCTION h5imis_palette_c(loc_id,namelen,dset_name) &
            BIND(C,NAME='h5imis_palette_c')
         IMPORT :: C_CHAR
         IMPORT :: HID_T, SIZE_T
         IMPLICIT NONE
         INTEGER(hid_t),   INTENT(in) :: loc_id
         INTEGER(size_t) :: namelen
         CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(in) :: dset_name
       END FUNCTION h5imis_palette_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5imis_palette_c(loc_id,namelen,dset_name)
    h5imis_palette_f = errcode

  END FUNCTION h5imis_palette_f

END MODULE H5IM

