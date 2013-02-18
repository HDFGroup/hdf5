!****h* ROBODoc/H5F (F03)
!
! NAME
!  H5F_PROVISIONAL
!
! PURPOSE
!  This file contains Fortran 2003 interfaces for H5F functions.
!
! COPYRIGHT
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
! NOTES
!                         *** IMPORTANT ***
!  If you add a new H5T function you must add the function name to the
!  Windows dll file 'hdf5_fortrandll.def.in' in the fortran/src directory.
!  This is needed for Windows based operating systems.
!
!*****


MODULE H5F_PROVISIONAL

  USE H5GLOBAL
  USE, INTRINSIC :: ISO_C_BINDING


CONTAINS
!****s* H5F (F03)/h5fget_file_image_f_F03
!
! NAME
!  h5fget_file_image_f
!
! PURPOSE
!  Retrieves a copy of the image of an existing, open file. 
!
! Inputs:
!  file_id    - Target file identifier.
!  buf_ptr    - Pointer to the buffer into which the image of the HDF5 file is to be copied.
!  buf_len    - Size of the supplied buffer.
!
! Outputs:
!  hdferr     - error code:
!                 0 on success and -1 on failure
! Optional:  
!  buf_size   - The size in bytes of the buffer required to store the file image.
!
! AUTHOR
!  M. Scot Breitenfeld
!  November 26, 2012
!
! Fortran2003 Interface:
  SUBROUTINE h5fget_file_image_f(file_id, buf_ptr, buf_len, hdferr, buf_size)
    USE, INTRINSIC :: ISO_C_BINDING
    IMPLICIT NONE
    INTEGER(HID_T)  , INTENT(IN)    :: file_id
    TYPE(C_PTR)     , INTENT(INOUT) :: buf_ptr
    INTEGER(SIZE_T) , INTENT(IN)    :: buf_len
    INTEGER(SIZE_T) , INTENT(OUT), OPTIONAL :: buf_size
    INTEGER         , INTENT(OUT)   :: hdferr
!*****

    INTEGER(SIZE_T) :: buf_size_default

    INTERFACE
       INTEGER FUNCTION h5fget_file_image_c(file_id, buf_ptr, buf_len, buf_size)
         USE, INTRINSIC :: ISO_C_BINDING
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_FILE_IMAGE_C'::h5fget_file_image_c
         !DEC$ENDIF
         INTEGER(HID_T)  , INTENT(IN)    :: file_id
         TYPE(C_PTR)     , VALUE         :: buf_ptr
         INTEGER(SIZE_T) , INTENT(IN)    :: buf_len
         INTEGER(SIZE_T), INTENT(IN)     :: buf_size
       END FUNCTION h5fget_file_image_c
    END INTERFACE


    IF(PRESENT(buf_size))THEN
       buf_ptr = C_NULL_PTR
    ENDIF

    hdferr = h5fget_file_image_c(file_id, buf_ptr, buf_len, buf_size_default)

    IF(PRESENT(buf_size))THEN
       buf_size = buf_size_default
    ENDIF

  END SUBROUTINE h5fget_file_image_f

END MODULE H5F_PROVISIONAL
