! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
!   access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
!
!
! This file contains FORTRAN90 interfaces for H5LT functions
!

module h5lt
use h5fortran_types
use hdf5 

interface h5ltmake_dataset_f
 module procedure h5ltmake_dataset_f_int1
 module procedure h5ltmake_dataset_f_int2
 module procedure h5ltmake_dataset_f_int3
 module procedure h5ltmake_dataset_f_float1
 module procedure h5ltmake_dataset_f_float2
 module procedure h5ltmake_dataset_f_float3
 module procedure h5ltmake_dataset_f_double1
 module procedure h5ltmake_dataset_f_double2
 module procedure h5ltmake_dataset_f_double3
end interface

interface h5ltread_dataset_f
 module procedure h5ltread_dataset_f_int1
 module procedure h5ltread_dataset_f_int2
 module procedure h5ltread_dataset_f_int3
 module procedure h5ltread_dataset_f_float1
 module procedure h5ltread_dataset_f_float2
 module procedure h5ltread_dataset_f_float3
 module procedure h5ltread_dataset_f_double1
 module procedure h5ltread_dataset_f_double2
 module procedure h5ltread_dataset_f_double3
end interface

interface h5ltmake_dataset_int_f
 module procedure h5ltmake_dataset_int_f_1
 module procedure h5ltmake_dataset_int_f_2
 module procedure h5ltmake_dataset_int_f_3
end interface

interface h5ltmake_dataset_float_f
 module procedure h5ltmake_dataset_float_f_1
 module procedure h5ltmake_dataset_float_f_2
 module procedure h5ltmake_dataset_float_f_3
end interface

interface h5ltmake_dataset_double_f
 module procedure h5ltmake_dataset_double_f_1
 module procedure h5ltmake_dataset_double_f_2
 module procedure h5ltmake_dataset_double_f_3
end interface

interface h5ltread_dataset_int_f
 module procedure h5ltread_dataset_int_f_1
 module procedure h5ltread_dataset_int_f_2
 module procedure h5ltread_dataset_int_f_3
end interface

interface h5ltread_dataset_float_f
 module procedure h5ltread_dataset_float_f_1
 module procedure h5ltread_dataset_float_f_2
 module procedure h5ltread_dataset_float_f_3
end interface

interface h5ltread_dataset_double_f
 module procedure h5ltread_dataset_double_f_1
 module procedure h5ltread_dataset_double_f_2
 module procedure h5ltread_dataset_double_f_3
end interface

contains
!-------------------------------------------------------------------------
! Make/Read dataset functions
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_int1
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_int1(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_int1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer, intent(in), dimension(*) :: buf           ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), dimension(*) :: buf                ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_int1

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_int2
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_int2(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none 
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_int2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(in), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer  
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_int2

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_int3
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_int3(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_int3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(in), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer 
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_int3


!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_float1
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_float1(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_float1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 real, intent(in), dimension(*) :: buf              ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), dimension(*) :: buf                   ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_float1

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_float2
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_float2(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_float2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(in), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer  
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_float2

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_float3
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_float3(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_float3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(in), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer 
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_float3

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_double1
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_double1(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_double1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 double precision, intent(in), dimension(*) :: buf  ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), dimension(*) :: buf       ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_double1

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_double2
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_double2(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_double2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(in), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer  
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_double2

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_f_double3
!
! Purpose: Creates and writes a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 1, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_f_double3(loc_id,& 
                                dset_name,&
                                rank,& 
                                dims,&
                                type_id,&
                                buf,&
                                errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_f_double3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(in), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer 
 
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

end subroutine h5ltmake_dataset_f_double3



!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_int1
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_int1(loc_id,& 
                                dset_name,&
                                type_id,&
                                buf,&
                                dims,&
                                errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_int1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer, intent(inout), dimension(*) :: buf        ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(HID_T),   intent(IN) :: loc_id                  ! file or group identifier
  integer(HID_T),   intent(IN) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(LEN=*), intent(IN) :: dset_name               ! name of the dataset 
  integer(HSIZE_T), dimension(*), intent(IN) :: dims      ! size of the bufffer buf  
  integer, intent(IN), dimension(*) :: buf                ! data buffer 
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_int1

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_int2
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_int2(loc_id,& 
                                 dset_name,&
                                 type_id,&
                                 buf,&
                                 dims,&
                                 errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_int2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(inout), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer 
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_int2

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_int3
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_int3(loc_id,& 
                                dset_name,&
                                type_id,&
                                buf,&
                                dims,&
                                errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_int3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(inout), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer  
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_int3




!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_float1
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_float1(loc_id,& 
                                dset_name,&
                                type_id,&
                                buf,&
                                dims,&
                                errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_float1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 real, intent(inout), dimension(*) :: buf           ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), dimension(*) :: buf                   ! data buffer 
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_float1

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_float2
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_float2(loc_id,& 
                                 dset_name,&
                                 type_id,&
                                 buf,&
                                 dims,&
                                 errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_float2
!DEC$endif
!


 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(inout), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer 
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_float2

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_float3
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_float3(loc_id,& 
                                dset_name,&
                                type_id,&
                                buf,&
                                dims,&
                                errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_float3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(inout), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer  
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_float3


!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_double1
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_double1(loc_id,& 
                                dset_name,&
                                type_id,&
                                buf,&
                                dims,&
                                errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport ::h5ltread_dataset_f_double1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 double precision, intent(inout), dimension(*) :: buf ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), dimension(*) :: buf       ! data buffer 
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_double1

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_double2
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_double2(loc_id,& 
                                 dset_name,&
                                 type_id,&
                                 buf,&
                                 dims,&
                                 errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_double2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(inout), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer 
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_double2

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_f_double3
!
! Purpose: Read a dataset of a type TYPE_ID
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_f_double3(loc_id,& 
                                dset_name,&
                                type_id,&
                                buf,&
                                dims,&
                                errcode )  

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_f_double3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hid_t),   intent(in) :: type_id            ! datatype identifier 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(inout), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer  
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)

end subroutine h5ltread_dataset_f_double3


!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_int_f_1
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_int_f_1 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_f_1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer, intent(in), dimension(*) :: buf           ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), dimension(*) :: buf                ! data buffer  
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,h5t_native_integer,buf)

end subroutine h5ltmake_dataset_int_f_1

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_int_f_2
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_int_f_2 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_f_2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(in), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 
 
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), &
  dimension(dims(1),dims(2)) :: buf                  ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

end subroutine h5ltmake_dataset_int_f_2


!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_int_f_3
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_int_f_3 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_int_f_3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(in), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer 
 
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

end subroutine h5ltmake_dataset_int_f_3



!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_float_f_1
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_float_f_1 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_f_1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 real, intent(in), dimension(*) :: buf              ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), dimension(*) :: buf                   ! data buffer  
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

end subroutine h5ltmake_dataset_float_f_1

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_float_f_2
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_float_f_2 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_f_2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(in), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), &
  dimension(dims(1),dims(2)) :: buf                  ! data buffer  
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

end subroutine h5ltmake_dataset_float_f_2

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_float_f_3
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_float_f_3 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_float_f_3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(in), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer 
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

end subroutine h5ltmake_dataset_float_f_3




!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_double_f_1
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_double_f_1 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_f_1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(in), &
 dimension(dims(1)) :: buf                          ! data buffer 
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1)) :: buf                               ! data buffer   
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

end subroutine h5ltmake_dataset_double_f_1


!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_double_f_2
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_double_f_2 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_f_2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(in), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer 
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer   
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

end subroutine h5ltmake_dataset_double_f_2

!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_double_f_3
!
! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_double_f_3 (loc_id,& 
                                   dset_name,&
                                   rank,& 
                                   dims,&
                                   buf,&
                                   errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_double_f_3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(in) :: rank               ! rank 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf  
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(in), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer 
            
 interface
  integer function h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_C'::h5ltmake_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(in) :: rank                    ! rank 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(in), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer    
  end function h5ltmake_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

end subroutine h5ltmake_dataset_double_f_3



!-------------------------------------------------------------------------
! Function: h5ltread_dataset_int_f_1
!
! Purpose: Read a dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_int_f_1(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_f_1
!DEC$endif
!

 integer(HID_T),   intent(IN) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(inout), &
 dimension(dims(1)) :: buf                          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(inout), &
  dimension(dims(1)) :: buf                               ! data buffer   
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

end subroutine h5ltread_dataset_int_f_1


!-------------------------------------------------------------------------
! Function: h5ltread_dataset_int_f_2
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_int_f_2(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_f_2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(inout), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(inout), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer    
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

end subroutine h5ltread_dataset_int_f_2

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_int_f_3
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_int_f_3(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_int_f_3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer, intent(inout), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  integer, intent(inout), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer   
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

end subroutine h5ltread_dataset_int_f_3


!-------------------------------------------------------------------------
! Function: h5ltread_dataset_float_f_1
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_float_f_1(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_f_1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(inout), &
 dimension(dims(1)) :: buf                          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(inout), &
  dimension(dims(1)) :: buf                               ! data buffer   
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

end subroutine h5ltread_dataset_float_f_1


!-------------------------------------------------------------------------
! Function: h5ltread_dataset_float_f_2
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_float_f_2(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_f_2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(inout), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(inout), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer    
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

end subroutine h5ltread_dataset_float_f_2

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_float_f_3
!
! Purpose: Read a dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_float_f_3(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_float_f_3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 real, intent(inout), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  real, intent(inout), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer   
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

end subroutine h5ltread_dataset_float_f_3

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_double_f_1
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_double_f_1(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_f_1
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(inout), &
 dimension(dims(1)) :: buf                          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(inout), &
  dimension(dims(1)) :: buf                               ! data buffer   
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

end subroutine h5ltread_dataset_double_f_1


!-------------------------------------------------------------------------
! Function: h5ltread_dataset_double_f_2
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_double_f_2(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_f_2
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(inout), &
 dimension(dims(1),dims(2)) :: buf                  ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(inout), &
  dimension(dims(1),dims(2)) :: buf                       ! data buffer    
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

end subroutine h5ltread_dataset_double_f_2

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_double_f_3
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_double_f_3(loc_id,& 
                                   dset_name,&
                                   buf,&
                                   dims,& 
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_double_f_3
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t), dimension(*), intent(in) :: dims ! size of the bufffer buf 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 double precision, intent(inout), &
 dimension(dims(1),dims(2),dims(3)) :: buf          ! data buffer  

 interface
  integer function h5ltread_dataset_c(loc_id,namelen,dset_name,type_id,buf,dims)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_C'::h5ltread_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer(hid_t),   intent(in) :: type_id                 ! datatype identifier 
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t), dimension(*), intent(in) :: dims      ! size of the bufffer buf  
  double precision, intent(inout), &
  dimension(dims(1),dims(2),dims(3)) :: buf               ! data buffer   
  end function h5ltread_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

end subroutine h5ltread_dataset_double_f_3


!-------------------------------------------------------------------------
! Function: h5ltmake_dataset_string_f
!
! Purpose: Creates and writes a dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltmake_dataset_string_f(loc_id,& 
                                     dset_name,&
                                     buf,&
                                     errcode )   

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltmake_dataset_string_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: buf                ! data buffer 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
            
 interface
  integer function h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTMAKE_DATASET_STRING_C'::h5ltmake_dataset_string_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: buf                     ! data buffer  
  end function h5ltmake_dataset_string_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buf)

end subroutine h5ltmake_dataset_string_f

!-------------------------------------------------------------------------
! Function: h5ltread_dataset_string_f
!
! Purpose: Read a dataset 
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 22, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltread_dataset_string_f(loc_id,& 
                                     dset_name,&
                                     buf,&
                                     errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltread_dataset_string_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(inout) :: buf             ! data buffer
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTREAD_DATASET_STRING_C'::h5ltread_dataset_string_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(inout) :: buf                  ! data buffer
  end function h5ltread_dataset_string_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf)

end subroutine h5ltread_dataset_string_f




!-------------------------------------------------------------------------
! Make/Read attribute functions
!-------------------------------------------------------------------------


!-------------------------------------------------------------------------
! Function: h5ltset_attribute_int_f
!
! Purpose: Create and write an attribute
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

subroutine h5ltset_attribute_int_f(loc_id,& 
                                    dset_name,&
                                    attr_name,&
                                    buf,&
                                    size,&
                                    errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltset_attribute_int_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer(size_t),  intent(in) :: size               ! size of attribute array
 integer :: errcode                                 ! error code
 integer, intent(in), dimension(*) :: buf           ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltset_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTSET_ATTRIBUTE_INT_C'::h5ltset_attribute_int_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  integer(size_t),  intent(in) :: size                    ! size of attribute array
  integer, intent(in), dimension(*) :: buf                ! data buffer
  end function h5ltset_attribute_int_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltset_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)

end subroutine h5ltset_attribute_int_f

!-------------------------------------------------------------------------
! Function: h5ltset_attribute_float_f
!
! Purpose: Create and write an attribute
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

subroutine h5ltset_attribute_float_f(loc_id,& 
                                    dset_name,&
                                    attr_name,&
                                    buf,&
                                    size,&
                                    errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltset_attribute_float_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer(size_t),  intent(in) :: size               ! size of attribute array
 integer :: errcode                                 ! error code
 real, intent(in), dimension(*) :: buf              ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltset_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTSET_ATTRIBUTE_FLOAT_C'::h5ltset_attribute_float_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  integer(size_t),  intent(in) :: size                    ! size of attribute array
  real, intent(in), dimension(*) :: buf                   ! data buffer
  end function h5ltset_attribute_float_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltset_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)

end subroutine h5ltset_attribute_float_f

!-------------------------------------------------------------------------
! Function: h5ltset_attribute_double_f
!
! Purpose: Create and write an attribute
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

subroutine h5ltset_attribute_double_f(loc_id,& 
                                      dset_name,&
                                      attr_name,&
                                      buf,&
                                      size,&
                                      errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltset_attribute_double_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer(size_t),  intent(in) :: size               ! size of attribute array
 integer :: errcode                                 ! error code
 double precision, intent(in), dimension(*) :: buf  ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltset_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTSET_ATTRIBUTE_DOUBLE_C'::h5ltset_attribute_double_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  integer(size_t),  intent(in) :: size                    ! size of attribute array
  double precision, intent(in), dimension(*) :: buf       ! data buffer
  end function h5ltset_attribute_double_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltset_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)

end subroutine h5ltset_attribute_double_f


!-------------------------------------------------------------------------
! Function: h5ltset_attribute_string_f
!
! Purpose: Create and write an attribute
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

subroutine h5ltset_attribute_string_f(loc_id,& 
                                      dset_name,&
                                      attr_name,&
                                      buf,&
                                      errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltset_attribute_string_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer :: errcode                                 ! error code
 character(len=*), intent(in) :: buf                ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltset_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTSET_ATTRIBUTE_STRING_C'::h5ltset_attribute_string_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  character(len=*), intent(in) :: buf                     ! data buffer
  end function h5ltset_attribute_string_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltset_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

end subroutine h5ltset_attribute_string_f



!-------------------------------------------------------------------------
! Function: h5ltget_attribute_int_f
!
! Purpose: Reads an attribute named ATTR_NAME
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

subroutine h5ltget_attribute_int_f(loc_id,& 
                                    dset_name,&
                                    attr_name,&
                                    buf,&
                                    errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_attribute_int_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer :: errcode                                 ! error code
 integer, intent(inout), dimension(*) :: buf        ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltget_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_ATTRIBUTE_INT_C'::h5ltget_attribute_int_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  integer, intent(inout), dimension(*) :: buf             ! data buffer
  end function h5ltget_attribute_int_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltget_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

end subroutine h5ltget_attribute_int_f


!-------------------------------------------------------------------------
! Function: h5ltget_attribute_float_f
!
! Purpose: Reads an attribute named ATTR_NAME
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

subroutine h5ltget_attribute_float_f(loc_id,& 
                                    dset_name,&
                                    attr_name,&
                                    buf,&
                                    errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_attribute_float_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer :: errcode                                 ! error code
 real, intent(inout), dimension(*) :: buf           ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltget_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_ATTRIBUTE_FLOAT_C'::h5ltget_attribute_float_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  real, intent(inout), dimension(*) :: buf                ! data buffer
  end function h5ltget_attribute_float_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltget_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

end subroutine h5ltget_attribute_float_f

!-------------------------------------------------------------------------
! Function: h5ltget_attribute_double_f
!
! Purpose: Reads an attribute named ATTR_NAME
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

subroutine h5ltget_attribute_double_f(loc_id,& 
                                    dset_name,&
                                    attr_name,&
                                    buf,&
                                    errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_attribute_double_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer :: errcode                                 ! error code
 double precision,intent(inout),dimension(*) :: buf ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltget_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_ATTRIBUTE_DOUBLE_C'::h5ltget_attribute_double_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  double precision, intent(inout), dimension(*) :: buf    ! data buffer
  end function h5ltget_attribute_double_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltget_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

end subroutine h5ltget_attribute_double_f

!-------------------------------------------------------------------------
! Function: h5ltget_attribute_string_f
!
! Purpose: Reads an attribute named ATTR_NAME
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

subroutine h5ltget_attribute_string_f(loc_id,& 
                                      dset_name,&
                                      attr_name,&
                                      buf,&
                                      errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_attribute_string_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer :: errcode                                 ! error code
 character(len=*), intent(inout) :: buf             ! data buffer
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltget_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_ATTRIBUTE_STRING_C'::h5ltget_attribute_string_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  character(len=*), intent(inout) :: buf                  ! data buffer
  end function h5ltget_attribute_string_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltget_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

end subroutine h5ltget_attribute_string_f

!-------------------------------------------------------------------------
! Query dataset functions
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! Function: h5ltget_dataset_ndims_f
!
! Purpose: Gets the dimensionality of a dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 30, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltget_dataset_ndims_f(loc_id,& 
                                   dset_name,&
                                   rank,&
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_dataset_ndims_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer,          intent(inout) :: rank            ! rank 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_DATASET_NDIMS_C'::h5ltget_dataset_ndims_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer,          intent(inout) :: rank                 ! rank 
  end function h5ltget_dataset_ndims_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank)

end subroutine h5ltget_dataset_ndims_f


!-------------------------------------------------------------------------
! Function: h5ltfind_dataset_f
!
! Purpose: Inquires if a dataset named dset_name exists attached 
!           to the object loc_id.
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

integer function h5ltfind_dataset_f(loc_id,& 
                                    dset_name) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltfind_dataset_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltfind_dataset_c(loc_id,namelen,dset_name)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTFIND_DATASET_C'::h5ltfind_dataset_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  end function h5ltfind_dataset_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltfind_dataset_c(loc_id,namelen,dset_name)
 h5ltfind_dataset_f = errcode

end function h5ltfind_dataset_f

!-------------------------------------------------------------------------
! Function: h5ltget_dataset_info_f
!
! Purpose: Gets information about a dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 30, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltget_dataset_info_f(loc_id,& 
                                   dset_name,&
                                   dims,&
                                   type_class,&
                                   type_size,&
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_dataset_info_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 integer(hsize_t),dimension(*),intent(inout):: dims ! dimensions 
 integer, intent(inout)         :: type_class       ! type class
 integer(size_t), intent(inout) :: type_size        ! type size
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_DATASET_INFO_C'::h5ltget_dataset_info_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  integer(hsize_t),dimension(*),intent(inout):: dims      ! dimensions 
  integer, intent(inout)         :: type_class            ! type class
  integer(size_t), intent(inout) :: type_size             ! type size 
  end function h5ltget_dataset_info_c
 end interface

 namelen = len(dset_name)
 errcode = h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size)

end subroutine h5ltget_dataset_info_f


!-------------------------------------------------------------------------
! Query attribute functions
!-------------------------------------------------------------------------


!-------------------------------------------------------------------------
! Function: h5ltget_attribute_ndims_f
!
! Purpose: Create and write an attribute
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

subroutine h5ltget_attribute_ndims_f(loc_id,& 
                                    dset_name,&
                                    attr_name,&
                                    rank,&
                                    errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_attribute_ndims_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer,          intent(inout) :: rank            ! rank 
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltget_attribute_ndims_c(loc_id,namelen,dset_name,attrlen,attr_name,rank)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_ATTRIBUTE_NDIMS_C'::h5ltget_attribute_ndims_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  integer,          intent(inout) :: rank                 ! rank 
  end function h5ltget_attribute_ndims_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltget_attribute_ndims_c(loc_id,namelen,dset_name,attrlen,attr_name,rank)

end subroutine h5ltget_attribute_ndims_f


!-------------------------------------------------------------------------
! Function: h5ltget_attribute_info_f
!
! Purpose: Gets information about an attribute
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: September 30, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5ltget_attribute_info_f(loc_id,& 
                                   dset_name,&
                                   attr_name,&
                                   dims,&
                                   type_class,&
                                   type_size,&
                                   errcode ) 

 implicit none
 
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5ltget_attribute_info_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier 
 character(len=*), intent(in) :: dset_name          ! name of the dataset 
 character(len=*), intent(in) :: attr_name          ! name of the attribute
 integer(hsize_t),dimension(*),intent(inout):: dims ! dimensions 
 integer, intent(inout)         :: type_class       ! type class
 integer(size_t), intent(inout) :: type_size        ! type size
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer :: attrlen                                 ! name length

 interface
  integer function h5ltget_attribute_info_c(loc_id,namelen,dset_name,attrlen,attr_name,dims,type_class,type_size)
  use h5global
  !DEC$ IF DEFINED(HDF5F90_WINDOWS)
  !MS$ATTRIBUTES C,reference,alias:'_H5LTGET_ATTRIBUTE_INFO_C'::h5ltget_attribute_info_c
  !DEC$ ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: attr_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  integer :: attrlen                                      ! lenght of attr name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset 
  character(len=*), intent(in) :: attr_name               ! name of the attribute
  integer(hsize_t),dimension(*),intent(inout):: dims      ! dimensions 
  integer, intent(inout)         :: type_class            ! type class
  integer(size_t), intent(inout) :: type_size             ! type size 
  end function h5ltget_attribute_info_c
 end interface

 namelen = len(dset_name)
 attrlen = len(attr_name)
 errcode = h5ltget_attribute_info_c(loc_id,namelen,dset_name,attrlen,attr_name,dims,type_class,type_size)

end subroutine h5ltget_attribute_info_f



!  end
!
end module H5LT
            

                                     



