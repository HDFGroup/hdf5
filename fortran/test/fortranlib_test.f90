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
! 
!    Testing Fortran functionality.
!
     PROGRAM fortranlibtest

       USE HDF5

     IMPLICIT NONE
     INTEGER :: total_error = 0
     INTEGER :: error 
     INTEGER :: mounting_total_error = 0
     INTEGER :: reopen_total_error = 0
     INTEGER :: fclose_total_error = 0
     INTEGER :: fspace_total_error = 0
     INTEGER :: dataset_total_error = 0
     INTEGER :: extend_dataset_total_error = 0
     INTEGER :: refobj_total_error = 0
     INTEGER :: refreg_total_error = 0
     INTEGER :: dataspace_total_error = 0
     INTEGER :: hyperslab_total_error = 0
     INTEGER :: element_total_error = 0
     INTEGER :: basic_select_total_error = 0
     INTEGER :: total_error_compoundtest = 0
     INTEGER :: basic_datatype_total_error = 0
     INTEGER :: enum_total_error = 0
     INTEGER :: external_total_error = 0
     INTEGER :: multi_file_total_error = 0
     INTEGER :: attribute_total_error = 0
     INTEGER :: identifier_total_error = 0
     INTEGER :: group_total_error = 0
     INTEGER :: error_total_error = 0
     INTEGER :: vl_total_error = 0
     INTEGER :: z_total_error = 0
     INTEGER :: sz_total_error = 0
     INTEGER :: derived_flt_error = 0
     INTEGER :: ret_total_error = 0
     INTEGER :: majnum, minnum, relnum
     CHARACTER(LEN=8) error_string
     CHARACTER(LEN=8) :: success = ' PASSED '
     CHARACTER(LEN=8) :: failure = '*FAILED*'
     CHARACTER(LEN=8) :: skip = '--SKIP--'
     CHARACTER(LEN=4) :: e_format ='(8a)'
     LOGICAL :: szip_flag
     LOGICAL :: cleanup, status

     CALL h5open_f(error) 

     cleanup = .TRUE.
     CALL h5_env_nocleanup_f(status)
     IF(status) cleanup=.FALSE.

     write(*,*) '                       ==========================                            '
     write(*,*) '                              FORTRAN tests '
     write(*,*) '                       ==========================                            '

     CALL h5get_libversion_f(majnum, minnum, relnum, total_error)
     if(total_error .eq. 0) then

     write(*, '(" FORTRANLIB_TEST is linked with HDF5 Library version ")', advance="NO")
     write(*, '(I1)', advance="NO") majnum
     write(*, '(".")', advance="NO") 
     write(*, '(I1)', advance="NO") minnum
     write(*, '(" release ")', advance="NO")
     write(*, '(I3)') relnum
     else
        total_error = total_error + 1
     endif
     write(*,*)
!     CALL h5check_version_f(1,4,4,total_error)

!     write(*,*) '========================================='
!     write(*,*) 'Testing FILE Interface                   '
!     write(*,*) '========================================='
     error_string = failure
     CALL mountingtest(cleanup, mounting_total_error)
     IF (mounting_total_error == 0) error_string = success
     write(*, fmt = '(14a)', advance = 'no') ' Mounting test'     
     write(*, fmt = '(56x,a)', advance = 'no') ' ' 

     write(*, fmt = e_format) error_string 
     total_error = total_error + mounting_total_error 

     error_string = failure
     CALL reopentest(cleanup, reopen_total_error)
     IF (reopen_total_error == 0) error_string = success
     write(*, fmt = '(12a)', advance = 'no') ' Reopen test'     
     write(*, fmt = '(58x,a)', advance = 'no') ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + reopen_total_error 

     error_string = failure
     CALL file_close(cleanup, fclose_total_error)
     IF (fclose_total_error == 0) error_string = success
     write(*, fmt = '(21a)', advance = 'no') ' File open/close test'     
     write(*, fmt = '(49x,a)', advance = 'no') ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + fclose_total_error 

     error_string = failure
     CALL file_space(cleanup, fspace_total_error)
     IF (fspace_total_error == 0) error_string = success
     write(*, fmt = '(21a)', advance = 'no') ' File free space test'     
     write(*, fmt = '(49x,a)', advance = 'no') ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + fspace_total_error 


!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing DATASET Interface                '
!     write(*,*) '========================================='

     error_string = failure
     CALL datasettest(cleanup, dataset_total_error)
     IF (dataset_total_error == 0) error_string = success
     write(*, fmt = '(13a)', advance = 'no') ' Dataset test'     
     write(*, fmt = '(57x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + dataset_total_error 

     error_string = failure
     CALL extenddsettest(cleanup, extend_dataset_total_error)
     IF (extend_dataset_total_error == 0)  error_string = success
     write(*, fmt = '(24a)', advance = 'no') ' Extendible dataset test'     
     write(*, fmt = '(46x,a)', advance = 'no') ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + extend_dataset_total_error 

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing DATASPACE Interface             '
!     write(*,*) '========================================='

     error_string = failure
     CALL dataspace_basic_test(cleanup, dataspace_total_error)
     IF (dataspace_total_error == 0) error_string = success
     write(*, fmt = '(21a)', advance = 'no') ' Basic dataspace test'     
     write(*, fmt = '(49x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + dataspace_total_error 


!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing REFERENCE Interface              '
!     write(*,*) '========================================='

     error_string = failure
     CALL refobjtest(cleanup, refobj_total_error)
     IF (refobj_total_error == 0) error_string = success
     write(*, fmt = '(25a)', advance = 'no') ' Reference to object test'     
     write(*, fmt = '(45x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + refobj_total_error 

     error_string = failure
     CALL refregtest(cleanup, refreg_total_error)
     IF (refreg_total_error == 0) error_string = success
     write(*, fmt = '(33a)', advance = 'no') ' Reference to dataset region test'     
     write(*, fmt = '(37x,a)', advance = 'no')  ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + refreg_total_error 

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing selection functionalities        '
!     write(*,*) '========================================='

     error_string = failure
     CALL test_basic_select(cleanup, basic_select_total_error)
     IF (basic_select_total_error == 0) error_string = success
     write(*, fmt = '(21a)', advance = 'no') ' Basic selection test'     
     write(*, fmt = '(49x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + basic_select_total_error 

     error_string = failure
     CALL  test_select_hyperslab( cleanup, hyperslab_total_error)
     IF ( hyperslab_total_error == 0) error_string = success
     write(*, fmt = '(25a)', advance = 'no') ' Hyperslab selection test'     
     write(*, fmt = '(45x,a)', advance = 'no')  ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + hyperslab_total_error 

     error_string = failure
     CALL test_select_element(cleanup, element_total_error)
     IF (element_total_error == 0) error_string = success
     write(*, fmt = '(23a)', advance = 'no') ' Element selection test'     
     write(*, fmt = '(47x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + element_total_error 

     error_string = failure
     ret_total_error = 0
     CALL test_select_point(cleanup, ret_total_error)
     IF (ret_total_error == 0) error_string = success
     write(*, fmt = '(33a)', advance = 'no') ' Element selection functions test'     
     write(*, fmt = '(37x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + ret_total_error

     error_string = failure
     ret_total_error = 0
     CALL test_select_combine(cleanup, ret_total_error)
     IF (ret_total_error == 0) error_string = success
     write(*, fmt = '(28a)', advance = 'no') ' Selection combinations test'     
     write(*, fmt = '(42x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + ret_total_error

     error_string = failure
     ret_total_error = 0
     CALL test_select_bounds(cleanup, ret_total_error)
     IF (ret_total_error == 0) error_string = success
     write(*, fmt = '(22a)', advance = 'no') ' Selection bounds test'     
     write(*, fmt = '(48x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + ret_total_error

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing DATATYPE interface               '
!     write(*,*) '========================================='

     error_string = failure
     CALL basic_data_type_test(cleanup, basic_datatype_total_error)
     IF (basic_datatype_total_error == 0) error_string = success
     write(*, fmt = '(20a)', advance = 'no') ' Basic datatype test'     
     write(*, fmt = '(50x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + basic_datatype_total_error 

     error_string = failure
     CALL compoundtest(cleanup, total_error_compoundtest)
     IF (total_error_compoundtest == 0) error_string = success
     write(*, fmt = '(23a)', advance = 'no') ' Compound datatype test'     
     write(*, fmt = '(47x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + total_error_compoundtest

     error_string = failure
     CALL enumtest(cleanup, enum_total_error)
     IF (enum_total_error == 0) error_string = success
     write(*, fmt = '(19a)', advance = 'no') ' Enum datatype test'     
     write(*, fmt = '(51x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + enum_total_error 

     error_string = failure
     CALL test_derived_flt(cleanup, derived_flt_error)
     IF (derived_flt_error == 0) error_string = success
     write(*, fmt = '(28a)', advance = 'no') ' Derived float datatype test'     
     write(*, fmt = '(42x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + derived_flt_error

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing PROPERTY interface               ' 
!     write(*,*) '========================================='

     error_string = failure
     CALL external_test(cleanup, external_total_error)
     IF (external_total_error == 0) error_string = success
     write(*, fmt = '(22a)', advance = 'no') ' External dataset test'     
     write(*, fmt = '(48x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + external_total_error 
    
     error_string = failure
     CALL multi_file_test(cleanup, multi_file_total_error)
     IF (multi_file_total_error == 0) error_string = success
     write(*, fmt = '(23a)', advance = 'no') ' Multi file driver test'     
     write(*, fmt = '(47x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + multi_file_total_error 
    
!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing ATTRIBUTE interface              ' 
!     write(*,*) '========================================='

     error_string = failure
     CALL attribute_test(cleanup, attribute_total_error)
     write(*, fmt = '(15a)', advance = 'no') ' Attribute test'     
     write(*, fmt = '(55x,a)', advance = 'no')  ' '
     IF (attribute_total_error == 0) error_string = success
     write(*, fmt = e_format) error_string
     total_error = total_error + attribute_total_error 

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing IDENTIFIER interface             '
!     write(*,*) '========================================='

     error_string = failure
     CALL identifier_test(cleanup, identifier_total_error)
     IF (identifier_total_error == 0) error_string = success
     write(*, fmt = '(16a)', advance = 'no') ' Identifier test'     
     write(*, fmt = '(54x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + identifier_total_error 

     error_string = failure
     CALL filters_test(cleanup, z_total_error)
     IF (z_total_error == 0) error_string = success
     write(*, fmt = '(13a)', advance = 'no') ' Filters test'     
     write(*, fmt = '(57x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + z_total_error 

     CALL szip_test(szip_flag, cleanup, sz_total_error)
     IF (sz_total_error == 0) error_string = success
     ! Reset the flag is compression was not available 
     IF (.NOT. szip_flag) error_string = skip
     IF (sz_total_error .gt. 0) error_string = failure
     write(*, fmt = '(18a)', advance = 'no') ' SZIP filter test'     
     write(*, fmt = '(53x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     if(sz_total_error .gt. 0) total_error = total_error + sz_total_error 

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing GROUP interface             '
!     write(*,*) '========================================='

     error_string = failure
     CALL group_test(cleanup, group_total_error)
     IF (group_total_error == 0) error_string = success
     write(*, fmt = '(11a)', advance = 'no') ' Group test'     
     write(*, fmt = '(59x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + group_total_error 

     error_string = failure
     CALL error_report_test(cleanup, error_total_error)
     IF (error_total_error == 0) error_string = success
     write(*, fmt = '(11a)', advance = 'no') ' Error test'     
     write(*, fmt = '(59x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + error_total_error 

     error_string = failure
     CALL vl_test_integer(cleanup, vl_total_error)
     CALL vl_test_real(cleanup, vl_total_error)
     CALL vl_test_string(cleanup, vl_total_error)
     IF (vl_total_error == 0) error_string = success
     write(*, fmt = '(11a)', advance = 'no') ' VL test'     
     write(*, fmt = '(62x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + vl_total_error 

     write(*,*)

     write(*,*) '                  ============================================  '
     write(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
     write(*, fmt = '(i4)', advance='NO') total_error
     write(*, fmt = '(12a)' ) ' error(s) ! '
     write(*,*) '                  ============================================  '

     CALL h5close_f(error)

     ! if errors detected, exit with non-zero code.
     IF (total_error .ne. 0) CALL h5_exit_f (1)

    END PROGRAM fortranlibtest


