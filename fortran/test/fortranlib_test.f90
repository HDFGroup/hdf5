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
     INTEGER :: external_total_error = 0
     INTEGER :: attribute_total_error = 0
     INTEGER :: identifier_total_error = 0
     INTEGER :: group_total_error = 0
     CHARACTER*8 error_string
     CHARACTER*8 :: success = ' PASSED '
     CHARACTER*8 :: failure = '*FAILED*'
     CHARACTER*4 :: e_format ='(8a)'

     CALL h5open_f(error) 
     write(*,*) '                       ==========================                            '
     write(*,*) '                              FORTRAN tests '
     write(*,*) '                       ==========================                            '
!     write(*,*) '========================================='
!     write(*,*) 'Testing FILE Interface                   '
!     write(*,*) '========================================='

     error_string = failure
     CALL mountingtest(mounting_total_error)
     IF (mounting_total_error == 0) error_string = success
     write(*, fmt = '(14a)', advance = 'no') ' Mounting test'     
     write(*, fmt = '(56x,a)', advance = 'no') ' ' 

     write(*, fmt = e_format) error_string 
     total_error = total_error + mounting_total_error 

     error_string = failure
     CALL reopentest(reopen_total_error)
     IF (reopen_total_error == 0) error_string = success
     write(*, fmt = '(12a)', advance = 'no') ' Reopen test'     
     write(*, fmt = '(58x,a)', advance = 'no') ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + reopen_total_error 


!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing DATASET Interface                '
!     write(*,*) '========================================='

     error_string = failure
     CALL datasettest(dataset_total_error)
     IF (dataset_total_error == 0) error_string = success
     write(*, fmt = '(13a)', advance = 'no') ' Dataset test'     
     write(*, fmt = '(57x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + dataset_total_error 

     error_string = failure
     CALL extenddsettest(extend_dataset_total_error)
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
     CALL dataspace_basic_test(dataspace_total_error)
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
     CALL refobjtest(refobj_total_error)
     IF (refobj_total_error == 0) error_string = success
     write(*, fmt = '(25a)', advance = 'no') ' Reference to object test'     
     write(*, fmt = '(45x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + refobj_total_error 

     error_string = failure
     CALL refregtest(refreg_total_error)
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
     CALL test_basic_select(basic_select_total_error)
     IF (basic_select_total_error == 0) error_string = success
     write(*, fmt = '(21a)', advance = 'no') ' Basic selection test'     
     write(*, fmt = '(49x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + basic_select_total_error 

     error_string = failure
     CALL  test_select_hyperslab( hyperslab_total_error)
     IF ( hyperslab_total_error == 0) error_string = success
     write(*, fmt = '(25a)', advance = 'no') ' Hyperslab selection test'     
     write(*, fmt = '(45x,a)', advance = 'no')  ' ' 
     write(*, fmt = e_format) error_string
     total_error = total_error + hyperslab_total_error 

     error_string = failure
     CALL test_select_element(element_total_error)
     IF (element_total_error == 0) error_string = success
     write(*, fmt = '(23a)', advance = 'no') ' Element selection test'     
     write(*, fmt = '(47x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + element_total_error 


!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing DATATYPE interface               '
!     write(*,*) '========================================='

     error_string = failure
     CALL basic_data_type_test(basic_datatype_total_error)
     IF (basic_datatype_total_error == 0) error_string = success
     write(*, fmt = '(20a)', advance = 'no') ' Basic datatype test'     
     write(*, fmt = '(50x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + basic_datatype_total_error 

     error_string = failure
     CALL compoundtest(total_error_compoundtest)
     IF (total_error_compoundtest == 0) error_string = success
     write(*, fmt = '(23a)', advance = 'no') ' Compound datatype test'     
     write(*, fmt = '(47x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + total_error_compoundtest

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing PROPERTY interface               ' 
!     write(*,*) '========================================='

     error_string = failure
     CALL external_test(external_total_error)
     IF (external_total_error == 0) error_string = success
     write(*, fmt = '(22a)', advance = 'no') ' External dataset test'     
     write(*, fmt = '(48x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + external_total_error 
    
!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing ATTRIBUTE interface              ' 
!     write(*,*) '========================================='

     error_string = failure
     CALL attribute_test(attribute_total_error)
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
     CALL identifier_test(identifier_total_error)
     IF (identifier_total_error == 0) error_string = success
     write(*, fmt = '(16a)', advance = 'no') ' Identifier test'     
     write(*, fmt = '(54x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + identifier_total_error 

!     write(*,*)
!     write(*,*) '========================================='
!     write(*,*) 'Testing GROUP interface             '
!     write(*,*) '========================================='

     error_string = failure
     CALL group_test(group_total_error)
     IF (group_total_error == 0) error_string = success
     write(*, fmt = '(11a)', advance = 'no') ' Group test'     
     write(*, fmt = '(59x,a)', advance = 'no')  ' '
     write(*, fmt = e_format) error_string
     total_error = total_error + identifier_total_error 

     write(*,*)

     write(*,*) '                  ============================================  '
     write(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
     write(*, fmt = '(i4)', advance='NO') total_error
     write(*, fmt = '(12a)' ) ' error(s) ! '
     write(*,*) '                  ============================================  '

     CALL h5close_f(error)

    END PROGRAM fortranlibtest


