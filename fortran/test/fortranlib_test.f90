!
! 
!    Testing Fortran functionality.
!
     PROGRAM fortranlibtest

       !USE H5FTEST
       !USE H5DTEST
       !USE H5RTEST
       !USE H5STEST
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

     CALL h5init_types_f(error) 
     write(*,*)
     write(*,*) "Testing File Interface"

     CALL mountingtest(mounting_total_error)
     IF (mounting_total_error == 0) THEN
         write(*,*) "mounting test OK"
     ELSE
         write(*,*) "mounting test FAILED"
     END IF
     total_error = total_error + mounting_total_error 

     CALL reopentest(reopen_total_error)
     IF (reopen_total_error == 0) THEN
         write(*,*) "Reopen test OK"
     ELSE
         write(*,*) "Reopen test FAILED"
     END IF
     total_error = total_error + reopen_total_error 



     write(*,*)
     write(*,*) "Testing Dataset Interface"

     CALL datasettest(dataset_total_error)
     IF (dataset_total_error == 0) THEN
         write(*,*) "dataset test OK"
     ELSE
         write(*,*) "dataset test FAILED"
     END IF
     total_error = total_error + dataset_total_error 

     CALL extenddsettest(extend_dataset_total_error)
     IF (extend_dataset_total_error == 0) THEN
         write(*,*) "extend dataset test OK"
     ELSE
         write(*,*) "extend dataset test FAILED"
     END IF
     total_error = total_error + extend_dataset_total_error 

     write(*,*)
     write(*,*) "Testing DATASPACE Interface"

     CALL dataspace_basic_test(dataspace_total_error)
     IF (dataspace_total_error == 0) THEN
         write(*,*) "dataspce basic test OK"
     ELSE
         write(*,*) "dataspace basic test FAILED"
     END IF
     total_error = total_error + dataspace_total_error 


     write(*,*)
     write(*,*) "Testing Reference Interface"

     CALL refobjtest(refobj_total_error)
     IF (refobj_total_error == 0) THEN
         write(*,*) "Reference to object test OK"
     ELSE
         write(*,*) "Reference to object test FAILED"
     END IF
     total_error = total_error + refobj_total_error 

     CALL refregtest(refreg_total_error)
     IF (refreg_total_error == 0) THEN
         write(*,*) "Refernce to Region test OK"
     ELSE
         write(*,*) "Refernce to Region test FAILED"
     END IF
     total_error = total_error + refreg_total_error 

     write(*,*)
     write(*,*) "Testing selection functionalities"

     CALL  test_select_hyperslab( hyperslab_total_error)
     IF ( hyperslab_total_error == 0) THEN
         write(*,*) "hyperslab selection test OK"
     ELSE
         write(*,*) "hyperslab selection test FAILED"
     END IF
     total_error = total_error + hyperslab_total_error 

     CALL test_select_element(element_total_error)
     IF (element_total_error == 0) THEN
         write(*,*) "element selection test OK"
     ELSE
         write(*,*) "element selection test FAILED"
     END IF
     total_error = total_error + element_total_error 

     CALL test_basic_select(basic_select_total_error)
     IF (basic_select_total_error == 0) THEN
         write(*,*) "basic selection test OK"
     ELSE
         write(*,*) "basic selection test FAILED"
     END IF
     total_error = total_error + basic_select_total_error 
     write(*,*)

     write(*,*) "Testing Compound Datatypes"
     CALL compoundtest(total_error_compoundtest)
     IF (total_error_compoundtest == 0) THEN
         write(*,*) "Compound Datatype test OK"
     ELSE
         write(*,*) "Compound Datatype test FAILED"
     END IF
     total_error = total_error + total_error_compoundtest

     write(*,*)
     write(*,*) "Testing basic datatype functionalities"
     CALL basic_data_type_test(basic_datatype_total_error)
     IF (basic_datatype_total_error == 0) THEN
         write(*,*) "Basic Datatype test OK"
     ELSE
         write(*,*) "Basic Datatype test FAILED"
     END IF
     total_error = total_error + basic_datatype_total_error 

     write(*,*)
     write(*,*) "Testing external functionalities"
     CALL external_test(external_total_error)
     IF (external_total_error == 0) THEN
         write(*,*) "External test OK"
     ELSE
         write(*,*) "External test FAILED"
     END IF
     total_error = total_error + external_total_error 

     write(*,*)

     if (total_error .eq. 0) write(*,*) "Fortran_lib test passed!"
     if (total_error.gt. 0) write(*,*) "Fortran_lib test failed with ",&
                                           total_error, " error(s)"

     CALL h5close_types_f(error)

    END PROGRAM fortranlibtest


