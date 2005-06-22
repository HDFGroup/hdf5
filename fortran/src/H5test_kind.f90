! H5test_kind.f90
!
! This fortran program generates H5fortran_detect.f90
!
         program test_kind
         integer :: i, j, ii, last, kind_numbers(10)
         last = -1
         ii = 0
         j = selected_int_kind(18)
!         write(*,*) j
         do i = 1,100
            j = selected_int_kind(i)
            if(j .ne. last) then
              if(last .ne. -1) then
                  ii = ii + 1
                  kind_numbers(ii) = last
              endif
            last = j
            if(j .eq. -1) exit
            endif
          enddo
!          write(*,*) kind_numbers(1:ii)
! Generate a program
          write(*,*) "program int_kind"
          write(*,*) "write(*,*) "" /*generating header file*/ """
             j = 0
             write(*, "("" call i"", i2.2,""()"")") j
          do i = 1, ii
             j = kind_numbers(i)
             write(*, "("" call i"", i2.2,""()"")") j
          enddo
          write(*,*) "end program int_kind"
              j = 0
             write(*, "("" subroutine i"" i2.2,""()"")") j
             write(*,*)"   implicit none"
             write(*,*)"   integer :: a"
             write(*,*)"   integer :: a_size"
             write(*,*)"   a_size = bit_size(a)"
             write(*,*)"   if (a_size .eq. 8) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_1"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 16) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_2"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 32) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_4"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 64) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_8"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 128) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_NATIVE_16"" "
             write(*,*)"   endif"
             write(*,*)"   return"
             write(*,*)"   end subroutine"    
          do i = 1, ii
              j = kind_numbers(i)
             write(*, "("" subroutine i"" i2.2,""()"")") j
             write(*,*)"   implicit none"
             write(*,*)"   integer(",j,") :: a"
             write(*,*)"   integer :: a_size"
             write(*,*)"   a_size = bit_size(a)"
             write(*,*)"   if (a_size .eq. 8) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_1"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 16) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_2"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 32) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_4"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 64) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_8"" "
             write(*,*)"   endif"
             write(*,*)"   if (a_size .eq. 128) then"
             write(*,*)"       write(*,*) ""#define H5_FORTRAN_HAS_INTEGER_16"" "
             write(*,*)"   endif"
             write(*,*)"   return"
             write(*,*)"   end subroutine"    
          enddo
          end program
              
            

