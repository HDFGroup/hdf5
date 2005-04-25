 program int_kind
 write(*,*) " /*generating header file*/ "
 call i01()
 call i02()
 call i04()
 call i08()
 end program int_kind
 subroutine i01()
    implicit none
    integer( 1 ) :: a
    integer :: a_size
    a_size = bit_size(a)
    if (a_size .eq. 8) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_1" 
    endif
    if (a_size .eq. 16) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_2" 
    endif
    if (a_size .eq. 32) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_4" 
    endif
    if (a_size .eq. 64) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_8" 
    endif
    if (a_size .eq. 128) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_16" 
    endif
    return
    end subroutine
 subroutine i02()
    implicit none
    integer( 2 ) :: a
    integer :: a_size
    a_size = bit_size(a)
    if (a_size .eq. 8) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_1" 
    endif
    if (a_size .eq. 16) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_2" 
    endif
    if (a_size .eq. 32) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_4" 
    endif
    if (a_size .eq. 64) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_8" 
    endif
    if (a_size .eq. 128) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_16" 
    endif
    return
    end subroutine
 subroutine i04()
    implicit none
    integer( 4 ) :: a
    integer :: a_size
    a_size = bit_size(a)
    if (a_size .eq. 8) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_1" 
    endif
    if (a_size .eq. 16) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_2" 
    endif
    if (a_size .eq. 32) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_4" 
    endif
    if (a_size .eq. 64) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_8" 
    endif
    if (a_size .eq. 128) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_16" 
    endif
    return
    end subroutine
 subroutine i08()
    implicit none
    integer( 8 ) :: a
    integer :: a_size
    a_size = bit_size(a)
    if (a_size .eq. 8) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_1" 
    endif
    if (a_size .eq. 16) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_2" 
    endif
    if (a_size .eq. 32) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_4" 
    endif
    if (a_size .eq. 64) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_8" 
    endif
    if (a_size .eq. 128) then
        write(*,*) "#define H5_FORTRAN_HAS_INTEGER_16" 
    endif
    return
    end subroutine
