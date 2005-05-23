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

!//////////////////////////////////////////////////////////
! main program for parallel HDF5 Fortran tests
!//////////////////////////////////////////////////////////

program parallel_test
use hdf5
implicit none
include 'mpif.h'

integer :: mpierror                             ! MPI hdferror flag
integer :: hdferror                             ! HDF hdferror flag
logical :: do_collective                        ! use collective MPI I/O
logical :: do_chunk                             ! use chunking
integer :: nerrors = 0                          ! number of errors
integer :: mpi_rank                             ! rank of the calling process in the communicator 
integer :: lenght = 12000                       ! lenght of array

!//////////////////////////////////////////////////////////
! initialize MPI
!//////////////////////////////////////////////////////////

call mpi_init(mpierror)
call mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )

!//////////////////////////////////////////////////////////
! initialize the HDF5 fortran interface
!//////////////////////////////////////////////////////////

call h5open_f(hdferror)

!//////////////////////////////////////////////////////////
! test write/read dataset by hyperslabs with independent MPI I/O
!//////////////////////////////////////////////////////////

if (mpi_rank == 0) write(*,*) 'Writing/reading dataset by hyperslabs (contiguous layout, independent MPI I/O)'

do_collective = .false.
do_chunk      = .false.
call hyper(lenght,do_collective,do_chunk,nerrors)

!//////////////////////////////////////////////////////////
! test write/read dataset by hyperslabs with collective MPI I/O
!//////////////////////////////////////////////////////////

if (mpi_rank == 0) write(*,*) 'Writing/reading dataset by hyperslabs (contiguous layout, collective MPI I/O)'

do_collective = .true.
do_chunk      = .false.
call hyper(lenght,do_collective,do_chunk,nerrors)

!//////////////////////////////////////////////////////////
! test write/read dataset by hyperslabs with independent MPI I/O
!//////////////////////////////////////////////////////////

if (mpi_rank == 0) write(*,*) 'Writing/reading dataset by hyperslabs (chunk layout, independent MPI I/O)'

do_collective = .false.
do_chunk      = .true.
call hyper(lenght,do_collective,do_chunk,nerrors)

!//////////////////////////////////////////////////////////
! test write/read dataset by hyperslabs with collective MPI I/O
!//////////////////////////////////////////////////////////

if (mpi_rank == 0) write(*,*) 'Writing/reading dataset by hyperslabs (chunk layout, collective MPI I/O)'

do_collective = .true.
do_chunk      = .true.
call hyper(lenght,do_collective,do_chunk,nerrors)

!//////////////////////////////////////////////////////////
! test write/read several datasets (independent MPI I/O)
!//////////////////////////////////////////////////////////

if (mpi_rank == 0) write(*,*) 'Writing/reading several datasets (contiguous layout, independent MPI I/O)'

do_collective = .false.
do_chunk      = .false.
call multiple_dset_write(lenght,do_collective,do_chunk,nerrors)


!//////////////////////////////////////////////////////////
! close HDF5 interface
!//////////////////////////////////////////////////////////

call h5close_f(hdferror)

!//////////////////////////////////////////////////////////
! close MPI
!//////////////////////////////////////////////////////////

if (nerrors == 0) then
 call mpi_finalize(mpierror)
else
 write(*,*) 'Errors detected in process ', mpi_rank
 call mpi_abort(MPI_COMM_WORLD, 1, mpierror)
endif

!//////////////////////////////////////////////////////////
! end main program 
!//////////////////////////////////////////////////////////

end program parallel_test

