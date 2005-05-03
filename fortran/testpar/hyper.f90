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
! writes/reads dataset by hyperslabs 
!//////////////////////////////////////////////////////////

subroutine hyper(lenght,do_collective,do_chunk,nerrors)
use hdf5
implicit none
include 'mpif.h'

integer, intent(in) :: lenght                     ! array lenght
logical, intent(in) :: do_collective              ! use collective I/O
logical, intent(in) :: do_chunk                   ! use chunking
integer, intent(inout) :: nerrors                 ! number of errors
integer :: mpierror                               ! MPI hdferror flag
integer :: hdferror                               ! HDF hdferror flag
integer :: mpi_size                               ! number of processes in the group of communicator 
integer :: mpi_rank                               ! rank of the calling process in the communicator 
integer(hsize_t), dimension(1) :: dims            ! dataset dimensions
integer(hsize_t), dimension(1) :: cdims           ! chunk dimensions
integer, allocatable :: wbuf(:)                   ! write buffer
integer, allocatable :: rbuf(:)                   ! read buffer
integer(hsize_t), dimension(1) :: counti          ! hyperslab selection 
integer(hsize_t), dimension(1) :: start           ! hyperslab selection  
integer(hid_t) :: fapl_id                         ! file access identifier
integer(hid_t) :: dxpl_id                         ! dataset transfer property list 
integer(hid_t) :: dcpl_id                         ! dataset creation property list 
integer(hid_t) :: file_id                         ! file identifier
integer(hid_t) :: dset_id                         ! dataset identifier
integer(hid_t) :: fspace_id                       ! file space identifier
integer(hid_t) :: mspace_id                       ! memory space identifier
integer        :: istart                          ! start position in array 
integer        :: iend                            ! end position in array
integer        :: icount                          ! number of elements in array
integer        :: i


call mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
call mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )

!//////////////////////////////////////////////////////////
! initialize the array data between the processes (3)
! for the 12 size array we get
! p0 = 1,2,3,4
! p1 = 5,6,7,8
! p2 = 9,10,11,12
!//////////////////////////////////////////////////////////

allocate(wbuf(0:lenght-1))
allocate(rbuf(0:lenght-1))

icount  = lenght/mpi_size     ! divide the array by the number of processes
istart  = mpi_rank*icount     ! start position
iend    = istart + icount     ! end position

do i = istart, iend-1
 wbuf(i) = i
enddo

!//////////////////////////////////////////////////////////
! HDF5 I/O
!//////////////////////////////////////////////////////////

dims(1)  = lenght
cdims(1) = lenght/mpi_size     ! define chunks as the number of processes

call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
call h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
call h5fcreate_f("parf.h5", H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)

call h5screate_simple_f(1, dims, fspace_id, hdferror)
call h5screate_simple_f(1, dims, mspace_id, hdferror)

!//////////////////////////////////////////////////////////
! modify dataset creation properties to enable chunking
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, hdferror)
if (do_chunk) call h5pset_chunk_f(dcpl_id, 1, cdims, hdferror)

!//////////////////////////////////////////////////////////
! create the dataset
!//////////////////////////////////////////////////////////

call h5dcreate_f(file_id, "dset", H5T_NATIVE_INTEGER, fspace_id, dset_id, hdferror, dcpl_id)

!//////////////////////////////////////////////////////////
! define hyperslab 
!//////////////////////////////////////////////////////////

counti(1) = icount           
start(1)  = istart   

!//////////////////////////////////////////////////////////
! select hyperslab in memory
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)

!//////////////////////////////////////////////////////////
! select hyperslab in the file
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)

!//////////////////////////////////////////////////////////
! create a property list for collective dataset write
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror) 
if (do_collective) call h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)

!//////////////////////////////////////////////////////////
! write dataset
!//////////////////////////////////////////////////////////

call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, wbuf, dims, hdferror, file_space_id = fspace_id, mem_space_id = mspace_id, xfer_prp = dxpl_id)

!//////////////////////////////////////////////////////////
! close HDF5 I/O
!//////////////////////////////////////////////////////////

call h5sclose_f(fspace_id, hdferror)
call h5dclose_f(dset_id, hdferror)
call h5pclose_f(fapl_id, hdferror)
call h5fclose_f(file_id, hdferror)

!//////////////////////////////////////////////////////////
! reopen file with read access
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
call h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
call h5fopen_f("parf.h5", H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id)

call h5screate_simple_f(1, dims, fspace_id, hdferror)
call h5screate_simple_f(1, dims, mspace_id, hdferror)
call h5dopen_f(file_id, "dset", dset_id, hdferror)

!//////////////////////////////////////////////////////////
! select hyperslab in memory
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)

!//////////////////////////////////////////////////////////
! select hyperslab in the file
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)

!//////////////////////////////////////////////////////////
! create a property list for collective dataset write
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror) 
if (do_collective) call h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)

!//////////////////////////////////////////////////////////
! read dataset
!//////////////////////////////////////////////////////////

call h5dread_f(dset_id, H5T_NATIVE_INTEGER, rbuf, dims, hdferror, file_space_id = fspace_id, mem_space_id = mspace_id, xfer_prp = dxpl_id)

!//////////////////////////////////////////////////////////
! close HDF5 I/O
!//////////////////////////////////////////////////////////

call h5sclose_f(fspace_id, hdferror)
call h5dclose_f(dset_id, hdferror)
call h5pclose_f(fapl_id, hdferror)
call h5fclose_f(file_id, hdferror)


!//////////////////////////////////////////////////////////
! compare read and write data. each process compares a subset of the array
!//////////////////////////////////////////////////////////

do i = istart, iend-1
 if( wbuf(i) /= rbuf(i)) then
 write(*,*) 'buffers differs at ', i, rbuf(i), wbuf(i)
 nerrors = nerrors + 1
 endif
enddo

deallocate(wbuf)
deallocate(rbuf)

end subroutine hyper

