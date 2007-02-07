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


!//////////////////////////////////////////////////////////
! writes/reads dataset by hyperslabs 
!//////////////////////////////////////////////////////////

subroutine multiple_dset_write(lenght,do_collective,do_chunk,nerrors)
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
integer(hid_t) :: driver_id                       ! low-level file driver identifier
integer        :: istart                          ! start position in array 
integer        :: iend                            ! end position in array
integer        :: icount                          ! number of elements in array
character(len=80) :: filename                     ! filename
character(len=80) :: dsetname                     ! dataset name
integer        :: n, i

call mpi_comm_rank( MPI_COMM_WORLD, mpi_rank, mpierror )
call mpi_comm_size( MPI_COMM_WORLD, mpi_size, mpierror )

!//////////////////////////////////////////////////////////
! initialize the array data between the processes (3)
! for the 12 size array we get
! p0 = 1,2,3,4
! p1 = 5,6,7,8
! p2 = 9,10,11,12
!//////////////////////////////////////////////////////////

allocate(wbuf(0:lenght-1),stat=hdferror)
if (hdferror /= 0) then
 write(*,*) 'allocate error'
 return
endif

allocate(rbuf(0:lenght-1),stat=hdferror)
if (hdferror /= 0) then
 write(*,*) 'allocate error'
 return
endif

icount  = lenght/mpi_size     ! divide the array by the number of processes
istart  = mpi_rank*icount     ! start position
iend    = istart + icount     ! end position

!//////////////////////////////////////////////////////////
! HDF5 I/O
!//////////////////////////////////////////////////////////

dims(1)  = lenght
cdims(1) = lenght/mpi_size     ! define chunks as the number of processes

!//////////////////////////////////////////////////////////
! setup file access property list with parallel I/O access
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
call check("h5pset_fapl_mpio_f", hdferror, nerrors)

call h5pget_driver_f(fapl_id, driver_id, hdferror)
call check("h5pget_driver_f", hdferror, nerrors)
    
if( driver_id /= H5FD_MPIO_F) then
 write(*,*) "Wrong driver information returned"
 nerrors = nerrors + 1
endif

!//////////////////////////////////////////////////////////
! create the file collectively
!//////////////////////////////////////////////////////////

call h5_fixname_f("parf2", filename, fapl_id, hdferror)

call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)
call check("h5fcreate_f", hdferror, nerrors)

call h5screate_simple_f(1, dims, fspace_id, hdferror)
call check("h5screate_simple_f", hdferror, nerrors)

call h5screate_simple_f(1, dims, mspace_id, hdferror)
call check("h5screate_simple_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! modify dataset creation properties to enable chunking
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

if (do_chunk) then
 call h5pset_chunk_f(dcpl_id, 1, cdims, hdferror)
 call check("h5pset_chunk_f", hdferror, nerrors)
endif

!//////////////////////////////////////////////////////////
! create a property list for collective dataset write
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)
 
if (do_collective) then
 call h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)
 call check("h5pset_dxpl_mpio_f", hdferror, nerrors)
endif

!//////////////////////////////////////////////////////////
! define hyperslab 
!//////////////////////////////////////////////////////////

counti(1) = icount           
start(1)  = istart   

!//////////////////////////////////////////////////////////
! select hyperslab in memory
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
call check("h5sselect_hyperslab_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! select hyperslab in the file
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
call check("h5sselect_hyperslab_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! create and write the datasets
!//////////////////////////////////////////////////////////

do n = 1, 300

! direct the output of the write statement to unit "dsetname"
 write(dsetname,*) 'dataset', n

! create this dataset
 call h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, fspace_id, dset_id, hdferror, dcpl_id)
 call check("h5dcreate_f", hdferror, nerrors)

 do i = istart, iend-1
  wbuf(i) = n + mpi_rank
 enddo

! write this dataset
 call h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,wbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
 call check("h5dwrite_f", hdferror, nerrors)

! close this dataset
 call h5dclose_f(dset_id, hdferror)
 call check("h5dclose_f", hdferror, nerrors)

enddo

!//////////////////////////////////////////////////////////
! close HDF5 I/O
!//////////////////////////////////////////////////////////

call h5pclose_f(fapl_id, hdferror)
call check("h5pclose_f", hdferror, nerrors)

call h5pclose_f(dcpl_id, hdferror)
call check("h5pclose_f", hdferror, nerrors)

call h5pclose_f(dxpl_id, hdferror)
call check("h5pclose_f", hdferror, nerrors)

call h5sclose_f(mspace_id, hdferror)
call check("h5sclose_f", hdferror, nerrors)

call h5sclose_f(fspace_id, hdferror)
call check("h5sclose_f", hdferror, nerrors)

call h5fclose_f(file_id, hdferror)
call check("h5fclose_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! reopen file with read access
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id)
call check("h5pcreate_f", hdferror, nerrors)

call h5screate_simple_f(1, dims, fspace_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5screate_simple_f(1, dims, mspace_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! select hyperslab in memory
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! select hyperslab in the file
!//////////////////////////////////////////////////////////

call h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

!//////////////////////////////////////////////////////////
! create a property list for collective dataset read
!//////////////////////////////////////////////////////////

call h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)
 
if (do_collective) then
 call h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)
 call check("h5pcreate_f", hdferror, nerrors)
endif

!//////////////////////////////////////////////////////////
! read dataset
!//////////////////////////////////////////////////////////

do n = 1, 300

! direct the output of the write statement to unit "dsetname"
 write(dsetname,*) 'dataset', n

! create this dataset
 call h5dopen_f(file_id, dsetname, dset_id, hdferror)
 call check("h5pcreate_f", hdferror, nerrors)

! read this dataset
 call h5dread_f(dset_id,H5T_NATIVE_INTEGER,rbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
 call check("h5pcreate_f", hdferror, nerrors)

! close this dataset
 call h5dclose_f(dset_id, hdferror)
 call check("h5dclose_f", hdferror, nerrors)

 do i = istart, iend-1
  wbuf(i) = n + mpi_rank
 enddo

! compare read and write data. each process compares a subset of the array
 do i = istart, iend-1
  if( wbuf(i) /= rbuf(i)) then
  write(*,*) 'buffers differs at ', i, rbuf(i), wbuf(i)
  nerrors = nerrors + 1
  endif
 enddo

enddo


!//////////////////////////////////////////////////////////
! close HDF5 I/O
!//////////////////////////////////////////////////////////

call h5pclose_f(fapl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5pclose_f(dxpl_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5sclose_f(fspace_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5sclose_f(mspace_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)

call h5fclose_f(file_id, hdferror)
call check("h5pcreate_f", hdferror, nerrors)


deallocate(wbuf)
deallocate(rbuf)

end subroutine multiple_dset_write

