! This file is part of HDF5. The full HDF5 copyright notice and license
! terms are in the LICENSE file at the root of the source distribution.
!
! Read hdf5.mod through each library target without adding MPI include paths.
PROGRAM mpi_include
  USE HDF5
  IMPLICIT NONE

  INTEGER :: error
  INTEGER(HID_T) :: fapl_id = 0

  ! Resolve the MPI-dependent interface without making an MPI call at runtime.
  IF (.FALSE.) CALL h5pset_fapl_mpio_f(fapl_id, 0, 0, error)
  CALL h5open_f(error)
  IF (error < 0) STOP 1
  CALL h5close_f(error)
  IF (error < 0) STOP 1
END PROGRAM mpi_include
