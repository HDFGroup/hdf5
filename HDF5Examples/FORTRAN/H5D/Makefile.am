#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
noinst_PROGRAMS = \
  h5ex_d_alloc \
  h5ex_d_checksum \
  h5ex_d_chunk \
  h5ex_d_compact \
  h5ex_d_extern  \
  h5ex_d_fillval \
  h5ex_d_gzip \
  h5ex_d_hyper \
  h5ex_d_nbit \
  h5ex_d_rdwr \
  h5ex_d_soint \
  h5ex_d_szip \
  h5ex_d_transform \
  h5ex_d_unlimmod

h5ex_d_alloc_SOURCES     = h5ex_d_alloc.F90
h5ex_d_checksum_SOURCES  = h5ex_d_checksum.F90
h5ex_d_chunk_SOURCES     = h5ex_d_chunk.F90 
h5ex_d_compact_SOURCES   = h5ex_d_compact.F90
h5ex_d_extern_SOURCES    = h5ex_d_extern.F90   
h5ex_d_fillval_SOURCES   = h5ex_d_fillval.F90
h5ex_d_gzip_SOURCES      = h5ex_d_gzip.F90
h5ex_d_hyper_SOURCES     = h5ex_d_hyper.F90
h5ex_d_nbit_SOURCES      = h5ex_d_nbit.F90
h5ex_d_rdwr_SOURCES      = h5ex_d_rdwr.F90
h5ex_d_soint_SOURCES     = h5ex_d_soint.F90
h5ex_d_szip_SOURCES      = h5ex_d_szip.F90
h5ex_d_transform_SOURCES = h5ex_d_transform.F90
h5ex_d_unlimmod_SOURCES  = h5ex_d_unlimmod.F90

if FORTRAN_2003_CONDITIONAL_F
  noinst_PROGRAMS +=  h5ex_d_rdwr_kind
  h5ex_d_rdwr_kind_SOURCES = h5ex_d_rdwr_kind.F90
endif


EXTRA_DIST = tfiles/h5ex_d_rdwr.tst tfiles/h5ex_d_hyper.tst tfiles/h5ex_d_chunk.tst \
tfiles/h5ex_d_gzip.tst tfiles/h5ex_d_extern.tst tfiles/h5ex_d_compact.tst \
tfiles/h5ex_d_unlimadd.tst \
tfiles/h5ex_d_checksum.tst tfiles/h5ex_d_transform.tst \
tfiles/h5ex_d_fillval.tst tfiles/h5ex_d_alloc.tst \
tfiles/h5ex_d_rdwr.ddl tfiles/h5ex_d_hyper.ddl tfiles/h5ex_d_chunk.ddl \
tfiles/h5ex_d_gzip.ddl tfiles/h5ex_d_extern.ddl tfiles/h5ex_d_compact.ddl \
tfiles/h5ex_d_unlimadd.ddl \
tfiles/h5ex_d_checksum.ddl tfiles/h5ex_d_transform.ddl \
tfiles/h5ex_d_soint.tst tfiles/h5ex_d_soint.dll \
tfiles/h5ex_d_szip.tst tfiles/h5ex_d_szip.dll \
tfiles/h5ex_d_nbit.tst tfiles/h5ex_d_nbit.dll \
tfiles/h5ex_d_unlimmod.tst tfiles/h5ex_d_unlimmod.dll \
tfiles/h5ex_d_fillval.ddl tfiles/h5ex_d_alloc.ddl \
test.sh

if FORTRAN_2003_CONDITIONAL_F
  EXTRA_DIST += tfiles/h5ex_d_rdwr_kind.tst tfiles/h5ex_d_rdwr_kind.dll
endif

TESTS = test.sh

CLEANFILES = Makefile test.sh
