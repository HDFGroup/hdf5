#
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 Library Examples Makefile(.in)
#

include $(top_srcdir)/config/commence.am

if BUILD_PARALLEL_CONDITIONAL
  TEST_PROG_PARA =
endif

# Example directory
EXAMPLEDIR=$(docdir)/hdf5/examples/hl/c

# Example programs.
# Don't tell automake about them, because if it knew they were programs,
# it would try to compile them instead of using the h5cc script.
# Use the boilerplate in config/examples.am instead.
TEST_PROG = ex_lite1 ex_lite2 ex_lite3 ptExampleFL ptExampleVL \
            ex_image1 ex_image2 \
            ex_table_01 ex_table_02 ex_table_03 ex_table_04 \
            ex_table_05 ex_table_06 ex_table_07 ex_table_08 \
            ex_table_09 ex_table_10 ex_table_11 ex_table_12 \
            ex_ds1

# Install files
# List all file that should be installed in examples directory
INSTALL_FILES = ptExampleFL.c ptExampleVL.c

# Additional dependencies for each program are listed below.
$(EXTRA_PROG): $(LIBHDF5) $(LIBH5_HL)
if BUILD_PARALLEL_CONDITIONAL
	$(H5CC_PP) $(CFLAGS) -o $@ $(srcdir)/$@.c;
else
	$(H5CC) $(CFLAGS) -o $@ $(srcdir)/$@.c;
endif

# ex_lite2 uses a file created by ex_lite1
ex_lite2.chkexe_: ex_lite1.chkexe_

# List dependencies for each program.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C compiler, not h5cc.  This is
# an inelegant way of solving the problem.
# All programs share the same build rule and a dependency on the main hdf5
# library above.
ex_lite1: $(srcdir)/ex_lite1.c
ex_lite2: $(srcdir)/ex_lite2.c
ex_lite3: $(srcdir)/ex_lite3.c
ptExampleFL: $(srcdir)/ptExampleFL.c
ptExampleVL: $(srcdir)/ptExampleVL.c
ex_image1: $(srcdir)/ex_image1.c
ex_image2: $(srcdir)/ex_image2.c
ex_table01: $(srcdir)/ex_table01.c
ex_table02: $(srcdir)/ex_table02.c
ex_table03: $(srcdir)/ex_table03.c
ex_table04: $(srcdir)/ex_table04.c
ex_table05: $(srcdir)/ex_table05.c
ex_table06: $(srcdir)/ex_table06.c
ex_table07: $(srcdir)/ex_table07.c
ex_table08: $(srcdir)/ex_table08.c
ex_table09: $(srcdir)/ex_table09.c
ex_table10: $(srcdir)/ex_table10.c
ex_table11: $(srcdir)/ex_table11.c
ex_table12: $(srcdir)/ex_table12.c

include $(top_srcdir)/config/examples.am
include $(top_srcdir)/config/conclude.am
