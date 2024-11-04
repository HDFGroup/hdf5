#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##

noinst_PROGRAMS = h5ex_lite1 h5ex_lite2 h5ex_lite3 h5ex_ptExampleFL \
            h5ex_image1 h5ex_image2 \
            h5ex_table_01 h5ex_table_02 h5ex_table_03 h5ex_table_04 \
            h5ex_table_05 h5ex_table_06 h5ex_table_07 h5ex_table_08 \
            h5ex_table_09 h5ex_table_10 h5ex_table_11 h5ex_table_12 \
            h5ex_ds1

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

TESTS = test.sh

CLEANFILES = Makefile test.sh
