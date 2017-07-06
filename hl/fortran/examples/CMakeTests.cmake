#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.


##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

foreach (example ${examples})
  add_test (NAME HL_FORTRAN_f90_ex_${example} COMMAND $<TARGET_FILE:hl_f90_ex_${example}>)
endforeach ()
