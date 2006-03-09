$!#
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
$!#
$!
$!
$! This files copies all make files from the VMS directory to the source directories
$!
$ copy [.c__.examples]make.com [-.c__.examples]make.com
$ copy [.c__.src]make.com [-.c__.src]make.com
$ copy [.c__.test]make.com [-.c__.test]make.com
$ copy [.fortran.examples]make.com [-.fortran.examples]make.com
$ copy [.fortran.src]make.com [-.fortran.src]make.com
$ copy [.fortran.test]make.com [-.fortran.test]make.com
$ copy [.src]make.com [-.src]make.com
$ copy [.src]h5pubconf.h [-.src]
$ copy [.test]make.com [-.test]make.com
$ copy [.examples]make.com [-.examples]make.com
$ copy [.tools.h5dump]*.com [-.tools.h5dump]
$ copy [.tools.h5ls]*.com [-.tools.h5ls]
$ copy [.tools.h5diff]*.com [-.tools.h5diff]
$ copy [.tools.lib]make.com [-.tools.lib]

