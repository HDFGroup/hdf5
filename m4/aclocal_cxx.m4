dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
dnl
dnl Copyright by the Board of Trustees of the University of Illinois.
dnl All rights reserved.
dnl
dnl This file is part of HDF5.  The full HDF5 copyright notice, including
dnl terms governing use, modification, and redistribution, is contained in
dnl the files COPYING and Copyright.html.  COPYING can be found at the root
dnl of the source code distribution tree; Copyright.html can be found at the
dnl root level of an installed copy of the electronic HDF5 document set and
dnl is linked from the top-level documents page.  It can also be found at
dnl http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
dnl access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------

dnl *********************************
dnl PURPOSE
dnl  Contains Macros for HDF5 C++
dnl *********************************
dnl
dnl Special characteristics that have no autoconf counterpart but that
dnl we need as part of the C++ support.  To distinquish these, they
dnl have a [PAC] prefix.

dnl Checking if C++ needs old style header files in includes
AC_DEFUN([PAC_PROG_CXX_HEADERS],[
  AC_MSG_CHECKING([if $CXX needs old style header files in includes])
  TEST_SRC="`(echo \"#define OLD_HEADER_FILENAME 1\"; cat $srcdir/config/cmake_ext_mod/HDFCXXTests.cpp)`"

  AC_LINK_IFELSE([AC_LANG_SOURCE([$TEST_SRC])],
    [AC_MSG_RESULT([no])],
    [AC_MSG_RESULT([yes])
    CXXFLAGS="${CXXFLAGS} -DOLD_HEADER_FILENAME"
    AM_CXXFLAGS="${AM_CXXFLAGS} -DOLD_HEADER_FILENAME"])
])

dnl Checking if ++ can handle namespaces
AC_DEFUN([PAC_PROG_CXX_NAMESPACE],[
  AC_MSG_CHECKING([if $CXX can handle namespaces])
  TEST_SRC="`(echo \"#define HDF_NO_NAMESPACE 1\"; cat $srcdir/config/cmake_ext_mod/HDFCXXTests.cpp)`"

  AC_LINK_IFELSE([AC_LANG_SOURCE([$TEST_SRC])], [AC_MSG_RESULT([yes])],
     [AC_MSG_RESULT([no])
     CXXFLAGS="${CXXFLAGS} -DHDF_NO_NAMESPACE"
     AM_CXXFLAGS="${AM_CXXFLAGS} -DHDF_NO_NAMESPACE"])
])

dnl Checking if C++ supports std
AC_DEFUN([PAC_PROG_CXX_STD],[
  AC_MSG_CHECKING([if $CXX supports std])
  TEST_SRC="`(echo \"#define HDF_NO_STD 1\"; cat $srcdir/config/cmake_ext_mod/HDFCXXTests.cpp)`"

  AC_LINK_IFELSE([AC_LANG_SOURCE([$TEST_SRC])], [AC_MSG_RESULT([yes])],
     [AC_MSG_RESULT([no])
     CXXFLAGS="${CXXFLAGS} -DH5_NO_STD"
     AM_CXXFLAGS="${AM_CXXFLAGS} -DH5_NO_STD"])
])

dnl Checking if C++ has offsetof extension
AC_DEFUN([PAC_PROG_CXX_OFFSETOF],[
  AC_MSG_CHECKING([if $CXX has offsetof extension])
  TEST_SRC="`(echo \"#define CXX_HAVE_OFFSETOF 1\"; cat $srcdir/config/cmake_ext_mod/HDFCXXTests.cpp)`"

  AC_LINK_IFELSE([AC_LANG_SOURCE([$TEST_SRC])],[AC_MSG_RESULT([yes])
    AC_DEFINE([CXX_HAVE_OFFSETOF], [1], [Define if C++ compiler recognizes offsetof])],
    AC_MSG_RESULT([no]))
])

dnl Checking if C++ can handle static cast
AC_DEFUN([PAC_PROG_CXX_STATIC_CAST],[
  AC_MSG_CHECKING([if $CXX can handle static cast])
  TEST_SRC="`(echo \"#define NO_STATIC_CAST 1\"; cat $srcdir/config/cmake_ext_mod/HDFCXXTests.cpp)`"

  AC_LINK_IFELSE([AC_LANG_SOURCE([$TEST_SRC])], [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
    CXXFLAGS="${CXXFLAGS} -DNO_STATIC_CAST"
    AM_CXXFLAGS="${AM_CXXFLAGS} -DNO_STATIC_CAST"])
])
