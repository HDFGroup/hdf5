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
dnl Macros for HDF5 Fortran
dnl
dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------

dnl -------------------------------------------------------------------------
dnl _AC_SYS_LARGEFILE_MACRO_VALUE
dnl
dnl The following macro overrides the autoconf macro of the same name
dnl with this custom definition. This macro performs the same checks as 
dnl autoconf's native _AC_SYS_LARGEFILE_MACRO_VALUE, but will also set
dnl AM_CPPFLAGS with the appropriate -D defines so additional configure 
dnl sizeof checks do not fail.
dnl
# _AC_SYS_LARGEFILE_MACRO_VALUE(C-MACRO, VALUE,
#               CACHE-VAR,
#               DESCRIPTION,
#               PROLOGUE, [FUNCTION-BODY])
# ----------------------------------------------------------
m4_define([_AC_SYS_LARGEFILE_MACRO_VALUE],
[AC_CACHE_CHECK([for $1 value needed for large files], [$3],
[while :; do
  m4_ifval([$6], [AC_LINK_IFELSE], [AC_COMPILE_IFELSE])(
    [AC_LANG_PROGRAM([$5], [$6])],
    [$3=no; break])
  m4_ifval([$6], [AC_LINK_IFELSE], [AC_COMPILE_IFELSE])(
    [AC_LANG_PROGRAM([@%:@define $1 $2
$5], [$6])],
    [$3=$2; break])
  $3=unknown
  break
done])
case $$3 in #(
  no | unknown) ;;
  *) AC_DEFINE_UNQUOTED([$1], [$$3], [$4])
     AM_CPPFLAGS="-D$1=$$3 $AM_CPPFLAGS";;
esac
rm -rf conftest*[]dnl
])# _AC_SYS_LARGEFILE_MACRO_VALUE

