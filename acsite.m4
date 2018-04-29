dnl -------------------------------------------------------------------------
dnl -------------------------------------------------------------------------
dnl
dnl Copyright by The HDF Group.
dnl Copyright by the Board of Trustees of the University of Illinois.
dnl All rights reserved.
dnl
dnl This file is part of HDF5.  The full HDF5 copyright notice, including
dnl terms governing use, modification, and redistribution, is contained in
dnl the COPYING file, which can be found at the root of the source code
dnl dnl distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
dnl dnl If you do not have access to either file, you may request a copy from
dnl dnl help@hdfgroup.org.
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

