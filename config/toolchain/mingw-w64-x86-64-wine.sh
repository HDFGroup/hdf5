#!/bin/sh

set -e

# assume gfortran-mingw-w64-x86-64-win32 is installed
mingw_prefix=${TOOLCHAIN_PREFIX:-x86_64-w64-mingw32}
libgfortran_path=`ls /usr/lib/gcc/${mingw_prefix}/*/libgfortran.dll.a 2>/dev/null | head -1`
if test -n "${libgfortran_path}"; then
  export WINEPATH=`dirname ${libgfortran_path}`
fi
/usr/lib/wine/wine64 "$@"
