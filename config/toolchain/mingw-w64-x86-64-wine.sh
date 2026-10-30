#!/bin/sh

set -e

# assume gfortran-mingw-w64-x86-64-win32 is installed
mingw_prefix=${TOOLCHAIN_PREFIX:-x86_64-w64-mingw32}
libgfortran_path=`ls /usr/lib/gcc/${mingw_prefix}/*/libgfortran.dll.a 2>/dev/null | head -1`
if test -n "${libgfortran_path}"; then
  export WINEPATH=`dirname ${libgfortran_path}`
fi
# Wine 10 (e.g. Ubuntu 26.04) no longer ships /usr/lib/wine/wine64
if test -x /usr/lib/wine/wine64; then
  /usr/lib/wine/wine64 "$@"
else
  wine "$@"
fi
