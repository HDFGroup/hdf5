#!/bin/bash

# Check that an extracted HDF5 Linux binary works away from the machine it was
# built on: compile and run a program with each compiler wrapper, and with
# plain pkg-config. Run by the "AlmaLinux 8 gcc (glibc 2.28)" job in
# .github/workflows/ctest.yml on the runner, outside the build container, so
# a path that only exists in the container makes it fail.
#
# Usage: test-linux-binary.sh <install-root> <work-dir>
#   <install-root>  directory holding bin/, include/ and lib*/ of the binary
#   <work-dir>      empty scratch directory for the test programs

set -euo pipefail

ROOT="${1:?usage: $0 <install-root> <work-dir>}"
WORK="${2:?usage: $0 <install-root> <work-dir>}"

ROOT=$(cd -P -- "${ROOT}" && pwd -P)
mkdir -p "${WORK}"
cd "${WORK}"

libdir=$(find "${ROOT}" -maxdepth 1 -name 'lib*' -type d -exec test -f '{}/pkgconfig/hdf5.pc' ';' -print | sed -n 1p)
if [ -z "${libdir}" ]; then
  echo "::error::no lib*/pkgconfig/hdf5.pc under ${ROOT}"
  exit 1
fi

cat > c_test.c <<'EOF'
#include "hdf5.h"
#include "hdf5_hl.h"
int main(void)
{
    unsigned maj, min, rel;
    hid_t    file;
    int      data[2] = {1, 2};
    hsize_t  dims[1] = {2};

    if (H5get_libversion(&maj, &min, &rel) < 0)
        return 1;
    if ((file = H5Fcreate("c_test.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return 1;
    if (H5LTmake_dataset_int(file, "dset", 1, dims, data) < 0)
        return 1;
    if (H5Fclose(file) < 0)
        return 1;
    return 0;
}
EOF

cat > cpp_test.cpp <<'EOF'
#include "H5Cpp.h"
int main()
{
    H5::H5File file("cpp_test.h5", H5F_ACC_TRUNC);
    file.createGroup("/group");
    return 0;
}
EOF

cat > f_test.f90 <<'EOF'
program f_test
  use hdf5
  implicit none
  integer(hid_t) :: file
  integer :: error
  call h5open_f(error)
  if (error /= 0) error stop 1
  call h5fcreate_f("f_test.h5", H5F_ACC_TRUNC_F, file, error)
  if (error /= 0) error stop 1
  call h5fclose_f(file, error)
  if (error /= 0) error stop 1
  call h5close_f(error)
  if (error /= 0) error stop 1
end program f_test
EOF

nerrors=0

# Build with "$@" and then run the result, reporting the outcome
check() {
  local name=$1 exe=$2
  shift 2
  echo "=== ${name} ==="
  if "$@" && "./${exe}"; then
    echo "PASSED: ${name}"
  else
    echo "::error::FAILED: ${name}"
    nerrors=$((nerrors + 1))
  fi
}

check "h5cc"  c_test   "${ROOT}/bin/h5cc"  c_test.c -o c_test
check "h5c++" cpp_test "${ROOT}/bin/h5c++" cpp_test.cpp -o cpp_test
check "h5fc"  f_test   "${ROOT}/bin/h5fc"  f_test.f90 -o f_test

# The .pc files record the build-time prefix; --define-prefix relocates them
# to where they were extracted, as the binary's README documents
export PKG_CONFIG_PATH="${libdir}/pkgconfig"
pc_flags=$(pkg-config --define-prefix --cflags --libs hdf5_hl hdf5)
echo "pkg-config --define-prefix: ${pc_flags}"
# shellcheck disable=SC2086 # pc_flags is a list of flags
check "pkg-config" pc_test cc c_test.c -o pc_test ${pc_flags} "-Wl,-rpath,${libdir}"

if [ "${nerrors}" -ne 0 ]; then
  echo "${nerrors} binary checks failed"
  exit 1
fi
echo "All binary checks passed"
