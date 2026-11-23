#!/bin/bash

# Check that an extracted HDF5 Linux binary works away from the machine it was
# built on: compile and run a program with each compiler wrapper, and with
# plain pkg-config. Run by the "AlmaLinux 8 gcc (glibc 2.28)" and "Ubuntu
# Intel" jobs in .github/workflows/ctest.yml.
#
# The binary is configured to record each wrapper's compiler by name (for
# example gcc or icx) rather than by the absolute path CMake finds, so the
# wrappers run the compiler on PATH. Check that no absolute path is
# recorded, that the HDF5_CC-style overrides work, and that a missing
# compiler is reported.
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

unset HDF5_CC HDF5_PCC HDF5_CXX HDF5_FC HDF5_PFC

check "h5cc"  c_test   "${ROOT}/bin/h5cc"  c_test.c -o c_test
check "h5c++" cpp_test "${ROOT}/bin/h5c++" cpp_test.cpp -o cpp_test
check "h5fc"  f_test   "${ROOT}/bin/h5fc"  f_test.f90 -o f_test

# A recorded absolute path would only exist on the build machine
for w in h5cc h5c++ h5fc; do
  # shellcheck disable=SC2016 # the $ is literal
  recorded=$(sed -n 's/^compiler=\${HDF5_[A-Z]*:-"\(.*\)"}$/\1/p' "${ROOT}/bin/${w}")
  echo "=== ${w} records the compiler by name ==="
  case ${recorded} in
    ("") echo "::error::FAILED: cannot find the compiler recorded in ${w}"
         nerrors=$((nerrors + 1));;
    (/*) echo "::error::FAILED: ${w} records the absolute path ${recorded}"
         nerrors=$((nerrors + 1));;
    (*)  echo "PASSED: ${w} records ${recorded}";;
  esac
done

# The environment variables override the recorded compiler. Only h5cc
# compiles with a substitute, since another C++ or Fortran compiler may not
# read the library's headers or .mod files; for h5c++ and h5fc, check what
# they would run.
check "HDF5_CC=cc h5cc" c_test env HDF5_CC=cc "${ROOT}/bin/h5cc" c_test.c -o c_test
for pair in h5c++:HDF5_CXX h5fc:HDF5_FC; do
  w=${pair%%:*} var=${pair#*:}
  echo "=== ${var} ${w} -show ==="
  if [ "$(env "${var}=my-compiler" "${ROOT}/bin/${w}" -show x | awk '{print $1}')" = my-compiler ]; then
    echo "PASSED: ${var} ${w} -show"
  else
    echo "::error::FAILED: ${var} ${w} -show"
    nerrors=$((nerrors + 1))
  fi
done

# A compiler that cannot be found is reported with the variable to set
echo "=== h5cc, missing compiler ==="
if HDF5_CC=/nonexistent/cc "${ROOT}/bin/h5cc" c_test.c -o c_test 2> missing.err; then
  echo "::error::FAILED: h5cc, missing compiler: compile succeeded"
  nerrors=$((nerrors + 1))
elif grep -q "set HDF5_CC to the compiler h5cc should run" missing.err; then
  echo "PASSED: h5cc, missing compiler"
else
  cat missing.err
  echo "::error::FAILED: h5cc, missing compiler: no message naming HDF5_CC"
  nerrors=$((nerrors + 1))
fi

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
