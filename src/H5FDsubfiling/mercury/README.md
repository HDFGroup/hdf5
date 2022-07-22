Mercury
=======
[![Build status][github-ci-svg]][github-ci-link]
[![Latest version][mercury-release-svg]][mercury-release-link]
[![Spack version][spack-release-svg]][spack-release-link]

Mercury is an RPC framework specifically designed for use in HPC systems
that allows asynchronous transfer of parameters and execution requests,
as well as direct support of large data arguments. The network implementation
is abstracted, allowing easy porting to future systems and efficient use
of existing native transport mechanisms. Mercury's interface is generic
and allows any function call to be serialized.
Mercury is a core component of the [Mochi][mochi-link] ecosystem of
microservices.

Please see the accompanying LICENSE.txt file for license details.

Contributions and patches are welcomed but require a Contributor License
Agreement (CLA) to be filled out. Please contact us if you are interested
in contributing to Mercury by subscribing to the
[mailing lists][mailing-lists].

Architectures supported
=======================

Architectures supported by MPI implementations are generally supported by the
network abstraction layer.

The OFI libfabric plugin as well as the SM plugin
are stable and provide the best performance in most workloads. Libfabric
providers currently supported are: `tcp`, `verbs`, `psm2`, `gni`.

The UCX plugin is also available as an alternative transport on platforms
for which libfabric is either not available or not recommended to use,
currently supported protocols are tcp and verbs.

MPI and BMI (tcp) plugins are still supported but gradually being moved as
deprecated, therefore should only be used as fallback methods.
The CCI plugin is deprecated and no longer supported.

See the [plugin requirements](#plugin-requirements) section for
plugin requirement details.

Documentation
=============

Please see the documentation available on the mercury [website][documentation]
for a quick introduction to Mercury.

Software requirements
=====================

Compiling and running Mercury requires up-to-date versions of various
software packages. Beware that using excessively old versions of these
packages can cause indirect errors that are very difficult to track down.

Plugin requirements
-------------------

To make use of the OFI libfabric plugin, please refer to the libfabric build
instructions available on this [page][libfabric].

To make use of the UCX plugin, please refer to the UCX build
instructions available on this [page][ucx].

To make use of the native NA SM (shared-memory) plugin on Linux,
the cross-memory attach (CMA) feature introduced in kernel v3.2 is required.
The yama security module must also be configured to allow remote process memory
to be accessed (see this [page][yama]). On MacOS, code signing with inclusion of
the na_sm.plist file into the binary is currently required to allow process
memory to be accessed.

To make use of the BMI plugin, the most convenient way is to install it through
spack or one can also do:

    git clone https://github.com/radix-io/bmi.git && cd bmi
    ./prepare && ./configure --enable-shared --enable-bmi-only
    make && make install

To make use of the MPI plugin, Mercury requires a _well-configured_ MPI
implementation (MPICH2 v1.4.1 or higher / OpenMPI v1.6 or higher) with
`MPI_THREAD_MULTIPLE` available on targets that will accept remote
connections. Processes that are _not_ accepting incoming connections are
_not_ required to have a multithreaded level of execution.

Optional requirements
---------------------

For optional automatic code generation features (which are used for generating
serialization and deserialization routines), the preprocessor subset of the
BOOST library must be included (Boost v1.48 or higher is recommended).
The library itself is therefore not necessary since only the header is used.
Mercury includes those headers if one does not have BOOST installed and
wants to make use of this feature.

Building
========

If you install the full sources, put the tarball in a directory where you
have permissions (e.g., your home directory) and unpack it:

    bzip2 -dc mercury-X.tar.bz2 | tar xvf -

Replace `'X'` with the version number of the package.

(Optional) If you checked out the sources using git (without the `--recursive`
option) and want to build the testing suite (which requires the kwsys
submodule) or use checksums (which requires the mchecksum submodule), you need
to issue from the root of the source directory the following command:

    git submodule update --init

Mercury makes use of the CMake build-system and requires that you do an
out-of-source build. In order to do that, you must create a new build
directory and run the `ccmake` command from it:

    cd mercury-X
    mkdir build
    cd build
    ccmake .. (where ".." is the relative path to the mercury-X directory)

Type `'c'` multiple times and choose suitable options. Recommended options are:

    BUILD_SHARED_LIBS                ON (or OFF if the library you link
                                     against requires static libraries)
    BUILD_TESTING                    ON
    Boost_INCLUDE_DIR                /path/to/include/directory
    CMAKE_INSTALL_PREFIX             /path/to/install/directory
    MERCURY_ENABLE_DEBUG             ON/OFF
    MERCURY_ENABLE_PARALLEL_TESTING  ON/OFF
    MERCURY_USE_BOOST_PP             ON
    MERCURY_USE_CHECKSUMS            ON
    MERCURY_USE_SYSTEM_BOOST         ON/OFF
    MERCURY_USE_SYSTEM_MCHECKSUM     ON/OFF
    MERCURY_USE_XDR                  OFF
    NA_USE_BMI                       ON/OFF
    NA_USE_MPI                       ON/OFF
    NA_USE_CCI                       ON/OFF
    NA_USE_OFI                       ON/OFF
    NA_USE_SM                        ON/OFF
    NA_USE_UCX                       ON/OFF

Setting include directory and library paths may require you to toggle to
the advanced mode by typing `'t'`. Once you are done and do not see any
errors, type `'g'` to generate makefiles. Once you exit the CMake
configuration screen and are ready to build the targets, do:

    make

(Optional) Verbose compile/build output:

This is done by inserting `VERBOSE=1` in the `make` command. E.g.:

    make VERBOSE=1

Installing
==========

Assuming that the `CMAKE_INSTALL_PREFIX` has been set (see previous step)
and that you have write permissions to the destination directory, do
from the build directory:

     make install

Testing
=======

Tests can be run to check that basic RPC functionality (requests and bulk
data transfers) is properly working. CTest is used to run the tests,
simply run from the build directory:

    ctest .

(Optional) Verbose testing:

This is done by inserting `-V` in the `ctest` command.  E.g.:

    ctest -V .

Extra verbose information can be displayed by inserting `-VV`. E.g.:

    ctest -VV .

Some tests run with one server process and X client processes. To change the
number of client processes that are being used, the `MPIEXEC_MAX_NUMPROCS`
variable needs to be modified (toggle to advanced mode if you do not see
it). The default value is automatically detected by CMake based on the number
of cores that are available.
Note that you need to run `make` again after the makefile generation
to use the new value.

FAQ
===

Below is a list of the most common questions.

- _Q: Why am I getting undefined references to libfabric symbols?_

  A: In rare occasions, multiple copies of the libfabric library are installed
  on the same system. To make sure that you are using the correct copy of the
  libfabric library, do:

      ldconfig -p | grep libfabric

  If the library returned is not the one that you would expect, make sure to
  either set `LD_LIBRARY_PATH` or add an entry in your `/etc/ld.so.conf.d`
  directory.

- _Q: Is there any logging mechanism?_

  A: To turn on error/warning/debug logs, the `HG_LOG_LEVEL` environment
  variable can be set to either `error`, `warning` or `debug` values. Note that
  for debugging output to be printed, the CMake variable `MERCURY_ENABLE_DEBUG`
  must also be set at compile time. Specific subsystems can be selected using
  the `HG_LOG_SUBSYS` environment variable.

[mailing-lists]: http://mercury-hpc.github.io/help#mailing-lists
[documentation]: http://mercury-hpc.github.io/documentation/
[cci]: http://cci-forum.com/?page_id=46
[libfabric]: https://github.com/ofiwg/libfabric
[ucx]: https://openucx.readthedocs.io/en/master/running.html#ucx-build-and-install
[github-ci-svg]: https://github.com/mercury-hpc/mercury/actions/workflows/ci.yml/badge.svg?branch=master
[github-ci-link]: https://github.com/mercury-hpc/mercury/actions/workflows/ci.yml
[mercury-release-svg]: https://img.shields.io/github/release/mercury-hpc/mercury/all.svg
[mercury-release-link]: https://github.com/mercury-hpc/mercury/releases
[spack-release-svg]: https://img.shields.io/spack/v/mercury.svg
[spack-release-link]: https://spack.readthedocs.io/en/latest/package_list.html#mercury
[yama]: https://www.kernel.org/doc/Documentation/security/Yama.txt
[mochi-link]: https://github.com/mochi-hpc/

