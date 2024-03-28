# Building and testing HDF5 VOL connectors with CMake FetchContent

This document details the process of using CMake options to build and test
an HDF5 VOL connector alongside the HDF5 library when building HDF5 from
source. There are several benefits that this may provide, but among them
are the following:

  * A VOL connector built this way can be tested at the same time that
    HDF5 is, which eliminates the need to have a multi-step build process
    where one builds HDF5, uses it to build the VOL connector and then
    uses the external [HDF5 VOL tests](https://github.com/hdfGroup/vol-tests)
    repository to test their connector.
  * Building VOL connectors in this manner will usually install the built
    connector library alongside the HDF5 library, allowing future opportunities
    for HDF5 to set a default plugin path such that the HDF5_PLUGIN_PATH
    environment variable doesn't need to be set.

## Building

To enable building of an HDF5 VOL connector using HDF5's CMake functionality,
a CMake variable must first be set:

    HDF5_VOL_ALLOW_EXTERNAL (Default: "NO")
        This variable is a string that specifies the manner in which the source code for
        an external VOL connector will be retrieved. This variable must be set
        to "GIT" for building external VOL connectors from a Github repository, or
        set to "LOCAL_DIR" to build from a local source directory.


### Building

If the `HDF5_VOL_ALLOW_EXTERNAL` option is set to "GIT", the CMake cache will be populated with a predefined
(currently 10) amount of new variables, named:

    HDF5_VOL_URL01
    HDF5_VOL_URL02
    HDF5_VOL_URL03
    ...

For each of these variables, a URL that points to an HDF5 VOL connector Git
repository can be specified. These URLs should currently be HTTPS URLs. For
example, to specify the HDF5 Asynchronous I/O VOL Connector developed by the
ECP team, one can provide the following option to `cmake`:

    -DHDF5_VOL_URL01=https://github.com/hpc-io/vol-async.git

For each URL specified, HDF5's CMake code will attempt to use CMake's
[FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html)
functionality to retrieve the source code for a VOL connector pointed to by
that URL and will try to build that VOL connector as part of the HDF5 library
build process. 

If `HDF5_VOL_ALLOW_EXTERNAL` is instead set to "LOCAL_DIR", then the CMake cache 
will instead be populated with the variables:

    HDF5_VOL_PATH01
    HDF5_VOL_PATH02
    HDF5_VOL_PATH03
    ...

For each of these variables, an absolute path that points to a local 
directory containing source code for an HDF5 VOL connector
can be specified. For example, to specify a local clone of the 
REST VOL connector stored under one's home directory, one can provide 
the following option to `cmake`:

    -DHDF5_VOL_PATH01=/home/vol-rest

Regardless of the method used to obtain the VOL source code, 
the VOL connector must be able to be built by CMake and currently
must have a CMakeLists.txt file in the top level of the source tree in order to
be buildable by this process. If the source code for a VOL connector is successfully
retrieved, the HDF5 build's CMake cache will be populated with variables from
the VOL connector's CMake code, as if one were building the connector by itself.
This gives one the ability to customize the build of the connector as usual.

The CMake cache will also be populated with a few new variables for each VOL
connector that was successfully retrieved. To generate these
variables, the CMake code first creates an internal name for the VOL connector.
If the source was retrieved from a URL, then the name is generated
by stripping off the last part of the Git repository URL given for the connector,
removing the ".git" suffix and any whitespace and then upper-casing the result.
For example, the name of the VOL connector located at the URL
https://github.com/hpc-io/vol-async.git would become "VOL-ASYNC". If the source was 
retrieved from a local directory, then the source directory's name is trimmed of whitespace, 
upper-cased, and has any trailing slashes removed.

After the VOL's internal name is generated, the following new variables get created:

    HDF5_VOL_<VOL name>_NAME (Default: "")
        This variable specifies the string that should be used when setting the
        HDF5_VOL_CONNECTOR environment variable for testing the VOL connector
        with the CMake-internal name '<VOL name>'. The value for this variable
        can be determined according to the canonical name given to the connector
        by the connector's author(s), as well as any extra info that needs to be
        passed to the connector for its configuration (see example below). This
        variable must be set in order for the VOL connector to be testable with
        HDF5's tests.

    HDF5_VOL_<VOL name>_CMAKE_PACKAGE_NAME (Default: "<lowercased <VOL name>>")
        This variable specifies the exact name that would be passed to CMake
        find_package(...) calls for the VOL connector in question. It is used as
        the dependency name when making CMake FetchContent calls to try to ensure
        that any other VOL connectors to be built which depend on this VOL connector
        can make find_package(...) calls for this VOL connector at configure time.
        By default, this variable is set to a lowercased version of the internal
        name generated for the VOL connector (described above).

    HDF5_VOL_<VOL name>_TEST_PARALLEL (Default: OFF)
        This variable determines whether the VOL connector with the CMake-internal
        name '<VOL name>' should be tested against HDF5's parallel tests.

If the source was retrieved from a Git URL, then the following variable will additionally be created:

    HDF5_VOL_<VOL name>_BRANCH (Default: "main")
        This variable specifies the git branch name or tag to use when fetching
        the source code for the VOL connector with the CMake-internal name
        '<VOL name>'.

As an example, this would create the following variables for the
previously-mentioned VOL connector if it is retrieved from a URL:

    HDF5_VOL_VOL-ASYNC_NAME                  ""
    HDF5_VOL_VOL-ASYNC_CMAKE_PACKAGE_NAME    "vol-async"
    HDF5_VOL_VOL-ASYNC_BRANCH                "main"
    HDF5_VOL_VOL-ASYNC_TEST_PARALLEL         OFF

**NOTE**
If a VOL connector requires extra information to be passed in its
HDF5_VOL_<VOL name>_NAME variable and that information contains any semicolons,
those semicolons should be escaped with a single backslash so that CMake
doesn't parse the string as a list. If `cmake` is run from a shell, extra care
may need to be taken when escaping the semicolons depending on how the
shell interprets backslashes.

### Example - Build and test HDF5 Asynchronous I/O VOL connector from GIT

Assuming that the HDF5 source code has been checked out and a build directory
has been created, running the following cmake command from that build directory
will retrieve, build and test the HDF5 Asynchronous I/O VOL connector while
building HDF5. Note that `[hdf5 options]` represents other build options that
would typically be passed when building HDF5, such as `CMAKE_INSTALL_PREFIX`,
`HDF5_BUILD_CPP_LIB`, etc.

    cmake [hdf5 options]
      -DHDF5_ENABLE_THREADSAFE=ON
      -DHDF5_ENABLE_PARALLEL=ON
      -DALLOW_UNSUPPORTED=ON
      -DHDF5_TEST_API=ON
      -DHDF5_VOL_ALLOW_EXTERNAL="GIT"
      -DHDF5_VOL_URL01=https://github.com/hpc-io/vol-async.git
      -DHDF5_VOL_VOL-ASYNC_BRANCH=develop
      -DHDF5_VOL_VOL-ASYNC_NAME="async under_vol=0\;under_info={}"
      -DHDF5_VOL_VOL-ASYNC_TEST_PARALLEL=ON
      ..

Here, we are specifying that:

  * HDF5 should be built with thread-safety enabled (required by Async VOL connector)
  * HDF5 should be built with parallel enabled (required by Async VOL connector)
  * Allow unsupported HDF5 combinations (thread-safety and HL, which is on by default)
  * Enable the API tests so that they can be tested with the Async VOL connector
  * Build and use the HDF5 Asynchronous I/O VOL connector, located at
    https://github.com/hpc-io/vol-async.git
  * Clone the Asynchronous I/O VOL connector from the repository's 'develop' branch
  * When testing the Asynchronous I/O VOL connector, the `HDF5_VOL_CONNECTOR` environment
    variable should be set to "async under_vol=0\;under_info={}", which
    specifies that the VOL connector with the canonical name "async" should
    be loaded and it should be passed the string "under_vol=0;under_info={}"
    for its configuration (note the backslash-escaping of semicolons in the string
    provided)
  * The Asynchronous I/O VOL connector should be tested against HDF5's parallel API tests

Note that this also assumes that the Asynchronous I/O VOL connector's
[other dependencies](https://hdf5-vol-async.readthedocs.io/en/latest/gettingstarted.html#preparation)
are installed on the system in a way that CMake can find them. If that is not
the case, the locations for these dependencies may need to be provided to CMake
by passing extra options, such as:

    -DABT_INCLUDE_DIR=/path/to/argobots/build/include
    -DABT_LIBRARY=/path/to/argbots/build/lib/libabt.so

which would help CMake find an argobots installation in a non-standard location.

## Testing

To facilitate testing of HDF5 VOL connectors when building HDF5, tests from
the [HDF5 VOL tests](https://github.com/hdfGroup/vol-tests) repository were
integrated back into the library and the following new CMake options were
added to HDF5 builds for the 1.14.1 release:

    HDF5_TEST_API (Default: OFF)
        This variable determines whether the HDF5 API tests will be built and tested.

    HDF5_TEST_API_INSTALL (Default: OFF)
        This variable determines whether the HDF5 API test executables will be installed
        on the system alongside the HDF5 library.

    HDF5_TEST_API_ENABLE_ASYNC (Default: OFF)
        This variable determines whether the HDF5 Asynchronous I/O API tests will be
        built and tested. These tests will only run if a VOL connector reports that
        it supports asynchronous I/O operations when queried via the H5Pget_vol_cap_flags
        API routine.

    HDF5_TEST_API_ENABLE_DRIVER (Default: OFF)
        This variable determines whether the HDF5 API test driver program will be
        built and used for testing. This driver program is useful when a VOL connector
        uses a client/server model where the server program needs to be up and running
        before the VOL connector can function. This option is currently not functional.

When the `HDF5_TEST_API` option is set to ON, HDF5's CMake code builds and tests
the new API tests using the native VOL connector. When one or more external VOL
connectors are built successfully with the process described in this document,
the CMake code will duplicate some of these API tests by adding separate
versions of the tests (for each VOL connector that was built) that set the
`HDF5_VOL_CONNECTOR` environment variable to the value specified for the
HDF5_VOL_<VOL name>_NAME variable for each external VOL connector at build time.
Running the `ctest` command will then run these new tests which load and run with
each VOL connector that was built in turn. When run via the `ctest` command, the
new tests typically follow the naming scheme:

    HDF5_VOL_<VOL name lowercase>-h5_api_test_<test name>
    HDF5_VOL_<VOL name lowercase>-h5_api_test_parallel_<test name>

**NOTE**
If dependencies of a built VOL connector are installed on the system in
a non-standard location that would typically require one to set `LD_LIBRARY_PATH`
or similar, one should ensure that those environment variables are set before
running tests. Otherwise, the tests that run with that connector will likely
fail due to being unable to load the necessary libraries for its dependencies.
