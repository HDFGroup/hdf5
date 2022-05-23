# HDF5 Library initialization and shutdown

## Application perspective

### Implicit initialization and shutdown

When a developer exports a new symbol as part of the HDF5 library,
they should make sure that an application cannot enter the library in an
uninitialized state through a new API function, or read an uninitialized
value from a non-function HDF5 symbol.

The HDF5 library initializes itself when an application either enters
the library through an API function call such as `H5Fopen`, or when
an application evaluates an HDF5 symbol that represents either a
property-list identifier such as `H5F_ACC_RDONLY` or `H5F_ACC_RDWR`,
a property-list class identifier such as `H5P_FILE_ACCESS`, a VFD
identifier such as `H5FD_FAMILY` or `H5FD_SEC2`, or a type identifier
such as `H5T_NATIVE_INT64`.

The library sets a flag when initialization occurs and as long as the
flag is set, skips initialization.

The library provides a couple of macros that initialize the library
as necessary.  The library is initialized as a side-effect of the
`FUNC_ENTER_API*` macros used at the top of most API functions.  HDF5
library symbols other than functions are provided through `#define`s
that use `H5OPEN` to introduce a library-initialization call (`H5open`)
at each site where a non-function symbol is used.

Ordinarily the library registers an `atexit(3)` handler to shut itself
down when the application exits.

### Explicit initialization and shutdown

An application may use an API call, `H5open`, to explicitly initialize
the library.  `H5close` explicitly shuts down the library.

## Library internals perspective

No matter how library initializion begins, eventually the internal
function `H5_init_library` will be called.  `H5_init_library` is
responsible for calling the initializers for every internal HDF5
library module (aka "package") in the correct order so that no module is
initialized before its prerequisite modules.  A table in `H5_init_library`
establishes the order of initialization.  If a developer adds a
module to the library that it is appropriate to initialize with the rest
of the library, then they should insert its initializer into the right
place in the table.

`H5_term_library` drives library shutdown.  Library shutdown is
table-driven, too.  If a developer adds a module that needs to release
resources during library shutdown, then they should add a call at the
right place to the shutdown table.  Note that some entries in the shutdown
table are marked as "barriers," and if a new module should only be
shutdown *strictly after* the preceding modules, then it should be marked
as a barrier.  See the comments in `H5_term_library` for more information.
