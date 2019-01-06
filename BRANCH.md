This branch is for the development of a test VOL connector to better
test the VOL API. In particular, this will expose any issues where
we've baked 'native file'-ness into the API.

The intent is to implement 100% of the HDF5 library's functionality
so it can be transparently subbed in for the native VOL connector.

The JSON parser I've chosen is Jannsen, which is used in other THG
products and projects and has an MIT license.

It currently only supports the Autotools, but it will not be difficult
to add CMake support.

Plan:
* Implement configuration and building via the autotools. (DONE)
* Add a test program that loads the connector. (DONE)
* Implement basic file operations.
* Implement group operations.
* Implement basic dataset operations.
* Implement basic attribute operations.

NOTES:
* The internets says to use libuuid to generate UUIDs as it's pretty
  lightweight.
* Due to dependencies on libuuid and jansson, we're going to need to
  protect the code and only build when everything is present. I think
  this is okay since this is more of a test for ensuring the VOL doesn't
  suck than it is a test that someone's build is working normally.
* Partial I/O is going to be crazy slow unless I want to add in some
  sort of optimized data container behind the scenes.
