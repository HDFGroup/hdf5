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

The difficult part of dealing with this is going to involve dealing with
state, particularly as 'files' grow large. There may be clashes when it
comes to mapping a fundamentally random-access data store to a file stream
data store.

