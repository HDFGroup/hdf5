## A Warning

Any application that creates threads that use the HDF5 library must join those threads before either process exit or library close through H5close(). If all HDF5-using threads aren't joined, the threads may exhibit undefined behavior.

## Discussion for Developers on Potential Improvements

It would in principle be possible to make it safe to have threads continue using HDF5 resources after a call to H5close() by keeping a count of threads within the library. (There is probably no solution to an early process exit producing undefined behavior within threads.) This method would only be able to count (and presumably, only _need_ to count) threads that directly interact with the library. Because each thread would need to be counted exactly once, this would most likely be done by use of a thread-local key with e.g. a boolean value used to track whether the a global atomic thread counter has already counted this thread. Then, if H5close() is invoked while this thread counter is above one (because one thread must be doing the closing), the library would not close, and instead keep its resources valid to hopefully avoid bad behavior with the threads. 

The issues with this approach are as follows:

1. The process of checking for the existence/value of the thread-local key is slow, or at least slow enough that it's probably not worth adding this to almost every single API call to prevent this particular edge case.
2. Even with this approach, bad behavior would still be possible if the application does something like expose HDF5 resources to threads indirectly via a global variable. 
3. How to allow H5close() to fail is nonobvious. H5close() could be allowed to return an error indicating a failure to close, but the number of applications which could usefully respond to such an error by joining threads is small. If an application were able/willing to join its created threads, presumably it would have done so before calling H5close(). Alternatively, H5close() could succeed but silently leave the library open. This creates the potential for confusing, unexpected behavior when the user thinks they are closing and re-opening the library, e.g. if environment variables are modified between close and re-open, or if resources such as default property lists are modified.
4. Applications should join threads before closing libraries that those threads are using, so all of this work would constitute an above-and-beyond effort to maintain safe and defined behavior in the face of an unsafe application.

Despite these issues, if a more performant method was found to perform threadcounting like this, it might still constitute a worthwhile change.