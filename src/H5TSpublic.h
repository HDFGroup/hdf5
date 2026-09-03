/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5TS (threadsafety) module.
 */

#ifndef H5TSpublic_H
#define H5TSpublic_H

#include "H5public.h" /* Generic Functions                        */

/*****************/
/* Public Macros */
/*****************/

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* HDF5 global thread pool routines */

/**
 * \ingroup H5TS
 *
 * \brief Sets the number of internal threads to use for internal multithreading
 *
 * \param[in] num_threads Number of threads to use for internal multithreading
 *
 * \return \herr_t
 *
 * \details H5TSset_internal_threads() directs the HDF5 library to us
 *          \p num_threads threads to accelerate parallelizable operations.
 *
 *          This is currently only used to accelerate read operations for
 *          chunked datasets that either have data filters applied or for which
 *          the chunks are small enough to fit in cache. In this case, the
 *          library parallelizes the reads from disk, the data filter
 *          operations, and the memory scatter operation. However, all of these
 *          operations are currently protected by a mutex so no performance gain
 *          is expected and this feature is purely experimental. These mutexes
 *          will be relaxed in the future to enable performance acceleration.
 *
 *          Currently, the library will immediately create \p num_threads
 *          threads and retain them until this function is called again. Calling
 *          this function with \p num_threads set to \c 0 will terminate these
 *          threads and disable internal multithreading.
 *
 * \note    This function is only present when the library is compiled with HDF5_ENABLE_CONCURRENCY=ON.
 *
 * \since 2.3.0
 *
 */
H5_DLL herr_t H5TSset_internal_threads(unsigned num_threads);

#ifdef __cplusplus
}
#endif

#endif /* H5TSpublic_H */
