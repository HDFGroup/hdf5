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
 * \brief Creates the global thread pool
 *
 * \param[in] num_threads Number of threads to add to thread pool
 *
 * \return \herr_t
 *
 * \details H5TSglobal_pool_create() creates the global thread pool with
 *          \p num_threads threads for the HDF5 library to use to accelerate
 *          parallelizable operations.
 *
 *          This is currently only used to accelerate read operations for
 *          chunked datasets that either have data filters applied or the chunks
 *          are small enough to fit in cache. In this case, the library
 *          parallelizes the reads from disk, the data filter operations, and
 *          the memory scatter operation. However, all of these operations are
 *          currently protected by a mutex so no performance gain is expected
 *          and this feature is purely experimental. These mutexes will be
 *          relaxed in the future to enable performance acceleration.
 *
 *          The thread pool must not already exist. If this function is called
 *          when the thread pool already exists, an error will be returned.
 *
 * \note    This function is only present when the library is compiled with HDF5_ENABLE_CONCURRENCY ON.
 *
 * \since 2.3.0
 *
 */
H5_DLL herr_t H5TSglobal_pool_create(unsigned num_threads);

/**
 * \ingroup H5TS
 *
 * \brief Destroys the global thread pool
 *
 * \return \herr_t
 *
 * \details H5TSglobal_pool_destroy() destroys the global thread pool created
 *          with H5TSglobal_pool_create(). After calling this function, the
 *          library will no longer use this thread pool to accelerate
 *          operations. The global thread pool may be created again with another
 *          call to H5TSglobal_pool_create().
 *
 *          The thread pool must exist. If this function is called when the
 *          thread pool does not exist, an error will be returned.
 *
 * \note    This function is only present when the library is compiled with HDF5_ENABLE_CONCURRENCY ON.
 *
 * \since 2.3.0
 *
 */
H5_DLL herr_t H5TSglobal_pool_destroy(void);

#ifdef __cplusplus
}
#endif

#endif /* H5TSpublic_H */
