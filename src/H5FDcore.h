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
 * Purpose:	The public header file for the core virtual file driver (VFD)
 */
#ifndef H5FDcore_H
#define H5FDcore_H

/* Public header files */
#include "H5FDpublic.h" /* File drivers             */

/** ID for the core VFD */
#define H5FD_CORE (H5OPEN H5FD_CORE_id_g)

/** Identifier for the core VFD \since 1.14.0 */
#define H5FD_CORE_VALUE H5_VFD_CORE

#ifdef __cplusplus
extern "C" {
#endif

/** @private
 *
 * \brief ID for the core VFD
 */
H5_DLLVAR hid_t H5FD_CORE_id_g;

/**
 * \ingroup FAPL
 *
 * \brief Modifies the file access property list to use the #H5FD_CORE driver
 *
 * \fapl_id
 * \param[in] initial_size Initial size, in bytes, of the backing memory
 *            buffer; a value of 0 preserves the existing lazy-allocation
 *            behavior
 * \param[in] increment Size, in bytes, of memory increments
 * \param[in] backing_store Boolean flag indicating whether to write the file
 *            contents to disk when the file is closed
 * \returns \herr_t
 *
 * \details H5Pset_fapl_core2() modifies the file access property list to use the
 *          #H5FD_CORE driver.
 *
 *          The #H5FD_CORE driver enables an application to work with a file in
 *          memory, speeding reads and writes as no disk access is made. File
 *          contents are stored only in memory until the file is closed. The \p
 *          backing_store parameter determines whether file contents are ever
 *          written to disk.
 *
 *          \p initial_size specifies the size of the initial memory buffer
 *          used by the driver. If \p initial_size is 0, memory allocation is
 *          deferred until needed. If nonzero, \p initial_size must be greater
 *          than or equal to \p increment, after default increment handling is
 *          applied.
 *
 *          \p increment specifies the increment by which allocated memory is to
 *          be increased each time more memory is required.
 *
 *          While using H5Fcreate() to create a core file, if the \p
 *          backing_store is set to 1 (true), the file contents are flushed to a
 *          file with the same name as this core file when the file is closed or
 *          access to the file is terminated in memory.
 *
 *          The application is allowed to open an existing file with #H5FD_CORE
 *          driver. While using H5Fopen() to open an existing file, if the \p
 *          backing_store is set to 1 (true) and the \c flags for H5Fopen() is set to
 *          #H5F_ACC_RDWR, any change to the file contents are saved to the file
 *          when the file is closed. If \p backing_store is set to 0 (false) and the \c
 *          flags for H5Fopen() is set to #H5F_ACC_RDWR, any change to the file
 *          contents will be lost when the file is closed. If the flags for
 *          H5Fopen() is set to #H5F_ACC_RDONLY, no change to the file is
 *          allowed either in memory or on file.
 *
 * \note Currently this driver cannot create or open family or multi files.
 *
 * \version 2.0.0 C function H5Pset_fapl_core() renamed to H5Pset_fapl_core1()
 *          and deprecated; this function H5Pset_fapl_core2() introduced with
 *          the new \p initial_size parameter.
 *
 * \since 2.0.0
 *
 */
H5_DLL herr_t H5Pset_fapl_core2(hid_t fapl_id, size_t initial_size, size_t increment, bool backing_store);

/**
 * \ingroup FAPL
 *
 * \brief Queries core file driver properties
 *
 * \fapl_id
 * \param[out] initial_size Initial size, in bytes, of the backing memory
 *             buffer
 * \param[out] increment Size, in bytes, of memory increments
 * \param[out] backing_store Boolean flag indicating whether to write the file
 *             contents to disk when the file is closed
 * \returns \herr_t
 *
 * \details H5Pget_fapl_core2() queries the #H5FD_CORE driver properties as set
 *          by H5Pset_fapl_core().
 *
 * \version 2.0.0 C function H5Pget_fapl_core() renamed to H5Pget_fapl_core1()
 *          and deprecated; this function H5Pget_fapl_core2() introduced with
 *          the new \p initial_size parameter.
 *
 * \since 2.0.0
 *
 */
H5_DLL herr_t H5Pget_fapl_core2(hid_t fapl_id, size_t *initial_size /*out*/, size_t *increment /*out*/,
                                bool *backing_store /*out*/);

/* Deprecated API functions */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/**
 * \ingroup FAPL
 *
 * \brief Modifies the file access property list to use the #H5FD_CORE driver
 *
 * \fapl_id
 * \param[in] increment Size, in bytes, of memory increments
 * \param[in] backing_store Boolean flag indicating whether to write the file
 *            contents to disk when the file is closed
 * \returns \herr_t
 *
 * \deprecated This function has been renamed from H5Pset_fapl_core() and is
 *             deprecated in favor of the macro #H5Pset_fapl_core or the
 *             function H5Pset_fapl_core2().
 *
 * \details H5Pset_fapl_core1() modifies the file access property list to use
 *          the #H5FD_CORE driver, equivalent to calling H5Pset_fapl_core2()
 *          with \p initial_size set to 0 (lazy allocation).
 *
 * \version 2.0.0 Function H5Pset_fapl_core() renamed to H5Pset_fapl_core1()
 *          and deprecated in this release.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pset_fapl_core1(hid_t fapl_id, size_t increment, bool backing_store);

/**
 * \ingroup FAPL
 *
 * \brief Queries core file driver properties
 *
 * \fapl_id
 * \param[out] increment Size, in bytes, of memory increments
 * \param[out] backing_store Boolean flag indicating whether to write the file
 *             contents to disk when the file is closed
 * \returns \herr_t
 *
 * \deprecated This function has been renamed from H5Pget_fapl_core() and is
 *             deprecated in favor of the macro #H5Pget_fapl_core or the
 *             function H5Pget_fapl_core2().
 *
 * \details H5Pget_fapl_core1() queries the #H5FD_CORE driver properties as
 *          set by H5Pset_fapl_core(). Equivalent to calling H5Pget_fapl_core2()
 *          with \p initial_size set to NULL.
 *
 * \version 2.0.0 Function H5Pget_fapl_core() renamed to H5Pget_fapl_core1()
 *          and deprecated in this release.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Pget_fapl_core1(hid_t fapl_id, size_t *increment /*out*/, bool *backing_store /*out*/);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif
