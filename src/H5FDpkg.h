/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: This file contains declarations which are visible only within
 *          the H5FD package.  Source files outside the H5FD package should
 *          include H5FDprivate.h instead.
 */
#if !(defined H5FD_FRIEND || defined H5FD_MODULE)
#error "Do not include this file outside the H5FD package!"
#endif

#ifndef H5FDpkg_H
#define H5FDpkg_H

/* Get package's private header */
#include "H5FDprivate.h" /* File drivers				*/

/* Other private headers needed by this file */

/**************************/
/* Package Private Macros */
/**************************/

/****************************/
/* Package Private Typedefs */
/****************************/

/*****************************/
/* Package Private Variables */
/*****************************/

/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL haddr_t H5FD__alloc_real(H5FD_t *file, H5FD_mem_t type, hsize_t size, haddr_t *align_addr,
                                hsize_t *align_size);
H5_DLL herr_t  H5FD__free_real(H5FD_t *file, H5FD_mem_t type, haddr_t addr, hsize_t size);

/* Testing functions */
#ifdef H5FD_TESTING
H5_DLL H5FD_t *H5FDopen_test(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
H5_DLL herr_t  H5FDclose_test(H5FD_t *file);
H5_DLL haddr_t H5FDalloc_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
H5_DLL haddr_t H5FDget_eoa_test(H5FD_t *file, H5FD_mem_t type);
H5_DLL herr_t  H5FDset_eoa_test(H5FD_t *file, H5FD_mem_t type, haddr_t eoa);
H5_DLL haddr_t H5FDget_eof_test(H5FD_t *file, H5FD_mem_t type);
H5_DLL herr_t  H5FDread_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                             void *buf);
H5_DLL herr_t  H5FDwrite_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                              const void *buf);
H5_DLL herr_t  H5FDread_vector_test(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                    haddr_t addrs[], size_t sizes[], void *bufs[]);
H5_DLL herr_t  H5FDwrite_vector_test(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                     haddr_t addrs[], size_t sizes[], const void *bufs[]);
H5_DLL herr_t  H5FDread_selection_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count,
                                       hid_t mem_spaces[], hid_t file_spaces[], haddr_t offsets[],
                                       size_t element_sizes[], void *bufs[]);
H5_DLL herr_t  H5FDwrite_selection_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count,
                                        hid_t mem_spaces[], hid_t file_spaces[], haddr_t offsets[],
                                        size_t element_sizes[], const void *bufs[]);
H5_DLL herr_t  H5FDtruncate_test(H5FD_t *file, hid_t dxpl_id, hbool_t closing);
H5_DLL herr_t  H5FDctl_test(H5FD_t *file, uint64_t op_code, uint64_t flags, const void *input, void **output);
H5_DLL bool    H5FD__supports_swmr_test(const char *vfd_name);
#endif /* H5FD_TESTING */

#endif /* H5FDpkg_H */
