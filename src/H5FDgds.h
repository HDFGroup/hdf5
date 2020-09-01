/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  John Ravi <jjravi@lbl.gov>
 *              Wednesday, July  1, 2020
 *
 * Purpose:	The public header file for the CUDA GPUDirect Storage driver.
 */
#ifndef H5FDgds_H
#define H5FDgds_H

#define H5FD_GDS	(H5FD_gds_init())

#define check_cudadrivercall(fn) \
	do { \
		CUresult res = fn; \
		if (res != CUDA_SUCCESS) { \
			const char *str = nullptr; \
			cuGetErrorName(res, &str); \
			fprintf(stderr, "cuda driver api call failed %d, %d : %s\n", fn, \
				__LINE__, str); \
			fprintf(stderr, "EXITING program!!!\n"); \
			exit(1); \
		} \
	} while(0)

#define check_cudaruntimecall(fn) \
	do { \
		cudaError_t res = fn; \
		if (res != cudaSuccess) { \
			const char *str = cudaGetErrorName(res); \
			fprintf(stderr, "cuda runtime api call failed %d, %d : %s\n", fn, \
				__LINE__, str); \
			fprintf(stderr, "EXITING program!!!\n"); \
			exit(1); \
		} \
	} while(0)

/* Function prototypes */
#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_gds_init(void);
H5_DLL herr_t H5Pset_fapl_gds(hid_t fapl_id);

#ifdef __cplusplus
}
#endif

#endif

