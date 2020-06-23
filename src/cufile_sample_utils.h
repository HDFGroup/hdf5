/*
 * Copyright 2019 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

#ifndef __CUFILE_SAMPLE_UTILS_H_
#define __CUFILE_SAMPLE_UTILS_H_

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
//#include <random>
//#include <chrono>
//#include <fstream>
//#include <iostream>
//#include <stdexcept>

#include <sys/stat.h>
#include <openssl/sha.h>

#include <cuda.h>
#include <cuda_runtime.h>

#define MAX_CHUNK_READ (64 * 1024UL)

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

//struct Prng {
//	long rmax_;
//	std::mt19937 rand_;
//	std::uniform_int_distribution<long> dist_;
//	Prng(long rmax) :
//		rmax_(rmax),
//		rand_(std::chrono::high_resolution_clock::now().time_since_epoch().count()),
//		dist_(std::uniform_int_distribution<long>(0, rmax_))
//		{}
//	long next_random_offset(void) {
//		return dist_(rand_);
//	}
//};
//
////
//// cuda driver error description
////
//static inline const char *GetCuErrorString(CUresult curesult) {
//	const char *descp;
//	if (cuGetErrorName(curesult, &descp) != CUDA_SUCCESS)
//		descp = "unknown cuda error";
//	return descp;
//}
//
////
//// cuda runtime error description
////
//static inline const char *GetCudaErrorString(cudaError_t cudaerr) {
//	return cudaGetErrorName(cudaerr);
//}
//
//
////
//// cuFile APIs return both cuFile specific error codes as well as POSIX error codes
//// for ease, the below template can be used for getting the error description depending
//// on its type.
//
//// POSIX
//template<class T,
//	typename std::enable_if<std::is_integral<T>::value, std::nullptr_t>::type = nullptr>
//std::string cuFileGetErrorString(T status) {
//	status = std::abs(status);
//	return IS_CUFILE_ERR(status) ?
//		std::string(CUFILE_ERRSTR(status)) : std::string(std::strerror(status));
//}
//
//// CUfileError_t
//template<class T,
//	typename std::enable_if<!std::is_integral<T>::value, std::nullptr_t>::type = nullptr>
//std::string cuFileGetErrorString(T status) {
//	std::string errStr = cuFileGetErrorString(static_cast<int>(status.err));
//	if (IS_CUDA_ERR(status))
//		errStr.append(".").append(GetCuErrorString(status.cu_err));
//	return errStr;
//}
//
//
//// SHASUM routine : computes digest of nbytes of a file
//static inline int SHASUM256(const char *fpath, unsigned char md[SHA256_DIGEST_LENGTH],
//		size_t bytes = 0) {
//	size_t size;
//	SHA256_CTX c;
//	char buf[MAX_CHUNK_READ];
//	std::ifstream fp(fpath, std::ifstream::in | std::ifstream::binary);
//
//	if (!fp.is_open()) {
//		std::cerr << "file open failed" << std::endl;
//		return -1;
//	}
//
//	fp.seekg(0, fp.end);
//	size = fp.tellg();
//	fp.seekg(0, fp.beg);
//
//	if (!size) {
//		fp.close();
//		std::cerr << "file is empty" << std::endl;
//		return -1;
//	}
//
//	if (bytes > size) {
//		fp.close();
//		std::cerr << bytes << ":" << size << std::endl;
//		std::cerr << "bytes more than file size" << std::endl;
//		return -1;
//	}
//
//	if (!bytes)
//		bytes = size;
//
//	SHA256_Init(&c);
//
//	while (bytes && !fp.eof()) {
//		size = std::min(bytes, MAX_CHUNK_READ);
//		fp.read(buf, size);
//		SHA256_Update(&c, buf, fp.gcount());
//		bytes -= size;
//	}
//
//	fp.close();
//
//	SHA256_Final(md, &c);
//	return 0;
//}
//
//// SHASUM routine : computes digest of nbytes from a device memory region
//static inline int SHASUM256_DEVICEMEM(char *devPtr,
//                         size_t memSize,
//                         unsigned char md[SHA256_DIGEST_LENGTH],
//                         size_t devPtrOff,
//                         size_t bytes = 0) {
//	size_t size;
//	SHA256_CTX c;
//	char buf[MAX_CHUNK_READ];
//	char *devbuf = devPtr + devPtrOff;
//
//	size = memSize - devPtrOff;
//	if (!size || size < 0) {
//		std::cerr << "invalid parameters" << std::endl;
//		return -1;
//	}
//
//	if (bytes > size) {
//		std::cerr << bytes << ":" << size << std::endl;
//		std::cerr << "bytes more than size" << std::endl;
//		return -1;
//	}
//
//	if (!bytes)
//		bytes = size;
//
//	SHA256_Init(&c);
//
//	while (bytes) {
//		size = std::min(bytes, MAX_CHUNK_READ);
//		cudaMemcpy(buf, devbuf, size, cudaMemcpyDeviceToHost);
//		SHA256_Update(&c, buf, size);
//		bytes -= size;
//		devbuf += size;
//	}
//
//	SHA256_Final(md, &c);
//	return 0;
//}
//
//// SHASUM routine : print
//static inline void DumpSHASUM(unsigned char md[SHA256_DIGEST_LENGTH]) {
//	for (int i = 0; i < SHA256_DIGEST_LENGTH ; i++)
//		std::cout << std::hex << static_cast<int>(md[i]);
//	std::cout << std::dec << std::endl;
//}
//
//size_t GetFileSize(int fd) {
//	int ret;
//	struct stat st;
//
//	ret = fstat(fd, &st);
//	return (ret == 0) ? st.st_size : -1;
//}
#endif
