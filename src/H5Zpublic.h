/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, April 16, 1998
 */
#ifndef _H5Zpublic_H
#define _H5Zpublic_H

/*
 * Compression methods.  Method zero means no compression.  Methods 1 through
 * 15 are defined by the library.  Methods 16-255 are user-defined.
 */
typedef int H5Z_method_t;
#define H5Z_NONE	0	/*no compression, must be zero	     */
#define H5Z_DEFLATE	1	/*deflation like gzip		     */
#define H5Z_RES_2	2	/*reserved for internal use	     */
#define H5Z_RES_3	3	/*reserved for internal use	     */
#define H5Z_RES_4	4	/*reserved for internal use	     */
#define H5Z_RES_5	5	/*reserved for internal use	     */
#define H5Z_RES_6	6	/*reserved for internal use	     */
#define H5Z_RES_7	7	/*reserved for internal use	     */
#define H5Z_RES_8	8	/*reserved for internal use	     */
#define H5Z_RES_9	9	/*reserved for internal use	     */
#define H5Z_RES_10	10	/*reserved for internal use	     */
#define H5Z_RES_11	11	/*reserved for internal use	     */
#define H5Z_RES_12	12	/*reserved for internal use	     */
#define H5Z_RES_13	13	/*reserved for internal use	     */
#define H5Z_RES_14	14	/*reserved for internal use	     */
#define H5Z_RES_15	15	/*reserved for internal use	     */
/* user-defined 16-255 */
#define H5Z_MAXVAL	255	/*maximum compression method ID	     */

/*
 * A compression function takes some configuration data which comes from the
 * compression message, namely FLAGS, CD_SIZE, and CLIENT_DATA.  It should
 * read SRC_NBYTES from SRC and compress them into at most DST_NBYTES of DST.
 * If the compressed data would be larger than DST_NBYTES the function should
 * return a value greater than or equal to DST_NBYTES.  On failure the
 * function may return zero.
 *
 * The uncompression function is the inverse of compression and takes the
 * same arguments.  The SRC_NBYTES argument is the number of compressed bytes
 * in SRC.  The function should uncompress SRC into DST. For redundancy,
 * DST_NBYTES contains the size of the DST buffer although if the algorithm
 * is operating properly and the file has not been corrupted the uncompressed
 * data will never be larger than DST_NBYTES.  The function should return the
 * number of bytes in the DST buffer or zero on failure.  Failure includes
 * the overflow of the DST buffer.
 */
typedef size_t (*H5Z_func_t)(unsigned int flags, size_t cd_size,
			     const void *client_data, size_t src_nbytes,
			     const void *src, size_t dst_nbytes,
			     void *dst/*out*/);


#ifdef __cplusplus
extern "C" {
#endif

herr_t H5Zregister (H5Z_method_t method, const char *name, H5Z_func_t compress,
		    H5Z_func_t uncompress);

#ifdef __cplusplus
}
#endif
#endif

