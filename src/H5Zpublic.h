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
 * Filter identifiers.  Values 0 through 255 are for filters defined by the
 * HDF5 library.  Values 256 through 511 are available for testing new
 * filters. Subsequent values should be obtained from the HDF5 development
 * team at hdf5dev@ncsa.uiuc.edu.  These values will never change because they
 * appear in the HDF5 files.
 */
typedef int H5Z_filter_t;
#define H5Z_FILTER_ERROR	(-1)	/*no filter			*/
#define H5Z_FILTER_NONE		0	/*reserved indefinitely		*/
#define H5Z_FILTER_DEFLATE	1 	/*deflation like gzip	     	*/
#define H5Z_FILTER_SHUFFLE      2       /*shuffle the data              */
#define H5Z_FILTER_ADLER32      3       /*adler32 checksum of EDC       */
#define H5Z_FILTER_RESERVED     256	/*filter ids below this value are reserved */
#define H5Z_FILTER_MAX		65535	/*maximum filter id		*/

/* Flags for filter definition */
#define H5Z_FLAG_DEFMASK	0x00ff	/*definition flag mask		*/
#define H5Z_FLAG_MANDATORY      0x0000  /*filter is mandatory		*/
#define H5Z_FLAG_OPTIONAL	0x0001	/*filter is optional		*/

/* Additional flags for filter invocation */
#define H5Z_FLAG_INVMASK	0xff00	/*invocation flag mask		*/
#define H5Z_FLAG_REVERSE	0x0100	/*reverse direction; read	*/
#define H5Z_FLAG_SKIP_EDC	0x0200	/*skip EDC filters for read	*/

/* Values to decide if EDC is enabled for reading data */
typedef enum H5Z_EDC_t {
    H5Z_ERROR_EDC       = -1,   /* error value */
    H5Z_DISABLE_EDC     = 0,
    H5Z_ENABLE_EDC      = 1,
    H5Z_NO_EDC          = 2     /* must be the last */
} H5Z_EDC_t;    

/* Return values for filter callback function */
typedef enum H5Z_cb_return_t {
    H5Z_CB_ERROR  = -1,
    H5Z_CB_FAIL   = 0,    /* I/O should fail if filter fails. */
    H5Z_CB_CONT   = 1,    /* I/O continues if filter fails.   */
    H5Z_CB_NO     = 2
} H5Z_cb_return_t;

/* Filter callback function definition */
typedef H5Z_cb_return_t (*H5Z_filter_func_t)(H5Z_filter_t filter, void* buf,
                                size_t buf_size, void* op_data);
                                
/* Structure for filter callback property */
typedef struct H5Z_cb_t {
    H5Z_filter_func_t func;
    void*              op_data;
} H5Z_cb_t;

#ifdef __cplusplus
extern "C" {
#endif

/*
 * A filter gets definition flags and invocation flags (defined above), the
 * client data array and size defined when the filter was added to the
 * pipeline, the size in bytes of the data on which to operate, and pointers
 * to a buffer and its allocated size.
 *
 * The filter should store the result in the supplied buffer if possible,
 * otherwise it can allocate a new buffer, freeing the original.  The
 * allocated size of the new buffer should be returned through the BUF_SIZE
 * pointer and the new buffer through the BUF pointer.
 *
 * The return value from the filter is the number of bytes in the output
 * buffer. If an error occurs then the function should return zero and leave
 * all pointer arguments unchanged.
 */
typedef size_t (*H5Z_func_t)(unsigned int flags, size_t cd_nelmts,
			     const unsigned int cd_values[], size_t nbytes,
			     size_t *buf_size, void **buf);

H5_DLL herr_t H5Zregister(H5Z_filter_t id, const char *comment,
			   H5Z_func_t filter);

H5_DLL herr_t H5Zunregister(H5Z_filter_t id);

H5_DLL htri_t H5Zfilter_avail(H5Z_filter_t id);


#ifdef __cplusplus
}
#endif
#endif

