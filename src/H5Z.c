/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, April 16, 1998
 *
 * Purpose:	Functions for data compression.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Zprivate.h>

#ifdef HAVE_ZLIB_H
#   include <zlib.h>
#endif

/* Interface initialization */
#define PABLO_MASK	H5Z_mask
#define INTERFACE_INIT H5Z_init_interface
static intn interface_initialize_g = FALSE;
static herr_t H5Z_init_interface (void);
static void H5Z_term_interface (void);

/*
 * The compression table maps compression method number to a struct that
 * contains pointers to the compress and uncompress methods along with timing
 * statistics.
 */
typedef struct H5Z_class_t {
    char	*name;		/*method name for debugging		*/
    H5Z_func_t	compress;	/*compression function			*/
    H5Z_func_t	uncompress;	/*uncompression function		*/

#ifdef H5Z_DEBUG
    struct {
	hsize_t	nbytes;		/*bytes compressed including overruns	*/
	hsize_t over;		/*bytes of overrun			*/
	H5_timer_t timer;	/*total compression time inc. overruns	*/
	hsize_t	failed;		/*bytes of failure (not overruns)	*/
    } comp;

    struct {
	hsize_t	nbytes;		/*bytes uncompressed, including overruns*/
	hsize_t	over;		/*bytes of overrun			*/
	H5_timer_t timer;	/*total uncompression time		*/
	hsize_t failed;		/*bytes of failure (not overruns)	*/
    } uncomp;
#endif
} H5Z_class_t;
static H5Z_class_t	H5Z_g[H5Z_USERDEF_MAX+1];

/* Compression and uncompression methods */
static size_t H5Z_zlib_c (unsigned int flags, size_t __unused__ cd_size,
			  const void __unused__ *client_data,
			  size_t src_nbytes, const void *_src,
			  size_t dst_nbytes, void *dst/*out*/);
static size_t H5Z_zlib_u (unsigned int flags, size_t __unused__ cd_size,
			  const void __unused__ *client_data,
			  size_t src_nbytes, const void *_src,
			  size_t dst_nbytes, void *dst/*out*/);


/*-------------------------------------------------------------------------
 * Function:	H5Z_init_interface
 *
 * Purpose:	Initializes the data compression layer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_init_interface (void)
{
    FUNC_ENTER (H5Z_init_interface, FAIL);

    H5_add_exit (H5Z_term_interface);

    H5Z_register (H5Z_NONE, "none", NULL, NULL);
    H5Z_register (H5Z_DEFLATE, "deflate", H5Z_zlib_c, H5Z_zlib_u);

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_term_interface
 *
 * Purpose:	Terminate the H5Z layer.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
H5Z_term_interface (void)
{
#ifdef H5Z_DEBUG
    int		i, nprint=0;
    char	name[16];

    for (i=0; i<=H5Z_USERDEF_MAX; i++) {
	if (H5Z_g[i].comp.nbytes || H5Z_g[i].uncomp.nbytes) {
	    if (0==nprint++) {
		HDfprintf (stderr, "H5Z: compression statistics accumulated "
			   "over life of library:\n");
		HDfprintf (stderr, "   %-10s   %10s %7s %7s %8s %8s %8s %9s\n",
			   "Method", "Total", "Overrun", "Errors", "User",
			   "System", "Elapsed", "Bandwidth");
		HDfprintf (stderr, "   %-10s   %10s %7s %7s %8s %8s %8s %9s\n",
			   "------", "-----", "-------", "------", "----",
			   "------", "-------", "---------");
	    }
	    sprintf (name, "%s-c", H5Z_g[i].name);
	    HDfprintf (stderr,
		       "   %-12s %10Hd %7Hd %7Hd %8.2f %8.2f %8.2f ",
		       name,
		       H5Z_g[i].comp.nbytes,
		       H5Z_g[i].comp.over,
		       H5Z_g[i].comp.failed,
		       H5Z_g[i].comp.timer.utime,
		       H5Z_g[i].comp.timer.stime,
		       H5Z_g[i].comp.timer.etime);
	    if (H5Z_g[i].comp.timer.etime>0) {
		HDfprintf (stderr, "%9.3e\n",
			   H5Z_g[i].comp.nbytes / H5Z_g[i].comp.timer.etime);
	    } else {
		HDfprintf (stderr, "%9s\n", "NaN");
	    }
	    
	    sprintf (name, "%s-u", H5Z_g[i].name);
	    HDfprintf (stderr,
		       "   %-12s %10Hd %7Hd %7Hd %8.2f %8.2f %8.2f ",
		       name,
		       H5Z_g[i].uncomp.nbytes,
		       H5Z_g[i].uncomp.over,
		       H5Z_g[i].uncomp.failed,
		       H5Z_g[i].uncomp.timer.utime,
		       H5Z_g[i].uncomp.timer.stime,
		       H5Z_g[i].uncomp.timer.etime);
	    if (H5Z_g[i].uncomp.timer.etime>0) {
		HDfprintf (stderr, "%9.3e\n", 
			   H5Z_g[i].uncomp.nbytes/H5Z_g[i].uncomp.timer.etime);
	    } else {
		HDfprintf (stderr, "%9s\n", "NaN");
	    }
	}
	H5MM_xfree (H5Z_g[i].name);
    }
    HDmemset (H5Z_g, 0, sizeof H5Z_g);
#endif
}


/*-------------------------------------------------------------------------
 * Function:	H5Zregister
 *
 * Purpose:	This function registers new compression and uncompression
 *		methods for a method number.  The NAME argument is used for
 *		debugging and may be the null pointer.  Either or both of
 *		CFUNC (the compression function) and UFUNC (the uncompression
 *		method) may be null pointers.
 *
 *		The statistics associated with a method number are not reset
 *		by this function; they accumulate over the life of the
 *		library.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Zregister (H5Z_method_t method, const char *name, H5Z_func_t cfunc,
	     H5Z_func_t ufunc)
{
    FUNC_ENTER (H5Zregister, FAIL);
    H5TRACE4("e","Zmsxx",method,name,cfunc,ufunc);

    /* Check args */
    if (method<0 || method>H5Z_USERDEF_MAX) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "invalid data compression method number");
    }
    if (method<16) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "unable to modify predefined compression methods");
    }

    /* Do it */
    if (H5Z_register (method, name, cfunc, ufunc)<0) {
	HRETURN_ERROR (H5E_COMP, H5E_CANTINIT, FAIL,
		       "unable to register compression methods");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_compress
 *
 * Purpose:	Compress NBYTES from SRC into at most NBYTES of DST.
 *
 * Return:	Success:	Number of bytes in DST
 *
 *		Failure:	0 if the DST buffer overflowed or something
 *				else went wrong.
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Z_compress (const H5O_compress_t *comp, size_t nbytes, const void *src,
	      void *dst/*out*/)
{
    size_t		ret_value = 0;
    H5Z_method_t	method = comp ? comp->method : H5Z_NONE;
    H5Z_func_t		cfunc = NULL;
#ifdef H5Z_DEBUG
    intn		over = 0;
    H5_timer_t		timer;
#endif
    
    FUNC_ENTER (H5Z_compress, 0);

#ifdef H5Z_DEBUG
    H5_timer_begin (&timer);
#endif

    if (H5Z_NONE==method) {
	/* No compression method */
	HGOTO_DONE (0);
	
    } else if (NULL==(cfunc=H5Z_g[method].compress)) {
	/* No compress function */
	HGOTO_ERROR (H5E_COMP, H5E_UNSUPPORTED, 0,
		     "compression method is not supported");
	
    } else if (0==(ret_value=(cfunc)(comp->flags, comp->cd_size,
				     comp->client_data, nbytes,
				     src, nbytes, dst))) {
	/* Compress failed */
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "compression failed");
	
    } else if (ret_value>=nbytes) {
	/* Output is not smaller than input */
#ifdef H5Z_DEBUG
	H5Z_g[method].comp.over += nbytes;
	over = 1;
#endif
	HGOTO_DONE (0);
    }

 done:
#ifdef H5Z_DEBUG
    H5Z_g[method].comp.nbytes += nbytes;
    if (0==ret_value && !over) H5Z_g[method].comp.failed += nbytes;
    H5_timer_end (&(H5Z_g[method].comp.timer), &timer);
#endif
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_uncompress
 *
 * Purpose:	Uncompress SRC_NBYTES from SRC into at most DST_NBYTES of
 *		DST. 
 *
 * Return:	Success:	Number of bytes in DST buffer.
 *
 *		Failure:	0 if the uncompression failed or DST wasn't
 *				big enough to hold the result.
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Z_uncompress (const H5O_compress_t *comp, size_t src_nbytes, const void *src,
		size_t dst_nbytes, void *dst/*out*/)
{
    size_t		ret_value = 0;
    H5Z_func_t		ufunc = NULL;
    H5Z_method_t	method = comp ? comp->method : H5Z_NONE;
#ifdef H5Z_DEBUG
    H5_timer_t		timer;
#endif
    
    FUNC_ENTER (H5Z_uncompress, 0);

#ifdef H5Z_DEBUG
    H5_timer_begin (&timer);
#endif

    if (H5Z_NONE==method) {
	/* No compression method */
	assert (src_nbytes<=dst_nbytes);
	HDmemcpy (dst, src, src_nbytes);
	ret_value = src_nbytes;
	
    } else if (src_nbytes==dst_nbytes) {
	/* Data is not compressed */
#ifdef H5Z_DEBUG
	H5Z_g[method].uncomp.over += src_nbytes;
#endif
	HDmemcpy (dst, src, src_nbytes);
	ret_value = src_nbytes;
	
    } else if (NULL==(ufunc=H5Z_g[method].uncompress)) {
	/* No uncompress function */
	HGOTO_ERROR (H5E_COMP, H5E_UNSUPPORTED, 0,
		     "uncompression method is not supported");
	
    } else if (0==(ret_value=(ufunc)(comp->flags, comp->cd_size,
				     comp->client_data, src_nbytes,
				     src, dst_nbytes, dst))) {
	/* Uncompress failed */
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "uncompression failed");
    }

 done:
#ifdef H5Z_DEBUG
    H5Z_g[method].uncomp.nbytes += dst_nbytes;
    if (0==ret_value) H5Z_g[method].uncomp.failed += dst_nbytes;
    H5_timer_end (&(H5Z_g[method].uncomp.timer), &timer);
#endif

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_register
 *
 * Purpose:	Same as the public version except this one allows compression
 *		methods to be set for predefined method numbers <16.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Z_register (H5Z_method_t method, const char *name, H5Z_func_t cfunc,
	      H5Z_func_t ufunc)
{
    FUNC_ENTER (H5Z_register, FAIL);

    assert (method>=0 && method<=H5Z_USERDEF_MAX);
    H5MM_xfree (H5Z_g[method].name);
    H5Z_g[method].name = H5MM_xstrdup (name);
    H5Z_g[method].compress = cfunc;
    H5Z_g[method].uncompress = ufunc;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_zlib_c
 *
 * Purpose:	Compress SRC_NBYTES bytes from SRC into at most DST_NBYTES of
 *		DST using the compression level as specified in FLAGS.
 *
 * Return:	Success:	Number of bytes compressed into DST limited
 *				by DST_NBYTES.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_zlib_c (unsigned int flags, size_t __unused__ cd_size,
	    const void __unused__ *client_data, size_t src_nbytes,
	    const void *src, size_t dst_nbytes, void *dst/*out*/)
{
    size_t	ret_value = 0;
#if defined(HAVE_LIBZ) && defined(HAVE_ZLIB_H)
    const Bytef	*z_src = (const Bytef*)src;
    Bytef	*z_dst = (Bytef*)dst;
    uLongf	z_dst_nbytes = (uLongf)dst_nbytes;
    uLong	z_src_nbytes = (uLong)src_nbytes;
    int		level = flags % 10;
    int		status;
#endif
    
    FUNC_ENTER (H5Z_zlib_c, 0);

#if defined(HAVE_LIBZ) && defined (HAVE_ZLIB_H)
    status = compress2 (z_dst, &z_dst_nbytes, z_src, z_src_nbytes, level);
    if (Z_BUF_ERROR==status) {
	ret_value = dst_nbytes;
    } else if (Z_MEM_ERROR==status) {
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "deflate memory error");
    } else if (Z_OK!=status) {
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "deflate error");
    } else {
	ret_value = z_dst_nbytes;
    }
#else
    HGOTO_ERROR (H5E_COMP, H5E_UNSUPPORTED, 0,
		 "hdf5 was not compiled with zlib-1.0.2 or better");
#endif

 done:
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_zlib_u
 *
 * Purpose:	Uncompress SRC_NBYTES from SRC into at most DST_NBYTES of
 *		DST.
 *
 * Return:	Success:	Number of bytes returned in DST.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_zlib_u (unsigned int __unused__ flags, size_t __unused__ cd_size,
	    const void __unused__ *client_data, size_t src_nbytes,
	    const void *src, size_t dst_nbytes, void *dst/*out*/)
{
    size_t	ret_value = 0;
#ifdef HAVE_ZLIB_H
    const Bytef	*z_src = (const Bytef*)src;
    Bytef	*z_dst = (Bytef*)dst;
    uLongf	z_dst_nbytes = (uLongf)dst_nbytes;
    uLong	z_src_nbytes = (uLong)src_nbytes;
    int		status;
#endif
    
    FUNC_ENTER (H5Z_zlib_u, 0);

#if defined(HAVE_LIBZ) && defined (HAVE_ZLIB_H)
    status = uncompress (z_dst, &z_dst_nbytes, z_src, z_src_nbytes);
    if (Z_BUF_ERROR==status) {
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0,
		     "deflate destination buffer was too small");
    } else if (Z_MEM_ERROR==status) {
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "deflate memory error");
    } else if (Z_DATA_ERROR==status) {
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "deflate corrupted data");
    } else if (Z_OK!=status) {
	HGOTO_ERROR (H5E_COMP, H5E_CANTINIT, 0, "deflate error");
    }
    ret_value = z_dst_nbytes;
#else
    HGOTO_ERROR (H5E_COMP, H5E_UNSUPPORTED, 0,
		 "hdf5 was not compiled with zlib-1.0.2 or better");
#endif

 done:
    FUNC_LEAVE (ret_value);
}
