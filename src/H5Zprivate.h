/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, April 16, 1998
 */
#ifndef _H5Zprivate_H
#define _H5Zprivate_H

#include <H5Zpublic.h>

struct H5O_compress_t; /*forward decl*/

herr_t H5Z_register (H5Z_method_t method, const char *name,
		     H5Z_func_t compress, H5Z_func_t uncompress);
size_t H5Z_compress (const struct H5O_compress_t *compress, size_t nbytes,
		     const void *src, void *dst/*out*/);
size_t H5Z_uncompress (const struct H5O_compress_t *compress,
		       size_t src_nbytes, const void *src, size_t dst_nbytes,
		       void *dst/*out*/);

#endif
