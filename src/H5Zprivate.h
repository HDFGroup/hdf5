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

struct H5O_pline_t; /*forward decl*/

herr_t H5Z_register(H5Z_filter_t id, const char *comment, H5Z_func_t filter);
herr_t H5Z_append(struct H5O_pline_t *pline, H5Z_filter_t filter, uintn flags,
		  size_t cd_nelmts, const unsigned int cd_values[]);
herr_t H5Z_pipeline(H5F_t *f, const struct H5O_pline_t *pline, uintn flags,
		    uintn *filter_mask/*in,out*/, size_t *nbytes/*in,out*/,
		    size_t *buf_size/*in,out*/, void **buf/*in,out*/);

#endif
