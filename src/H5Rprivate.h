/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, August 25, 1998
 */
#ifndef _H5Rprivate_H
#define _H5Rprivate_H
#include <H5Rpublic.h>

#include <H5Dprivate.h>
#include <H5Gprivate.h>
#include <H5Tprivate.h>

typedef struct H5R_t H5R_t;

herr_t H5R_close(H5R_t *ra);
H5R_t *H5R_create(H5G_t *loc, const char *name, H5T_t *type,
		  const H5D_create_t *dcpl);
H5R_t *H5R_open(H5G_t *loc, const char *name);
herr_t H5R_write(H5R_t *ra, hssize_t start_row, hsize_t nrows, H5T_t *type,
		 hsize_t size[], void *buf[]);
herr_t H5R_read(H5R_t *ra, hssize_t start_row, hsize_t nrows, H5T_t *type,
		hsize_t size[], void *buf[]);

#endif
