/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, August 25, 1998
 */
#ifndef _H5RAprivate_H
#define _H5RAprivate_H
#include <H5RApublic.h>

#include <H5Dprivate.h>
#include <H5Gprivate.h>
#include <H5Tprivate.h>

typedef struct H5RA_t H5RA_t;

herr_t H5RA_close(H5RA_t *ra);
H5RA_t *H5RA_create(H5G_entry_t *loc, const char *name, H5T_t *type,
		  const H5D_create_t *dcpl);
H5RA_t *H5RA_open(H5G_entry_t *loc, const char *name);
htri_t H5RA_isa(H5G_entry_t *ent);
herr_t H5RA_write(H5RA_t *ra, hssize_t start_row, hsize_t nrows, H5T_t *type,
		 hsize_t size[], void *buf[]);
herr_t H5RA_read(H5RA_t *ra, hssize_t start_row, hsize_t nrows, H5T_t *type,
		hsize_t size[], void *buf[]);
H5G_entry_t *H5RA_entof(H5RA_t *ra);

#endif
