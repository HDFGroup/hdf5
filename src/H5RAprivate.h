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

__DLL__ herr_t H5RA_close(H5RA_t *ra);
__DLL__ H5RA_t *H5RA_create(H5G_entry_t *loc, const char *name, H5T_t *type,
			    const H5D_create_t *dcpl);
__DLL__ H5RA_t *H5RA_open(H5G_entry_t *loc, const char *name);
__DLL__ htri_t H5RA_isa(H5G_entry_t *ent);
__DLL__ herr_t H5RA_write(H5RA_t *ra, hssize_t start_row, hsize_t nrows,
			  H5T_t *type, hsize_t size[], void *buf[]);
__DLL__ herr_t H5RA_read(H5RA_t *ra, hssize_t start_row, hsize_t nrows,
			 H5T_t *type, hsize_t size[], void *buf[]);
__DLL__ H5G_entry_t *H5RA_entof(H5RA_t *ra);

#endif
