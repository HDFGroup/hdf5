/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, August 25, 1998
 */
#ifndef _H5Rpublic_H
#define _H5Rpublic_H

#include <H5Ipublic.h>

#ifdef __cplusplus
extern "C" {
#endif

hid_t H5Rcreate(hid_t loc_id, const char *name, hid_t type_id, hid_t plist_id);
hid_t H5Ropen(hid_t loc_id, const char *name);
herr_t H5Rclose(hid_t array_id);
herr_t H5Rwrite(hid_t array_id, hssize_t start_row, hsize_t nrows,
		hid_t type_id, hsize_t size[], void *buf[]);
herr_t H5Rread(hid_t array_id, hssize_t start_row, hsize_t nrows,
	       hid_t type_id, hsize_t size[], void *buf[]);

#ifdef __cplusplus
}
#endif
#endif
