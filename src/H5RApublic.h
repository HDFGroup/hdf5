/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, August 25, 1998
 */
#ifndef _H5RApublic_H
#define _H5RApublic_H

#include <H5Ipublic.h>

#ifdef __cplusplus
extern "C" {
#endif

HDF5API hid_t H5RAcreate(hid_t loc_id, const char *name, hid_t type_id, hid_t plist_id);
HDF5API hid_t H5RAopen(hid_t loc_id, const char *name);
HDF5API herr_t H5RAclose(hid_t array_id);
HDF5API herr_t H5RAwrite(hid_t array_id, hssize_t start_row, hsize_t nrows,
		hid_t type_id, hsize_t size[], void *buf[]);
HDF5API herr_t H5RAread(hid_t array_id, hssize_t start_row, hsize_t nrows,
	       hid_t type_id, hsize_t size[], void *buf[]);

#ifdef __cplusplus
}
#endif
#endif
