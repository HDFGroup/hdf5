/*------------------------------------------------------------------------- 
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Git_H
#define _H5Git_H

#include <hdf5.h>

int H5Gn_members( hid_t loc_id, char *group_name );

herr_t H5Gget_obj_info_idx( hid_t loc_id, char *group_name, int idx, char **objname, int *type );

#endif /*_H5Git_H*/
