/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5COPY_GENTEST_H
#define H5COPY_GENTEST_H

#include "hdf5.h"

#define H5COPY_UDFILTER_FILE  "tudfilter.h5"
#define H5COPY_UDFILTER_FILE2 "tudfilter2.h5"

void gent_simple(hid_t loc_id);
void gent_chunked(hid_t loc_id);
void gent_compact(hid_t loc_id);
void gent_compound(hid_t loc_id);
void gent_compressed(hid_t loc_id);
void gent_named_vl(hid_t loc_id);
void gent_nested_vl(hid_t loc_id);
void gent_att_compound_vlstr(hid_t loc_id);
void gent_datasets(hid_t loc_id);
void gent_empty_group(hid_t loc_id);
void gent_nested_datasets(hid_t loc_id);
void gent_nested_group(hid_t loc_id);

void Test_Obj_Copy(void);
void Test_Ref_Copy(void);
void Test_Extlink_Copy(void);

#endif /* H5COPY_GENTEST_H */