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

#ifndef H5DUMP_GENTEST_H
#define H5DUMP_GENTEST_H

#include "hdf5.h"

#define H5DUMP_UDFILTER_FILE "tudfilter.h5"

void gent_group(void);
void gent_dataset(void);
void gent_dataset2(void);
void gent_attribute(void);
void gent_softlink(void);
int  gent_softlink2(void);
void gent_hardlink(void);
void gent_extlink(void);
void gent_udlink(void);
void gent_compound_dt(void);
void gent_compound_dt2(void);
void gent_all(void);
void gent_loop(void);
void gent_loop2(void);
void gent_many(void);

void gent_str(void);
void gent_str2(void);
void gent_enum(void);
void gent_objref(void);
void gent_datareg(void);
void gent_attrreg(void);
void gent_nestcomp(void);
void gent_opaque(void);
void gent_bitfields(void);
void gent_vldatatypes(void);
void gent_vldatatypes2(void);
void gent_vldatatypes3(void);
void gent_vldatatypes4(void);
void gent_vldatatypes5(void);
void gent_array1_big(void);
void gent_array1(void);
void gent_array2(void);
void gent_array3(void);
void gent_array4(void);
void gent_array5(void);
void gent_array6(void);
void gent_array7(void);
void gent_array8(void);
void gent_empty(void);
void gent_group_comments(void);
void gent_split_file(void);
void gent_family(void);

void gent_multi(void);
void gent_large_objname(void);
void gent_vlstr(void);
void gent_char(void);

void gent_attr_all(void);

void gent_compound_complex(void);
void gent_named_dtype_attr(void);
void gent_null_space(void);
void gent_zero_dim_size(void);

void gent_filters(void);

void gent_fcontents(void);
void gent_fvalues(void);
void gent_string(void);
void gent_aindices(void);
void gent_longlinks(void);
int  gent_ldouble(void);
int  gent_ldouble_scalar(void);
void gent_binary(void);
void gent_bigdims(void);
void gent_hyperslab(void);
void gent_group_creation_order(void);
void gent_attr_creation_order(void);
void gent_fpformat(void);
void gent_extlinks(void);
void gent_fs_strategy_threshold(void);
void gent_dataset_idx(void);
void gent_packedbits(void);
void gent_attr_intsize(void);
void gent_nodata(void);
void gent_charsets(void);
void gent_compound_intsizes(void);
void gent_compound_attr_intsizes(void);
void gent_nested_compound_dt(void);
void gent_intscalars(void);
void gent_attr_intscalars(void);
void gent_string_scalars(void);
void gent_compound_int_array(void);
void gent_compound_ints(void);
void gent_intattrscalars(void);
void gent_intsattrs(void);
void gent_floatsattrs(void);
void gent_bitnopaquefields(void);
void gent_intsfourdims(void);
void gent_compound_complex2(void);
void gent_vlenstr_array(void);
void gent_udfilter(const char *filename);

void gent_null_space_group(void);
void gent_err_attr_dspace(void);

int gent_onion_1d_dset(void);
int gent_onion_create_delete_objects(void);
int gent_onion_dset_extension(void);

void gent_float16(void);
void gent_float16_be(void);

void gent_complex(void);
void gent_complex_be(void);

void gent_trefer_attr(void);
void gent_tattr4_be(void);
void gent_tno_subset(void);
void gent_trefer_compat(void);
void gent_trefer_grp(void);
void gent_trefer_obj_del(void);
void gent_trefer_obj(void);
void gent_trefer_param(void);
void gent_trefer_reg(void);
void gent_trefer_reg_1d(void);
#endif /* H5DUMP_GENTEST_H */