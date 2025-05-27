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

#ifndef H5DIFF_GENTEST_H
#define H5DIFF_GENTEST_H

#include "hdf5.h"

#define H5DIFF_FILE1  "h5diff_basic1.h5"
#define H5DIFF_FILE2  "h5diff_basic2.h5"
#define H5DIFF_FILE3  "h5diff_types.h5"
#define H5DIFF_FILE4  "h5diff_dtypes.h5"
#define H5DIFF_FILE5  "h5diff_attr1.h5"
#define H5DIFF_FILE6  "h5diff_attr2.h5"
#define H5DIFF_FILE6a "h5diff_attr3.h5"
#define H5DIFF_FILE7  "h5diff_dset1.h5"
#define H5DIFF_FILE8  "h5diff_dset2.h5"
#define H5DIFF_FILE8A "h5diff_dset3.h5"
#define H5DIFF_FILE9  "h5diff_hyper1.h5"
#define H5DIFF_FILE10 "h5diff_hyper2.h5"
#define H5DIFF_FILE11 "h5diff_empty.h5"
#define H5DIFF_FILE12 "h5diff_links.h5"
#define H5DIFF_FILE13 "h5diff_softlinks.h5"
#define H5DIFF_FILE14 "h5diff_linked_softlink.h5"
#define H5DIFF_FILE15 "h5diff_extlink_src.h5"
#define H5DIFF_FILE16 "h5diff_extlink_trg.h5"
#define H5DIFF_FILE17 "h5diff_ext2softlink_src.h5"
#define H5DIFF_FILE18 "h5diff_ext2softlink_trg.h5"
#define H5DIFF_FILE19 "h5diff_dset_zero_dim_size1.h5"
#define H5DIFF_FILE20 "h5diff_dset_zero_dim_size2.h5"
#define H5DIFF_FILE21 "h5diff_dset_idx1.h5"
#define H5DIFF_FILE22 "h5diff_dset_idx2.h5"
#define H5DIFF_FILE23 "h5diff_onion_dset_1d.h5"
#define H5DIFF_FILE24 "h5diff_onion_objs.h5"
#define H5DIFF_FILE25 "h5diff_onion_dset_ext.h5"

#define DANGLE_LINK_FILE1 "h5diff_danglelinks1.h5"
#define DANGLE_LINK_FILE2 "h5diff_danglelinks2.h5"
#define GRP_RECURSE_FILE1 "h5diff_grp_recurse1.h5"
#define GRP_RECURSE_FILE2 "h5diff_grp_recurse2.h5"

/* same structure, same obj name with different value */
#define EXCLUDE_FILE1_1 "h5diff_exclude1-1.h5"
#define EXCLUDE_FILE1_2 "h5diff_exclude1-2.h5"
/* different structure and obj names */
#define EXCLUDE_FILE2_1 "h5diff_exclude2-1.h5"
#define EXCLUDE_FILE2_2 "h5diff_exclude2-2.h5"
/* only one file has unique objs  */
#define EXCLUDE_FILE3_1 "h5diff_exclude3-1.h5"
#define EXCLUDE_FILE3_2 "h5diff_exclude3-2.h5"

/* compound type with multiple vlen string types */
#define COMP_VL_STRS_FILE "h5diff_comp_vl_strs.h5"
/* attribute compare with verbose level */
#define ATTR_VERBOSE_LEVEL_FILE1 "h5diff_attr_v_level1.h5"
#define ATTR_VERBOSE_LEVEL_FILE2 "h5diff_attr_v_level2.h5"

/* file containing valid/invalid enum value mix */
#define ENUM_INVALID_VALUES "h5diff_enum_invalid_values.h5"
/* file with container types (array,vlen) with multiple compounds */
#define COMPS_COMPLEX1 "compounds_array_vlen1.h5"
#define COMPS_COMPLEX2 "compounds_array_vlen2.h5"
/* non-comparable dataset and attribute */
#define NON_COMPARBLES1 "non_comparables1.h5"
#define NON_COMPARBLES2 "non_comparables2.h5"
/* string dataset and attribute */
#define DIFF_STRINGS1 "h5diff_strings1.h5"
#define DIFF_STRINGS2 "h5diff_strings2.h5"
/* double dataset and epsilon */
#define DIFF_EPS1 "h5diff_eps1.h5"
#define DIFF_EPS2 "h5diff_eps2.h5"

/* tests called in main() */
int test_basic(const char *fname1, const char *fname2, const char *fname3);
int test_types(const char *fname);
int test_datatypes(const char *fname);
int test_attributes(const char *fname, int make_diffs);
int test_datasets(const char *fname, int make_diffs);
int test_special_datasets(const char *fname, int make_diffs);
int test_hyperslab(const char *fname, int make_diffs);
int test_link_name(const char *fname1);
int test_soft_links(const char *fname1);
int test_linked_softlinks(const char *fname1);
int test_external_links(const char *fname1, const char *fname2);
int test_ext2soft_links(const char *fname1, const char *fname2);
int test_dangle_links(const char *fname1, const char *fname2);
int test_group_recurse(const char *fname1, const char *fname2);
int test_group_recurse2(void);
int test_exclude_obj1(const char *fname1, const char *fname2);
int test_exclude_obj2(const char *fname1, const char *fname2);
int test_exclude_obj3(const char *fname1, const char *fname2);
int test_comp_vlen_strings(const char *fname1, const char *grp_name, int is_file_new);
int test_attributes_verbose_level(const char *fname1, const char *fname2);
int test_enums(const char *fname);

int gen_dataset_idx(const char *file, int format);

void test_comps_array(const char *fname, const char *dset, const char *attr, int diff, int is_file_new);
void test_comps_vlen(const char *fname, const char *dset, const char *attr, int diff, int is_file_new);
void test_comps_array_vlen(const char *fname, const char *dset, const char *attr, int diff, int is_file_new);
void test_comps_vlen_arry(const char *fname, const char *dset, const char *attr, int diff, int is_file_new);
void test_data_nocomparables(const char *fname, int diff);
void test_objs_nocomparables(const char *fname1, const char *fname2);
void test_objs_strings(const char *fname, const char *fname2);
void test_double_epsilon(const char *fname1, const char *fname2);

/* Generate the files for testing Onion VFD */
int test_onion_1d_dset(const char *fname);
int test_onion_create_delete_objects(const char *fname);
int test_onion_dset_extension(const char *fname);

#endif /* H5DIFF_GENTEST_H */