/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed. This constant sets the limit on the
 * size of that temporary buffer in bytes. For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */

#ifndef H5DIFF_GENTEST_H
#define H5DIFF_GENTEST_H

#include "hdf5.h"

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