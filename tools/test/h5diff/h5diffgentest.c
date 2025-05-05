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

#include "h5diffgentest.h"

/*-------------------------------------------------------------------------
 * Program: h5diffgentest
 *
 * Purpose: generate files for h5diff testing
 *
 *-------------------------------------------------------------------------
 */

#define FILE1  "h5diff_basic1.h5"
#define FILE2  "h5diff_basic2.h5"
#define FILE3  "h5diff_types.h5"
#define FILE4  "h5diff_dtypes.h5"
#define FILE5  "h5diff_attr1.h5"
#define FILE6  "h5diff_attr2.h5"
#define FILE6a "h5diff_attr3.h5"
#define FILE7  "h5diff_dset1.h5"
#define FILE8  "h5diff_dset2.h5"
#define FILE8A "h5diff_dset3.h5"
#define FILE9  "h5diff_hyper1.h5"
#define FILE10 "h5diff_hyper2.h5"
#define FILE11 "h5diff_empty.h5"
#define FILE12 "h5diff_links.h5"
#define FILE13 "h5diff_softlinks.h5"
#define FILE14 "h5diff_linked_softlink.h5"
#define FILE15 "h5diff_extlink_src.h5"
#define FILE16 "h5diff_extlink_trg.h5"
#define FILE17 "h5diff_ext2softlink_src.h5"
#define FILE18 "h5diff_ext2softlink_trg.h5"
#define FILE19 "h5diff_dset_zero_dim_size1.h5"
#define FILE20 "h5diff_dset_zero_dim_size2.h5"
#define FILE21 "h5diff_dset_idx1.h5"
#define FILE22 "h5diff_dset_idx2.h5"
#define FILE23 "h5diff_onion_dset_1d.h5"
#define FILE24 "h5diff_onion_objs.h5"
#define FILE25 "h5diff_onion_dset_ext.h5"

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

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: main program
 *
 *-------------------------------------------------------------------------
 */

int
main(void)
{
    test_basic(FILE1, FILE2, FILE11);

    test_types(FILE3);
    test_datatypes(FILE4);

    /* generate 2 files, the second call creates a similar file with differences */
    test_attributes(FILE5, 0);
    test_attributes(FILE6, 1);
    /* generate file with string datatypes swapped */
    test_attributes(FILE6a, 2);

    /* test attributes with verbose level */
    test_attributes_verbose_level(ATTR_VERBOSE_LEVEL_FILE1, ATTR_VERBOSE_LEVEL_FILE2);

    /* generate 2 files, the second call creates a similar file with differences */
    test_datasets(FILE7, 0);
    test_datasets(FILE8, 1);
    test_datasets(FILE8A, 2);

    /* generate 2 files, the second call creates a similar file with differences */
    test_hyperslab(FILE9, 0);
    test_hyperslab(FILE10, 1);

    test_link_name(FILE12);

    test_soft_links(FILE13);

    test_linked_softlinks(FILE14);

    test_external_links(FILE15, FILE16);

    test_ext2soft_links(FILE17, FILE18);

    /* generate 2 files, the second call creates a similar file with differences */
    test_special_datasets(FILE19, 0);
    test_special_datasets(FILE20, 1);

    /*
     * Generate 2 files: FILE21 with old format; FILE22 with new format
     *     Create 2 datasets in each file:
     *      One dataset: chunked layout, w/o filters, fixed dimension
     *      One dataset: chunked layout,  w/ filters, fixed dimension
     */
    gen_dataset_idx(FILE21, 0);
    gen_dataset_idx(FILE22, 1);

    test_dangle_links(DANGLE_LINK_FILE1, DANGLE_LINK_FILE2);

    test_group_recurse(GRP_RECURSE_FILE1, GRP_RECURSE_FILE2);
    test_group_recurse2();

    test_exclude_obj1(EXCLUDE_FILE1_1, EXCLUDE_FILE1_2);
    test_exclude_obj2(EXCLUDE_FILE2_1, EXCLUDE_FILE2_2);
    test_exclude_obj3(EXCLUDE_FILE3_1, EXCLUDE_FILE3_2);

    /* diff various multiple vlen and fixlen string types in a compound dataset */
    test_comp_vlen_strings(COMP_VL_STRS_FILE, "group", 1);
    test_comp_vlen_strings(COMP_VL_STRS_FILE, "group_copy", 0);

    /* diff when invalid enum values are present.
     * This will probably grow to involve more extensive testing of
     * enums so it has been given its own test file and test (apart
     * from the basic type testing).
     */
    test_enums(ENUM_INVALID_VALUES);

    /* -------------------------------------------------
     * Create test files with dataset and attribute with container types
     * (array, vlen) with multiple nested compound types.
     */
    /* file1 */
    test_comps_array(COMPS_COMPLEX1, "dset1", "attr1", 0, 1);
    test_comps_vlen(COMPS_COMPLEX1, "dset2", "attr2", 0, 0);
    test_comps_array_vlen(COMPS_COMPLEX1, "dset3", "attr3", 0, 0);
    test_comps_vlen_arry(COMPS_COMPLEX1, "dset4", "attr4", 0, 0);
    /* file2 */
    test_comps_array(COMPS_COMPLEX2, "dset1", "attr1", 5, 1);
    test_comps_vlen(COMPS_COMPLEX2, "dset2", "attr2", 5, 0);
    test_comps_array_vlen(COMPS_COMPLEX2, "dset3", "attr3", 5, 0);
    test_comps_vlen_arry(COMPS_COMPLEX2, "dset4", "attr4", 5, 0);

    /*-------------------------------------------------
     * Create test files with non-comparable dataset and attributes with
     * comparable datasets and attributes.  All the comparables should display
     * differences.
     */
    test_data_nocomparables(NON_COMPARBLES1, 0);
    test_data_nocomparables(NON_COMPARBLES2, 5);

    /* common objects (same name) with different object types. HDFFV-7644 */
    test_objs_nocomparables(NON_COMPARBLES1, NON_COMPARBLES2);

    /* string dataset and attribute. HDFFV-10028 */
    test_objs_strings(DIFF_STRINGS1, DIFF_STRINGS2);

    /* double dataset and epsilion. HDFFV-10897 */
    test_double_epsilon(DIFF_EPS1, DIFF_EPS2);

    /* Generate the files for testing Onion VFD */
    test_onion_1d_dset(FILE23);
    test_onion_create_delete_objects(FILE24);
    test_onion_dset_extension(FILE25);

    return 0;
}