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

#include "hdf5.h"
#include "H5private.h"

#include "h5gentest.h"
#include "h5copygentest.h"
#include "h5diffgentest.h"
#include "h5dumpgentest.h"
#include "h5format_convertgentest.h"
#include "h5jamgentest.h"
#include "h5repackgentest.h"
#include "h5statgentest.h"
#include "h5repartgentest.h"

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

#define NON_V3_FILE    "h5fc_non_v3.h5"
#define EDGE_V3_FILE   "h5fc_edge_v3.h5"
#define ERR_LEVEL_FILE "h5fc_err_level.h5"

/* not used yet
#define UBTXT1 "u0.txt"
*/
#define UBTXT2 "u10.txt"
#define UBTXT3 "u511.txt"
#define UBTXT4 "u512.txt"
#define UBTXT5 "u513.txt"

/* tall is same as dumper test */
#define H5JAM_FILE7 "tall.h5"
#define H5JAM_FILE8 "twithub.h5"
#define H5JAM_FILE9 "twithub513.h5"

static const char *H5FC_FILENAME[] = {"h5fc_ext1_i.h5",   /* 0 */
                                      "h5fc_ext1_s.h5",   /* 1 */
                                      "h5fc_ext1_f.h5",   /* 2 */
                                      "h5fc_ext2_is.h5",  /* 3 */
                                      "h5fc_ext2_if.h5",  /* 4 */
                                      "h5fc_ext2_sf.h5",  /* 5 */
                                      "h5fc_ext3_isf.h5", /* 6 */
                                      "h5fc_ext_none.h5", /* 7 */
                                      NULL};

#define NEWGRAT_FILE      "h5stat_newgrat.h5"
#define IDX_FILE          "h5stat_idx.h5"
#define THRESHOLD_FILE    "h5stat_threshold.h5"
#define ERR_REFCOUNT_FILE "h5stat_err_refcount.h5"

#define H5COPY_UDFILTER_FILE  "tudfilter.h5"
#define H5COPY_UDFILTER_FILE2 "tudfilter2.h5"
#define H5DUMP_UDFILTER_FILE  "tudfilter.h5"

static int
gen_h5copy_files(void)
{
    Test_Obj_Copy();
    Test_Ref_Copy();
    Test_Extlink_Copy();
    gent_udfilter(H5COPY_UDFILTER_FILE);
    gent_udfilter(H5COPY_UDFILTER_FILE2);

    return EXIT_SUCCESS;
}

static int
gen_h5diff_files(void)
{
    int nerrors = 0;

    nerrors += (test_basic(H5DIFF_FILE1, H5DIFF_FILE2, H5DIFF_FILE11) < 0 ? 1 : 0);

    nerrors += (test_types(H5DIFF_FILE3) < 0 ? 1 : 0);
    nerrors += (test_datatypes(H5DIFF_FILE4) < 0 ? 1 : 0);

    /* generate 2 files, the second call creates a similar file with differences */
    nerrors += (test_attributes(H5DIFF_FILE5, 0) < 0 ? 1 : 0);
    nerrors += (test_attributes(H5DIFF_FILE6, 1) < 0 ? 1 : 0);
    /* generate file with string datatypes swapped */
    nerrors += (test_attributes(H5DIFF_FILE6a, 2) < 0 ? 1 : 0);

    /* test attributes with verbose level */
    nerrors += (test_attributes_verbose_level(ATTR_VERBOSE_LEVEL_FILE1, ATTR_VERBOSE_LEVEL_FILE2) < 0 ? 1 : 0);

    /* generate 2 files, the second call creates a similar file with differences */
    nerrors += (test_datasets(H5DIFF_FILE7, 0) < 0 ? 1 : 0);
    nerrors += (test_datasets(H5DIFF_FILE8, 1) < 0 ? 1 : 0);
    nerrors += (test_datasets(H5DIFF_FILE8A, 2) < 0 ? 1 : 0);

    /* generate 2 files, the second call creates a similar file with differences */
    nerrors += (test_hyperslab(H5DIFF_FILE9, 0) < 0 ? 1 : 0);
    nerrors += (test_hyperslab(H5DIFF_FILE10, 1) < 0 ? 1 : 0);

    nerrors += (test_link_name(H5DIFF_FILE12) < 0 ? 1 : 0);
    nerrors += (test_soft_links(H5DIFF_FILE13) < 0 ? 1 : 0);

    nerrors += (test_linked_softlinks(H5DIFF_FILE14) < 0 ? 1 : 0);

    nerrors += (test_external_links(H5DIFF_FILE15, H5DIFF_FILE16) < 0 ? 1 : 0);

    nerrors += (test_ext2soft_links(H5DIFF_FILE17, H5DIFF_FILE18) < 0 ? 1 : 0);

    /* generate 2 files, the second call creates a similar file with differences */
    nerrors += (test_special_datasets(H5DIFF_FILE19, 0) < 0 ? 1 : 0);
    nerrors += (test_special_datasets(H5DIFF_FILE20, 1) < 0 ? 1 : 0);

    /*
     * Generate 2 files: H5DIFF_FILE21 with old format; H5DIFF_FILE22 with new format
     *     Create 2 datasets in each file:
     *      One dataset: chunked layout, w/o filters, fixed dimension
     *      One dataset: chunked layout,  w/ filters, fixed dimension
     */
    nerrors += (gen_dataset_idx(H5DIFF_FILE21, 0) < 0 ? 1 : 0);
    nerrors += (gen_dataset_idx(H5DIFF_FILE22, 1) < 0 ? 1 : 0);

    nerrors += (test_dangle_links(DANGLE_LINK_FILE1, DANGLE_LINK_FILE2) < 0 ? 1 : 0);

    nerrors += (test_group_recurse(GRP_RECURSE_FILE1, GRP_RECURSE_FILE2) < 0 ? 1 : 0);
    nerrors += (test_group_recurse2() < 0 ? 1 : 0);

    nerrors += (test_exclude_obj1(EXCLUDE_FILE1_1, EXCLUDE_FILE1_2) < 0 ? 1 : 0);
    nerrors += (test_exclude_obj2(EXCLUDE_FILE2_1, EXCLUDE_FILE2_2) < 0 ? 1 : 0);
    nerrors += (test_exclude_obj3(EXCLUDE_FILE3_1, EXCLUDE_FILE3_2) < 0 ? 1 : 0);

    /* diff various multiple vlen and fixlen string types in a compound dataset */
    nerrors += (test_comp_vlen_strings(COMP_VL_STRS_FILE, "group", 1) < 0 ? 1 : 0);
    nerrors += (test_comp_vlen_strings(COMP_VL_STRS_FILE, "group_copy", 0) < 0 ? 1 : 0);

    /* diff when invalid enum values are present.
     * This will probably grow to involve more extensive testing of
     * enums so it has been given its own test file and test (apart
     * from the basic type testing).
     */
    nerrors += (test_enums(ENUM_INVALID_VALUES) < 0 ? 1 : 0);

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
    nerrors += (test_onion_1d_dset(H5DIFF_FILE23) < 0 ? 1 : 0);
    nerrors += (test_onion_create_delete_objects(H5DIFF_FILE24) < 0 ? 1 : 0);
    nerrors += (test_onion_dset_extension(H5DIFF_FILE25) < 0 ? 1 : 0);

    return nerrors;
}

static int
gen_h5dump_files(void)
{
    int nerrors = 0;

    gent_group();
    gent_attribute();
    gent_softlink();
    nerrors += (gent_softlink2() < 0 ? 1 : 0);
    gent_dataset();
    gent_hardlink();
    gent_extlink();
    gent_udlink();
    gent_compound_dt();
    gent_all();
    gent_loop();
    gent_dataset2();
    gent_compound_dt2();
    gent_loop2();
    gent_many();
    gent_str();
    gent_str2();
    gent_enum();
    gent_objref();
    gent_datareg();
    gent_attrreg();
    gent_nestcomp();
    gent_opaque();
    gent_bitfields();
    gent_vldatatypes();
    gent_vldatatypes2();
    gent_vldatatypes3();
    gent_vldatatypes4();
    gent_vldatatypes5();
    gent_array1_big();
    gent_array1();
    gent_array2();
    gent_array3();
    gent_array4();
    gent_array5();
    gent_array6();
    gent_array7();
    gent_array8();
    gent_empty();
    gent_group_comments();
    gent_split_file();
    gent_family();
    gent_multi();
    gent_large_objname();
    gent_vlstr();
    gent_vlenstr_array();
    gent_char();
    gent_attr_all();
    gent_compound_complex();
    gent_compound_complex2();
    gent_named_dtype_attr();
    gent_null_space();
    gent_zero_dim_size();

    gent_filters();
    gent_fvalues();
    gent_fcontents();
    gent_string();
    gent_aindices();
    gent_longlinks();
    nerrors += (gent_ldouble() < 0 ? 1 : 0);
    nerrors += (gent_ldouble_scalar() < 0 ? 1 : 0);
    gent_binary();
    gent_bigdims();
    gent_hyperslab();
    gent_group_creation_order();
    gent_attr_creation_order();
    gent_fpformat();
    gent_extlinks();
    gent_fs_strategy_threshold();
    gent_packedbits();
    gent_dataset_idx();
    gent_attr_intsize();
    gent_charsets();
    gent_compound_intsizes();
    gent_compound_attr_intsizes();

    gent_nested_compound_dt();
    gent_intscalars();
    gent_attr_intscalars();
    gent_string_scalars();
    gent_compound_int_array();
    gent_compound_ints();
    gent_intattrscalars();
    gent_intsattrs();
    gent_floatsattrs();
    gent_bitnopaquefields();
    gent_nodata();

    gent_intsfourdims();
    gent_null_space_group();

    gent_udfilter(H5DUMP_UDFILTER_FILE);

    gent_err_attr_dspace();

    /* Generate the files for testing Onion VFD */
    nerrors += (gent_onion_1d_dset() < 0 ? 1 : 0);
    nerrors += (gent_onion_create_delete_objects() < 0 ? 1 : 0);
    nerrors += (gent_onion_dset_extension() < 0 ? 1 : 0);

#ifdef H5_HAVE__FLOAT16
    gent_float16();
    gent_float16_be();
#endif

#ifdef H5_HAVE_COMPLEX_NUMBERS
    gent_complex();
    gent_complex_be();
#endif

    return nerrors;
}

static int
gen_h5fc_files(void)
{
    unsigned i, new_format;

    /* Generate a non-latest-format file with v3 superblock */
    gen_non(NON_V3_FILE);

    /* Generate a new format file with a no-filter-edge-chunk dataset */
    gen_edge(EDGE_V3_FILE);

    /* Generate a new format file with 'K' value of 1 in H5Pset_istore_k() */
    gen_err_level(ERR_LEVEL_FILE);

    /* Generate old/new format file with/without messages in the superblock extension */
    for (new_format = false; new_format <= true; new_format++) {
        for (i = 0; i < 8; i++) {
            char filename[50];

            memset(filename, 0, sizeof(filename));
            if (!new_format)
                strcat(filename, "old_");
            strcat(filename, H5FC_FILENAME[i]);

            gen_ext(filename, new_format, i);
        } /* end for */
    }     /* end for */

    return EXIT_SUCCESS;
}

static int
gen_h5jam_files(void)
{
    int nerrors = 0;

    nerrors += (create_textfile(UBTXT2, 10) < 0 ? 1 : 0);
    nerrors += (create_textfile(UBTXT3, 511) < 0 ? 1 : 0);
    nerrors += (create_textfile(UBTXT4, 512) < 0 ? 1 : 0);
    nerrors += (create_textfile(UBTXT5, 513) < 0 ? 1 : 0);

    nerrors += (gent_ub(H5JAM_FILE7, 0, 0) < 0 ? 1 : 0);
    nerrors += (gent_ub(H5JAM_FILE8, 512, PATTERN_LEN) < 0 ? 1 : 0);
    nerrors += (gent_ub(H5JAM_FILE9, 1024, 513) < 0 ? 1 : 0);

    return nerrors;
}

static int
gen_h5repack_files(void)
{
    int nerrors = 0;
    int i = 0;

    for (i = 0; i < 2; i++) {
        bool external = (i & 1) ? true : false;
        nerrors += (generate_int32le_1d(external) < 0 ? 1 : 0);
        nerrors += (generate_int32le_2d(external) < 0 ? 1 : 0);
        nerrors += (generate_int32le_3d(external) < 0 ? 1 : 0);
        nerrors += (generate_uint8be(external) < 0 ? 1 : 0);
        nerrors += (generate_f32le(external) < 0 ? 1 : 0);
    } /* end for external data storage or not */

    return nerrors;
}

/*
 * The following two test files are generated with older versions
 * of the library for HDFFV-10333.  They are used for testing in
 * testh5stat.sh.in.
 *
 * (1) h5stat_err_old_layout.h5
 *     This file is generated with the 1.6 library so that a file
 *     with a version 2 layout message is created.
 *     Then a "0" is written to the "dimension" field in the layout
 *     message to trigger the error.
 *     This is to verify HDFFV-10333 that h5stat will exit gracefully
 *     when encountered error similar to H5O__layout_decode in the
 *     jira issue.
 *
 * (2) h5stat_err_old_fill.h5
 *     This file is generated with the 1.4 library so that a file
 *     with an old fill value message is created.
 *     Then an illegal size is written to the "size" fild in the
 *     fill value message to trigger the error.
 *     This is to verify HDFFV-10333 that h5stat will exit gracefully
 *     when encountered error similar to H5O_fill_old_decode in the
 *     jira issue.
 */
static int
gen_h5stat_files(void)
{
    int nerrors = 0;

    nerrors += gen_newgrat_file(NEWGRAT_FILE) < 0 ? 1 : 0;
    nerrors += gen_threshold_file(THRESHOLD_FILE) < 0 ? 1 : 0;

    /* Generate an HDF file to test for datasets with Fixed Array indexing */
    nerrors += gen_idx_file(IDX_FILE) < 0 ? 1 : 0;

    /* Generate a file with a refcount message ID */
    nerrors += gen_err_refcount(ERR_REFCOUNT_FILE) < 0 ? 1 : 0;

    return nerrors;
}

static int
gen_h5repart_files(void)
{
    gent_repart_family();

    return EXIT_SUCCESS;
}

/*
 * Generate the binary hdf5 files used for tools tests
 */
int
main(int argc, char *argv[])
{
    int  i;
    bool run_all      = true;
    bool run_h5copy   = false;
    bool run_h5diff   = false;
    bool run_h5dump   = false;
    bool run_h5fc     = false;
    bool run_h5jam    = false;
    bool run_h5repack = false;
    bool run_h5stat   = false;
    bool run_h5repart = false;

    /* Parse command line arguments */
    if (argc > 1) {
        run_all = false;

        for (i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                printf("Usage: %s [options]\n", argv[0]);
                printf("Generate HDF5 test files for various tools.\n\n");
                printf("Options:\n");
                printf("  -h, --help     Display this help message\n");
                printf("  -all           Generate all test files. Default if no options provided.\n");
                printf("  -h5copy        Generate h5copy test files\n");
                printf("  -h5diff        Generate h5diff test files\n");
                printf("  -h5dump        Generate h5dump test files\n");
                printf("  -h5fc          Generate h5fc test files\n");
                printf("  -h5jam         Generate h5jam test files\n");
                printf("  -h5repack      Generate h5repack test files\n");
                printf("  -h5stat        Generate h5stat test files\n");
                printf("  -h5repart      Generate h5repart test files\n");
                return EXIT_SUCCESS;
            }
            else if (strcmp(argv[i], "-h5copy") == 0) {
                run_h5copy = true;
            }
            else if (strcmp(argv[i], "-h5diff") == 0) {
                run_h5diff = true;
            }
            else if (strcmp(argv[i], "-h5dump") == 0) {
                run_h5dump = true;
            }
            else if (strcmp(argv[i], "-h5fc") == 0) {
                run_h5fc = true;
            }
            else if (strcmp(argv[i], "-h5jam") == 0) {
                run_h5jam = true;
            }
            else if (strcmp(argv[i], "-h5repack") == 0) {
                run_h5repack = true;
            }
            else if (strcmp(argv[i], "-h5stat") == 0) {
                run_h5stat = true;
            }
            else if (strcmp(argv[i], "-h5repart") == 0) {
                run_h5repart = true;
            }
            else if (strcmp(argv[i], "-all") == 0) {
                run_all = true;
            }
            else {
                printf("Unknown option: %s\n", argv[i]);
                printf("Use -h or --help for usage information\n");
                return EXIT_FAILURE;
            }
        }
    }

    /* If no specific options were selected or -all was specified, run all generators */
    if (run_all) {
        gen_h5copy_files();
        gen_h5diff_files();
        gen_h5dump_files();
        gen_h5fc_files();
        gen_h5jam_files();
        gen_h5repack_files();
        gen_h5stat_files();
        gen_h5repart_files();
    }
    else {
        if (run_h5copy) {
            gen_h5copy_files();
        }
        if (run_h5diff) {
            gen_h5diff_files();
        }
        if (run_h5dump) {
            gen_h5dump_files();
        }
        if (run_h5fc) {
            gen_h5fc_files();
        }
        if (run_h5jam) {
            gen_h5jam_files();
        }
        if (run_h5repack) {
            gen_h5repack_files();
        }
        if (run_h5stat) {
            gen_h5stat_files();
        }
        if (run_h5repart) {
            gen_h5repart_files();
        }
    }

    return EXIT_SUCCESS;
}
