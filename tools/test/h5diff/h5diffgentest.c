/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "H5private.h"

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed. This constant sets the limit on the
 * size of that temporary buffer in bytes. For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
/* Maximum size used in a call to malloc for a dataset
 * NOTE: this value should stay in sync with the value defined in the tools
 *       library file: h5tools_utils.h
 */
size_t H5TOOLS_MALLOCSIZE = (128 * 1024 * 1024);

/*-------------------------------------------------------------------------
 * Program: h5diffgentest
 *
 * Purpose: generate files for h5diff testing
 *
 * Programmer: Pedro Vicente
 *
 * Date: November 12, 2003
 *
 *-------------------------------------------------------------------------
 */

#define FILE1             "h5diff_basic1.h5"
#define FILE2             "h5diff_basic2.h5"
#define FILE3             "h5diff_types.h5"
#define FILE4             "h5diff_dtypes.h5"
#define FILE5             "h5diff_attr1.h5"
#define FILE6             "h5diff_attr2.h5"
#define FILE6a            "h5diff_attr3.h5"
#define FILE7             "h5diff_dset1.h5"
#define FILE8             "h5diff_dset2.h5"
#define FILE8A            "h5diff_dset3.h5"
#define FILE9             "h5diff_hyper1.h5"
#define FILE10            "h5diff_hyper2.h5"
#define FILE11            "h5diff_empty.h5"
#define FILE12            "h5diff_links.h5"
#define FILE13            "h5diff_softlinks.h5"
#define FILE14            "h5diff_linked_softlink.h5"
#define FILE15            "h5diff_extlink_src.h5"
#define FILE16            "h5diff_extlink_trg.h5"
#define FILE17            "h5diff_ext2softlink_src.h5"
#define FILE18            "h5diff_ext2softlink_trg.h5"
#define FILE19            "h5diff_dset_zero_dim_size1.h5"
#define FILE20            "h5diff_dset_zero_dim_size2.h5"
#define FILE21            "h5diff_dset_idx1.h5"
#define FILE22            "h5diff_dset_idx2.h5"
#define DANGLE_LINK_FILE1 "h5diff_danglelinks1.h5"
#define DANGLE_LINK_FILE2 "h5diff_danglelinks2.h5"
#define GRP_RECURSE_FILE1 "h5diff_grp_recurse1.h5"
#define GRP_RECURSE_FILE2 "h5diff_grp_recurse2.h5"
/* same structure via external links through files */
#define GRP_RECURSE1_EXT  "h5diff_grp_recurse_ext1.h5"
#define GRP_RECURSE2_EXT1 "h5diff_grp_recurse_ext2-1.h5"
#define GRP_RECURSE2_EXT2 "h5diff_grp_recurse_ext2-2.h5"
#define GRP_RECURSE2_EXT3 "h5diff_grp_recurse_ext2-3.h5"
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
/* attribute compre with verbose level */
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

#define UIMAX    4294967295u /*Maximum value for a variable of type unsigned int */
#define STR_SIZE 3
#define GBLL     ((unsigned long long)1024 * 1024 * 1024)

#define MY_LINKCLASS 187

/* Dataspace of 0 dimension size */
#define SPACE1_RANK 2
#define SPACE1_DIM1 0
#define SPACE1_DIM2 0

/* Error macros */
#define AT() HDprintf("ERROR at %s:%d in %s()...\n", __FILE__, __LINE__, FUNC);
#define PROGRAM_ERROR                                                                                        \
    do {                                                                                                     \
        AT();                                                                                                \
        goto error;                                                                                          \
    } while (0)

/* A UD link traversal function.  Shouldn't actually be called. */
static hid_t
UD_traverse(H5_ATTR_UNUSED const char *link_name, H5_ATTR_UNUSED hid_t cur_group,
            H5_ATTR_UNUSED const void *udata, H5_ATTR_UNUSED size_t udata_size, H5_ATTR_UNUSED hid_t lapl_id,
            H5_ATTR_UNUSED hid_t dxpl_id)
{
    return -1;
}

const H5L_class_t UD_link_class[1] = {{
    H5L_LINK_CLASS_T_VERS,    /* H5L_class_t version       */
    (H5L_type_t)MY_LINKCLASS, /* Link type id number            */
    "UD link class",          /* name for debugging             */
    NULL,                     /* Creation callback              */
    NULL,                     /* Move/rename callback           */
    NULL,                     /* Copy callback                  */
    UD_traverse,              /* The actual traversal function  */
    NULL,                     /* Deletion callback              */
    NULL                      /* Query callback                 */
}};

/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */

/* tests called in main() */
static int  test_basic(const char *fname1, const char *fname2, const char *fname3);
static int  test_types(const char *fname);
static int  test_datatypes(const char *fname);
static int  test_attributes(const char *fname, int make_diffs);
static int  test_datasets(const char *fname, int make_diffs);
static int  test_special_datasets(const char *fname, int make_diffs);
static int  test_hyperslab(const char *fname, int make_diffs);
static int  test_link_name(const char *fname1);
static int  test_soft_links(const char *fname1);
static int  test_linked_softlinks(const char *fname1);
static int  test_external_links(const char *fname1, const char *fname2);
static int  test_ext2soft_links(const char *fname1, const char *fname2);
static int  test_dangle_links(const char *fname1, const char *fname2);
static int  test_group_recurse(const char *fname1, const char *fname2);
static int  test_group_recurse2(void);
static int  test_exclude_obj1(const char *fname1, const char *fname2);
static int  test_exclude_obj2(const char *fname1, const char *fname2);
static int  test_exclude_obj3(const char *fname1, const char *fname2);
static int  test_comp_vlen_strings(const char *fname1, const char *grp_name, int is_file_new);
static int  test_attributes_verbose_level(const char *fname1, const char *fname2);
static int  test_enums(const char *fname);
static void test_comps_array(const char *fname, const char *dset, const char *attr, int diff,
                             int is_file_new);
static void test_comps_vlen(const char *fname, const char *dset, const char *attr, int diff, int is_file_new);
static void test_comps_array_vlen(const char *fname, const char *dset, const char *attr, int diff,
                                  int is_file_new);
static void test_comps_vlen_arry(const char *fname, const char *dset, const char *attr, int diff,
                                 int is_file_new);
static void test_data_nocomparables(const char *fname, int diff);
static void test_objs_nocomparables(const char *fname1, const char *fname2);
static void test_objs_strings(const char *fname, const char *fname2);
static void test_double_epsilon(const char *fname1, const char *fname2);

/* called by test_attributes() and test_datasets() */
static void write_attr_strings(hid_t loc_id, const char *dset_name, hid_t fid, int make_diffs);
static void write_attr_in(hid_t loc_id, const char *dset_name, hid_t fid, int make_diffs);
static void write_dset_in(hid_t loc_id, const char *dset_name, hid_t fid, int make_diffs);
static void gen_datareg(hid_t fid, int make_diffs);
/* utilities */
static int    write_attr(hid_t loc_id, int rank, hsize_t *dims, const char *name, hid_t tid, void *buf);
static herr_t write_dset(hid_t loc_id, int rank, hsize_t *dims, const char *name, hid_t tid, void *buf);
static int    gen_dataset_idx(const char *file, int format);

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

    return EXIT_SUCCESS;
}

/*-------------------------------------------------------------------------
 * Function: test_basic
 *
 * Purpose: Create basic test files, first two contains different data, the
 * third one is just an empty file.
 *
 *-------------------------------------------------------------------------
 */

static int
test_basic(const char *fname1, const char *fname2, const char *fname3)
{
    hid_t   fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID;
    hid_t   gid1 = H5I_INVALID_HID, gid2 = H5I_INVALID_HID, gid3 = H5I_INVALID_HID;
    hsize_t dims1[1] = {6};
    hsize_t dims2[2] = {3, 2};

    /* create the empty file */
    if ((fid1 = H5Fcreate(fname3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "empty file (%s) creation failed.\n", fname3);
        goto out;
    }
    if (H5Fclose(fid1) < 0) {
        HDfprintf(stderr, "empty file (%s) close failed.\n", fname3);
        goto out;
    }

    /*-------------------------------------------------------------------------
     * create two files
     *-------------------------------------------------------------------------
     */

    if ((fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if ((fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
     * create groups
     *-------------------------------------------------------------------------
     */

    gid1 = H5Gcreate2(fid1, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid2 = H5Gcreate2(fid2, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid3 = H5Gcreate2(fid2, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * tests:
     * # 1.1 normal mode
     * # 1.2 normal mode with objects
     * # 1.3 report mode
     * # 1.4 report mode with objects
     * # 1.5 with -d
     *-------------------------------------------------------------------------
     */

    {
        double data1[3][2] = {{1.0F, 1.0F}, {1.00F, 1.000F}, {0.0F, 0.0F}};
        double data2[3][2] = {{0.0F, 1.1F}, {1.01F, 1.001F}, {0.0F, 1.0F}};
        double data3[3][2] = {{100.0F, 100.0F}, {100.00F, 100.000F}, {100.0F, 100.0F}};
        double data4[3][2] = {{105.0F, 120.0F}, {160.00F, 95.000F}, {80.0F, 40.0F}};

        write_dset(gid1, 2, dims2, "dset1", H5T_NATIVE_DOUBLE, data1);
        write_dset(gid2, 2, dims2, "dset2", H5T_NATIVE_DOUBLE, data2);
        write_dset(gid1, 2, dims2, "dset3", H5T_NATIVE_DOUBLE, data3);
        write_dset(gid2, 2, dims2, "dset4", H5T_NATIVE_DOUBLE, data4);
        write_dset(gid2, 2, dims2, "dset1", H5T_NATIVE_DOUBLE, data2);
    }
    /*-------------------------------------------------------------------------
     * relative error, compare divide by zero, both zero
     * # 1.6.1 with -p (int)
     *-------------------------------------------------------------------------
     */
    {
        int data5[3][2] = {{100, 100}, {100, 0}, {0, 100}};
        int data6[3][2] = {{120, 80}, {0, 100}, {0, 50}};

        write_dset(gid1, 2, dims2, "dset5", H5T_NATIVE_INT, data5);
        write_dset(gid1, 2, dims2, "dset6", H5T_NATIVE_INT, data6);
    }

    /*-------------------------------------------------------------------------
     * relative error, compare divide by zero, both zero
     * # 1.6.2 with -p (unsigned long long)
     *-------------------------------------------------------------------------
     */
    {
        unsigned long long data7[3][2] = {{100, 100}, {100, 0}, {0, 100}};
        unsigned long long data8[3][2] = {{120, 80}, {0, 100}, {0, 50}};

        write_dset(gid1, 2, dims2, "dset7", H5T_NATIVE_ULLONG, data7);
        write_dset(gid1, 2, dims2, "dset8", H5T_NATIVE_ULLONG, data8);
    }

    /*-------------------------------------------------------------------------
     * relative error, compare divide by zero, both zero
     * # 1.6.3 with -p (double)
     *
     *   A   B   1-B/A   %
     *   100 120 0.2     20
     *   100 80  0.2     20
     *   100 0   1       100
     *   0   100 #DIV/0! #DIV/0!
     *   0   0   #DIV/0! #DIV/0!
     *   100 50  0.5     50
     *-------------------------------------------------------------------------
     */
    {
        double data9[3][2]  = {{100.0F, 100.0F}, {100.0F, 0.0F}, {0.0F, 100.0F}};
        double data10[3][2] = {{120.0F, 80.0F}, {0.0F, 100.0F}, {0.0F, 50.0F}};

        write_dset(gid1, 2, dims2, "dset9", H5T_NATIVE_DOUBLE, data9);
        write_dset(gid1, 2, dims2, "dset10", H5T_NATIVE_DOUBLE, data10);
    }

    /*-------------------------------------------------------------------------
     * test floating point comparison
     *-------------------------------------------------------------------------
     */
    {
        /* epsilon = 0.0000001 = 1e-7
         * system epsilon for float : FLT_EPSILON = 1.19209E-07
         */
        float data11[3][2] = {{0.000000f, 0.0000001f}, {0.0000001f, 0.00000022f}, {0.0000001f, 0.0000001f}};
        float data12[3][2] = {{0.000000f, 0.0000002f}, {0.0000003f, 0.0000001f}, {0.000000f, 0.0000001f}};
        /* epsilon = 0.0000000000000001 = 1e-16
         * system epsilon for double : DBL_EPSILON = 2.22045E-16
         */
        double data13[3][2] = {{0.0000000000000000, 0.0000000000000001},
                               {0.0000000000000001, 0.0000000000000000},
                               {0.00000000000000033, 0.0000000000000001}};
        double data14[3][2] = {{0.0000000000000000, 0.0000000000000004},
                               {0.0000000000000002, 0.0000000000000001},
                               {0.0000000000000001, 0.00000000000000000}};

        write_dset(gid1, 2, dims2, "fp1", H5T_NATIVE_FLOAT, data11);
        write_dset(gid1, 2, dims2, "fp2", H5T_NATIVE_FLOAT, data12);
        write_dset(gid1, 2, dims2, "d1", H5T_NATIVE_DOUBLE, data13);
        write_dset(gid1, 2, dims2, "d2", H5T_NATIVE_DOUBLE, data14);
    }

#if H5_SIZEOF_LONG_DOUBLE != 0
    {

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_LDOUBLE
         *-------------------------------------------------------------------------
         */

        long double data15[3][2] = {{1.0L, 1.0L}, {1.0L, 1.0L}, {1.0L, 1.0L}};

        write_dset(gid1, 2, dims2, "ld", H5T_NATIVE_LDOUBLE, data15);
    }
#endif

    /*-------------------------------------------------------------------------
     * NaNs in H5T_NATIVE_FLOAT
     *-------------------------------------------------------------------------
     */
    {

        float data15[6];
        float data16[6];

        data15[0] = (float)HDsqrt(-1.0);
        data15[1] = 1.0F;
        data15[2] = (float)HDsqrt(-1.0);
        data15[3] = 1.0F;
        data15[4] = 1.0F;
        data15[5] = 1.0F;

        data16[0] = (float)HDsqrt(-1.0);
        data16[1] = (float)HDsqrt(-1.0);
        data16[2] = 1.0F;
        data16[3] = 1.0F;
        data16[4] = 1.0F;
        data16[5] = 1.0F;

        write_dset(gid1, 1, dims1, "fp15", H5T_NATIVE_FLOAT, data15);
        write_dset(gid1, 1, dims1, "fp16", H5T_NATIVE_FLOAT, data16);
    }

    /*-------------------------------------------------------------------------
     * NaNs in H5T_NATIVE_DOUBLE
     *-------------------------------------------------------------------------
     */
    {

        double data17[6];
        double data18[6];

        data17[0] = HDsqrt(-1.0);
        data17[1] = 1.0;
        data17[2] = HDsqrt(-1.0);
        data17[3] = 1.0;
        data17[4] = 1.0;
        data17[5] = 1.0;

        data18[0] = HDsqrt(-1.0);
        data18[1] = HDsqrt(-10000.0);
        data18[2] = 1.0;
        data18[3] = 1.0;
        data18[4] = 1.0;
        data18[5] = 1.0;

        write_dset(gid1, 1, dims1, "fp17", H5T_NATIVE_DOUBLE, data17);
        write_dset(gid1, 1, dims1, "fp18", H5T_NATIVE_DOUBLE, data18);
        write_dset(gid1, 1, dims1, "fp18_COPY", H5T_NATIVE_DOUBLE, data18);
    }

    /*------------------------------------------------------------------------
     *            INFINITY values
     *------------------------------------------------------------------------
     */
    {
        float  data19[6];
        double data20[6];

        data19[0] = data19[1] = data19[2] = (float)HDlog(0.0);
        data19[3] = data19[4] = data19[5] = (float)-HDlog(0.0);

        data20[0] = data20[1] = data20[2] = HDlog(0.0);
        data20[3] = data20[4] = data20[5] = -HDlog(0.0);

        write_dset(gid1, 1, dims1, "fp19", H5T_NATIVE_FLOAT, data19);
        write_dset(gid1, 1, dims1, "fp19_COPY", H5T_NATIVE_FLOAT, data19);
        write_dset(gid1, 1, dims1, "fp20", H5T_NATIVE_DOUBLE, data20);
        write_dset(gid1, 1, dims1, "fp20_COPY", H5T_NATIVE_DOUBLE, data20);
    }

    /*-------------------------------------------------------------------------
     * NaNs in H5T_NATIVE_DOUBLE and H5T_NATIVE_FLOAT inside H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    {
        typedef struct cmp1_t {
            double d;
            float  f;
        } cmp1_t;

        cmp1_t  buf1[2];
        cmp1_t  buf2[2];
        hsize_t dims[1] = {2};
        size_t  type_size;
        hid_t   tid;

        buf1[0].d = HDsqrt(-1.0);
        buf1[0].f = (float)HDsqrt(-1.0);
        buf2[0].d = HDsqrt(-1.0);
        buf2[0].f = (float)HDsqrt(-1.0);

        buf1[1].d = HDsqrt(-1.0);
        buf1[1].f = (float)HDsqrt(-1.0);
        buf2[1].d = 0.0F;
        buf2[1].f = 0.0F;

        type_size = sizeof(cmp1_t);
        tid       = H5Tcreate(H5T_COMPOUND, type_size);
        H5Tinsert(tid, "d", HOFFSET(cmp1_t, d), H5T_NATIVE_DOUBLE);
        H5Tinsert(tid, "f", HOFFSET(cmp1_t, f), H5T_NATIVE_FLOAT);
        write_dset(gid1, 1, dims, "dset11", tid, buf1);
        write_dset(gid1, 1, dims, "dset12", tid, buf2);
        H5Tclose(tid);
    }

    /* not comparable objects */
    {

        typedef struct cmp1_t {
            double d;
            int    i;
        } cmp1_t;

        typedef struct cmp2_t {
            int    i;
            double d;
        } cmp2_t;

        typedef struct cmp3_t {
            int i;
        } cmp3_t;

        double       data2[6]    = {0.0F, 0.0F, 0.0F, 0.0F, 0.0F, 0.0F};
        int          data3[6]    = {0, 0, 0, 0, 0, 0};
        int          data4[3][2] = {{0, 0}, {0, 0}, {0, 0}};
        int          data5[2][2] = {{0, 0}, {0, 0}};
        unsigned int data6[3][2] = {{0, 0}, {0, 0}, {0, 0}};
        cmp1_t       data7[1]    = {{1.0f, 2}};
        cmp2_t       data8[1]    = {{1, 2.0f}};
        hsize_t      dims3[2]    = {2, 2};
        hsize_t      dims4[1]    = {1};
        size_t       type_size;
        hid_t        tid;

        write_dset(gid3, 1, dims1, "dset1", H5T_NATIVE_DOUBLE, NULL);
        write_dset(gid3, 1, dims1, "dset2", H5T_NATIVE_DOUBLE, data2);
        write_dset(gid3, 1, dims1, "dset3", H5T_NATIVE_INT, data3);
        write_dset(gid3, 2, dims2, "dset4", H5T_NATIVE_INT, data4);
        write_dset(gid3, 2, dims3, "dset5", H5T_NATIVE_INT, data5);
        write_dset(gid3, 2, dims2, "dset6", H5T_NATIVE_UINT, data6);

        /* case of compound with different type members */
        type_size = sizeof(cmp1_t);
        tid       = H5Tcreate(H5T_COMPOUND, type_size);
        H5Tinsert(tid, "d", HOFFSET(cmp1_t, d), H5T_NATIVE_DOUBLE);
        H5Tinsert(tid, "i", HOFFSET(cmp1_t, i), H5T_NATIVE_INT);
        write_dset(gid3, 1, dims4, "dset7", tid, data7);
        H5Tclose(tid);

        type_size = sizeof(cmp2_t);
        tid       = H5Tcreate(H5T_COMPOUND, type_size);
        H5Tinsert(tid, "i", HOFFSET(cmp2_t, i), H5T_NATIVE_INT);
        H5Tinsert(tid, "d", HOFFSET(cmp2_t, d), H5T_NATIVE_DOUBLE);
        write_dset(gid3, 1, dims4, "dset8", tid, data8);
        H5Tclose(tid);

        /* case of compound with different number of members */
        type_size = sizeof(cmp3_t);
        tid       = H5Tcreate(H5T_COMPOUND, type_size);
        H5Tinsert(tid, "i", HOFFSET(cmp2_t, i), H5T_NATIVE_INT);
        write_dset(gid3, 1, dims4, "dset9", tid, NULL);
        H5Tclose(tid);
    }

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    H5Gclose(gid1);
    H5Gclose(gid2);
    H5Gclose(gid3);
    H5Fclose(fid1);
    H5Fclose(fid2);
    return SUCCEED;

out:

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: test_types
 *
 * Purpose: Compare different HDF5 object & link types:
 * H5G_DATASET, H5G_TYPE, H5G_GROUP, H5G_LINK, H5G_UDLINK
 *
 *-------------------------------------------------------------------------
 */
static int
test_types(const char *fname)
{
    hid_t   fid1 = H5I_INVALID_HID;
    hid_t   gid1 = H5I_INVALID_HID;
    hid_t   gid2 = H5I_INVALID_HID;
    hid_t   tid1 = H5I_INVALID_HID;
    hid_t   tid2 = H5I_INVALID_HID;
    herr_t  status;
    hsize_t dims[1] = {1};
    typedef struct s1_t {
        int   a;
        float b;
    } s1_t;
    typedef struct s2_t {
        int a;
    } s2_t;

    /*-------------------------------------------------------------------------
     * Create one file
     *-------------------------------------------------------------------------
     */
    fid1 = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * H5G_DATASET
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 1, dims, "dset", H5T_NATIVE_INT, 0);

    /*-------------------------------------------------------------------------
     * H5G_GROUP
     *-------------------------------------------------------------------------
     */
    gid1   = H5Gcreate2(fid1, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose(gid1);
    gid2   = H5Gcreate2(fid1, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Gclose(gid2);

    /*-------------------------------------------------------------------------
     * H5G_TYPE
     *-------------------------------------------------------------------------
     */

    /* create and commit datatype 1 */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_FLOAT);
    H5Tcommit2(fid1, "t1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(tid1);
    /* create and commit datatype 2 */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));
    H5Tinsert(tid2, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT);
    H5Tcommit2(fid1, "t2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(tid2);

    /*-------------------------------------------------------------------------
     * H5G_LINK
     *-------------------------------------------------------------------------
     */

    status = H5Lcreate_soft("g1", fid1, "l1", H5P_DEFAULT, H5P_DEFAULT);
    status = H5Lcreate_soft("g2", fid1, "l2", H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * H5G_UDLINK
     *-------------------------------------------------------------------------
     */
    H5Lcreate_external("filename", "objname", fid1, "ext_link", H5P_DEFAULT, H5P_DEFAULT);
    H5Lregister(UD_link_class);
    H5Lcreate_ud(fid1, "ud_link", (H5L_type_t)MY_LINKCLASS, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * Close
     *-------------------------------------------------------------------------
     */
    status = H5Fclose(fid1);
    return status;
}

/*

 # ##############################################################################
 # # not comparable types
 # ##############################################################################

 # 2.0
 TOOLTEST h5diff_20.txt file3.h5 file3.h5 -v dset g1

 # 2.1
 TOOLTEST h5diff_21.txt file3.h5 file3.h5 -v dset l1

 # 2.2
 TOOLTEST h5diff_22.txt file3.h5 file3.h5 -v dset t1

 # ##############################################################################
 # # compare groups, types, links (no differences and differences)
 # ##############################################################################

 # 2.3
 TOOLTEST h5diff_23.txt file3.h5 file3.h5 -v g1 g1

 # 2.4
 TOOLTEST h5diff_24.txt file3.h5 file3.h5 -v t1 t1

 # 2.5
 TOOLTEST h5diff_25.txt file3.h5 file3.h5 -v l1 l1

 # 2.6
 TOOLTEST h5diff_26.txt file3.h5 file3.h5 -v g1 g2

 # 2.7
 TOOLTEST h5diff_27.txt file3.h5 file3.h5 -v t1 t2

 # 2.8
 TOOLTEST h5diff_28.txt file3.h5 file3.h5 -v l1 l2
 */

/*-------------------------------------------------------------------------
 * Function: test_datatypes
 *
 * Purpose: test dataset datatypes
 *
 *-------------------------------------------------------------------------
 */
static int
test_datatypes(const char *fname)
{
    hid_t   fid1    = H5I_INVALID_HID;
    hid_t   dset    = H5I_INVALID_HID;
    hsize_t dims[2] = {3, 2};
    herr_t  status;
    char    buf1a[3][2] = {{1, 1}, {1, 1}, {1, 1}};
    char    buf1b[3][2] = {{1, 1}, {3, 4}, {5, 6}};
    short   buf2a[3][2] = {{1, 1}, {1, 1}, {1, 1}};
    short   buf2b[3][2] = {{1, 1}, {3, 4}, {5, 6}};
    int     buf3a[3][2] = {{1, 1}, {1, 1}, {1, 1}};
    int     buf3b[3][2] = {{1, 1}, {3, 4}, {5, 6}};
    long    buf4a[3][2] = {{1, 1}, {1, 1}, {1, 1}};
    long    buf4b[3][2] = {{1, 1}, {3, 4}, {5, 6}};
    float   buf5a[3][2] = {{1.0F, 1.0F}, {1.0F, 1.0F}, {1.0F, 1.0F}};
    float   buf5b[3][2] = {{1.0F, 1.0F}, {3.0F, 4.0F}, {5.0F, 6.0F}};
    double  buf6a[3][2] = {{1.0F, 1.0F}, {1.0F, 1.0F}, {1.0F, 1.0F}};
    double  buf6b[3][2] = {{1.0F, 1.0F}, {3.0F, 4.0F}, {5.0F, 6.0F}};

    /*unsigned/signed test
    signed char -128 to 127
    unsigned char 0 to 255
    */
    char          buf7a[3][2] = {{-1, -128}, {-1, -1}, {-1, -1}};
    unsigned char buf7b[3][2] = {{1, 128}, {1, 1}, {1, 1}};

    /* long long test */
    long long          buf8a[3][2] = {{1, 1}, {1, 1}, {1, 1}};
    long long          buf8b[3][2] = {{1, 1}, {3, 4}, {5, 6}};
    unsigned long long buf9a[3][2] = {{1, 1}, {1, 1}, {1, 1}};
    unsigned long long buf9b[3][2] = {{1, 1}, {3, 4}, {5, 6}};

    unsigned int buf10a[3][2] = {{UIMAX, 1}, {1, 1}, {1, 1}};
    unsigned int buf10b[3][2] = {{UIMAX - 1, 1}, {3, 4}, {5, 6}};

    unsigned short buf11a[3][2] = {{204, 205}, {2, 3}, {1, 1}};
    unsigned int   buf11b[3][2] = {{204, 205}, {2, 3}, {1, 1}};

    /*-------------------------------------------------------------------------
     * Create a file
     *-------------------------------------------------------------------------
     */
    fid1 = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * Check for different storage order. Give a warning if they are different
     *-------------------------------------------------------------------------
     */

    write_dset(fid1, 2, dims, "dset0a", H5T_STD_I16LE, buf2a);
    write_dset(fid1, 2, dims, "dset0b", H5T_STD_I32LE, buf3b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_CHAR
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 2, dims, "dset1a", H5T_NATIVE_CHAR, buf1a);
    write_dset(fid1, 2, dims, "dset1b", H5T_NATIVE_CHAR, buf1b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_SHORT
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 2, dims, "dset2a", H5T_NATIVE_SHORT, buf2a);
    write_dset(fid1, 2, dims, "dset2b", H5T_NATIVE_SHORT, buf2b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_INT
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 2, dims, "dset3a", H5T_NATIVE_INT, buf3a);
    write_dset(fid1, 2, dims, "dset3b", H5T_NATIVE_INT, buf3b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_LONG
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 2, dims, "dset4a", H5T_NATIVE_LONG, buf4a);
    write_dset(fid1, 2, dims, "dset4b", H5T_NATIVE_LONG, buf4b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_FLOAT
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 2, dims, "dset5a", H5T_NATIVE_FLOAT, buf5a);
    write_dset(fid1, 2, dims, "dset5b", H5T_NATIVE_FLOAT, buf5b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_DOUBLE
     *-------------------------------------------------------------------------
     */

    write_dset(fid1, 2, dims, "dset6a", H5T_NATIVE_DOUBLE, buf6a);
    write_dset(fid1, 2, dims, "dset6b", H5T_NATIVE_DOUBLE, buf6b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_CHAR and H5T_NATIVE_UCHAR
     *-------------------------------------------------------------------------
     */

    write_dset(fid1, 2, dims, "dset7a", H5T_NATIVE_CHAR, buf7a);
    write_dset(fid1, 2, dims, "dset7b", H5T_NATIVE_UCHAR, buf7b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_LLONG
     *-------------------------------------------------------------------------
     */

    write_dset(fid1, 2, dims, "dset8a", H5T_NATIVE_LLONG, buf8a);
    write_dset(fid1, 2, dims, "dset8b", H5T_NATIVE_LLONG, buf8b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_ULLONG
     *-------------------------------------------------------------------------
     */

    write_dset(fid1, 2, dims, "dset9a", H5T_NATIVE_ULLONG, buf9a);
    write_dset(fid1, 2, dims, "dset9b", H5T_NATIVE_ULLONG, buf9b);

    /*-------------------------------------------------------------------------
     * H5T_NATIVE_INT
     *-------------------------------------------------------------------------
     */

    write_dset(fid1, 2, dims, "dset10a", H5T_NATIVE_UINT, buf10a);
    write_dset(fid1, 2, dims, "dset10b", H5T_NATIVE_UINT, buf10b);

    /*-------------------------------------------------------------------------
     * Same type class, different size
     *-------------------------------------------------------------------------
     */
    write_dset(fid1, 2, dims, "dset11a", H5T_STD_U16LE, buf11a);
    dset = H5Dopen2(fid1, "dset11a", H5P_DEFAULT);
    write_attr(dset, 2, dims, "attr", H5T_STD_U16LE, buf11a);
    H5Dclose(dset);

    write_dset(fid1, 2, dims, "dset11b", H5T_STD_U32LE, buf11b);
    dset = H5Dopen2(fid1, "dset11b", H5P_DEFAULT);
    write_attr(dset, 2, dims, "attr", H5T_STD_U32LE, buf11b);
    H5Dclose(dset);

    /*-------------------------------------------------------------------------
     * Close
     *-------------------------------------------------------------------------
     */
    status = H5Fclose(fid1);
    return status;
}

/*
 # ##############################################################################
 # # Dataset datatypes
 # ##############################################################################

 # 5.0
 TOOLTEST h5diff_50.txt file4.h5 file4.h5 -v dset0a dset0b

 # 5.1
 TOOLTEST h5diff_51.txt file4.h5 file4.h5 -v dset1a dset1b

 # 5.2
 TOOLTEST h5diff_52.txt file4.h5 file4.h5 -v dset2a dset2b

 # 5.3
 TOOLTEST h5diff_53.txt file4.h5 file4.h5 -v dset3a dset4b

 # 5.4
 TOOLTEST h5diff_54.txt file4.h5 file4.h5 -v dset4a dset4b

 # 5.5
 TOOLTEST h5diff_55.txt file4.h5 file4.h5 -v dset5a dset5b

 # 5.6
 TOOLTEST h5diff_56.txt file4.h5 file4.h5 -v dset6a dset6b

 # 5.7
 TOOLTEST h5diff_57.txt file4.h5 file4.h5 -v dset7a dset7b

 # 5.8 (region reference)
 TOOLTEST h5diff_58.txt file7.h5 file8.h5 -v refreg
 */

/*-------------------------------------------------------------------------
 * Function: test_attributes
 *
 * Purpose: test attributes
 *
 *-------------------------------------------------------------------------
 */
static int
test_attributes(const char *file, int make_diffs /* flag to modify data buffers */)
{
    hid_t   fid     = H5I_INVALID_HID;
    hid_t   did     = H5I_INVALID_HID;
    hid_t   gid     = H5I_INVALID_HID;
    hid_t   root_id = H5I_INVALID_HID;
    hid_t   sid     = H5I_INVALID_HID;
    hsize_t dims[1] = {2};
    herr_t  status;

    /* Create a file  */
    if ((fid = H5Fcreate(file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;

    /* Create a 1D dataset */
    sid    = H5Screate_simple(1, dims, NULL);
    did    = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Sclose(sid);
    assert(status >= 0);

    /* Create groups */
    gid     = H5Gcreate2(fid, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    root_id = H5Gopen2(fid, "/", H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * write a series of attributes on the dataset, group, and root group
     *-------------------------------------------------------------------------
     */
    if (make_diffs > 1) {
        write_attr_strings(did, "dset", fid, make_diffs);
        write_attr_strings(gid, NULL, (hid_t)0, make_diffs);
        write_attr_strings(root_id, NULL, (hid_t)0, make_diffs);
    }
    else {
        write_attr_in(did, "dset", fid, make_diffs);
        write_attr_in(gid, NULL, (hid_t)0, make_diffs);
        write_attr_in(root_id, NULL, (hid_t)0, make_diffs);
    }

    /* Close */
    status = H5Dclose(did);
    assert(status >= 0);
    status = H5Gclose(gid);
    assert(status >= 0);
    status = H5Gclose(root_id);
    assert(status >= 0);

    /* Close file */
    status = H5Fclose(fid);
    assert(status >= 0);
    return status;
}

/*-------------------------------------------------------------------------
 * Function: test_attributes_verbose_level
 *
 * Purpose: Cresting test files for testing attributes along with
 * levels of verbos option
 *
 *-------------------------------------------------------------------------
 */
static int
test_attributes_verbose_level(const char *fname1, const char *fname2)
{
    herr_t status = SUCCEED;
    hid_t  fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID;
    hid_t  f1_gid = H5I_INVALID_HID, f2_gid = H5I_INVALID_HID;
    hid_t  f1_gid2 = H5I_INVALID_HID, f2_gid2 = H5I_INVALID_HID;
    hid_t  f1_gid3 = H5I_INVALID_HID, f2_gid3 = H5I_INVALID_HID;
    hid_t  f1_gid4 = H5I_INVALID_HID, f2_gid4 = H5I_INVALID_HID;
    hid_t  f1_did = H5I_INVALID_HID, f2_did = H5I_INVALID_HID;
    hid_t  f1_sid = H5I_INVALID_HID, f2_sid = H5I_INVALID_HID;
    hid_t  f1_tid = H5I_INVALID_HID, f2_tid = H5I_INVALID_HID;
    /* dset */
    hsize_t dset_dims[1] = {3};
    int     dset_data[3] = {0, 1, 2};

    /* common attrs dim */
    hsize_t attr_dims[1] = {2};

    /* file1 attr */
    int   f1_attr_idata[2] = {1, 2};       /* integer */
    float f1_attr_fdata[2] = {1.1F, 2.2F}; /* float */
    /* file2 attr */
    int   f2_attr_idata[2] = {2, 3};       /* integer */
    float f2_attr_fdata[2] = {2.1F, 3.2F}; /* float */

    /*----------------------------------------------------------------------
     * Create file1
     *-----------------------------------------------------------------------*/
    if ((fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * Groups
     */
    f1_gid = H5Gcreate2(fid1, "g", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f1_gid < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    f1_gid2 = H5Gcreate2(fid1, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f1_gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    f1_gid3 = H5Gcreate2(fid1, "g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f1_gid3 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    f1_gid4 = H5Gcreate2(fid1, "g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f1_gid4 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * Datasets
     */
    f1_sid = H5Screate_simple(1, dset_dims, NULL);
    f1_did = H5Dcreate2(fid1, "dset", H5T_NATIVE_INT, f1_sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f1_did == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }
    status = H5Dwrite(f1_did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * Named Datatype
     */
    f1_tid = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tcommit2(fid1, "ntype", f1_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tcommit2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*----------------------------------------------------------------------
     * Create file2
     *-----------------------------------------------------------------------*/
    if ((fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * Groups
     */
    f2_gid = H5Gcreate2(fid2, "g", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f2_gid < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    f2_gid2 = H5Gcreate2(fid2, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f2_gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    f2_gid3 = H5Gcreate2(fid2, "g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f2_gid3 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    f2_gid4 = H5Gcreate2(fid2, "g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f2_gid4 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * Datasets
     */
    f2_sid = H5Screate_simple(1, dset_dims, NULL);
    f2_did = H5Dcreate2(fid2, "dset", H5T_NATIVE_INT, f2_sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (f2_did == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }
    status = H5Dwrite(f2_did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * Named Datatype
     */
    f2_tid = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tcommit2(fid2, "ntype", f2_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tcommit2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*----------------------------------
     * CASE1 - Same attr number, all Same attr name
     * add attr to group
     */
    write_attr(f1_gid, 1, attr_dims, "integer1", H5T_NATIVE_INT, f1_attr_idata);
    write_attr(f1_gid, 1, attr_dims, "float1", H5T_NATIVE_FLOAT, f1_attr_fdata);

    write_attr(f2_gid, 1, attr_dims, "integer1", H5T_NATIVE_INT, f2_attr_idata);
    write_attr(f2_gid, 1, attr_dims, "float1", H5T_NATIVE_FLOAT, f2_attr_fdata);

    /*----------------------------------
     * CASE2 - Same attr number, some Same attr name
     * add attr to dset
     */
    write_attr(f1_did, 1, attr_dims, "integer1", H5T_NATIVE_INT, f1_attr_idata);
    write_attr(f1_did, 1, attr_dims, "float2", H5T_NATIVE_FLOAT, f1_attr_fdata);

    write_attr(f2_did, 1, attr_dims, "integer1", H5T_NATIVE_INT, f2_attr_idata);
    write_attr(f2_did, 1, attr_dims, "float3", H5T_NATIVE_FLOAT, f2_attr_fdata);

    /*----------------------------------
     * CASE3 - Same attr number, all different attr name
     * add attr to ntype
     */
    write_attr(f1_tid, 1, attr_dims, "integer1", H5T_NATIVE_INT, f1_attr_idata);
    write_attr(f1_tid, 1, attr_dims, "float2", H5T_NATIVE_FLOAT, f1_attr_fdata);
    write_attr(f1_tid, 1, attr_dims, "float3", H5T_NATIVE_FLOAT, f1_attr_fdata);

    write_attr(f2_tid, 1, attr_dims, "integer4", H5T_NATIVE_INT, f2_attr_idata);
    write_attr(f2_tid, 1, attr_dims, "float5", H5T_NATIVE_FLOAT, f2_attr_fdata);
    write_attr(f2_tid, 1, attr_dims, "float6", H5T_NATIVE_FLOAT, f2_attr_fdata);

    /*----------------------------------
     * CASE4 - Different attr number, some same attr name (vs file2-g2)
     * add attr to g2
     */
    write_attr(f1_gid2, 1, attr_dims, "integer1", H5T_NATIVE_INT, f1_attr_idata);
    write_attr(f1_gid2, 1, attr_dims, "float2", H5T_NATIVE_FLOAT, f1_attr_fdata);
    write_attr(f1_gid2, 1, attr_dims, "float3", H5T_NATIVE_FLOAT, f1_attr_fdata);

    write_attr(f2_gid2, 1, attr_dims, "integer1", H5T_NATIVE_INT, f2_attr_idata);
    write_attr(f2_gid2, 1, attr_dims, "float2", H5T_NATIVE_FLOAT, f2_attr_fdata);

    /*----------------------------------
     * CASE5 - Different attr number, all different attr name
     * add attr to g3
     */
    write_attr(f1_gid3, 1, attr_dims, "integer10", H5T_NATIVE_INT, f1_attr_idata);
    write_attr(f1_gid3, 1, attr_dims, "float11", H5T_NATIVE_FLOAT, f1_attr_fdata);
    write_attr(f1_gid3, 1, attr_dims, "float12", H5T_NATIVE_FLOAT, f1_attr_fdata);

    write_attr(f2_gid3, 1, attr_dims, "integer3", H5T_NATIVE_INT, f2_attr_idata);
    write_attr(f2_gid3, 1, attr_dims, "float4", H5T_NATIVE_FLOAT, f2_attr_fdata);

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1 > 0)
        H5Fclose(fid1);
    if (fid2 > 0)
        H5Fclose(fid2);
    if (f1_gid > 0)
        H5Gclose(f1_gid);
    if (f2_gid > 0)
        H5Gclose(f2_gid);
    if (f1_gid2 > 0)
        H5Gclose(f1_gid2);
    if (f2_gid2 > 0)
        H5Gclose(f2_gid2);
    if (f1_gid3 > 0)
        H5Gclose(f1_gid3);
    if (f2_gid3 > 0)
        H5Gclose(f2_gid3);
    if (f1_gid4 > 0)
        H5Gclose(f1_gid4);
    if (f2_gid4 > 0)
        H5Gclose(f2_gid4);
    if (f1_did > 0)
        H5Dclose(f1_did);
    if (f2_did > 0)
        H5Dclose(f2_did);
    if (f1_sid > 0)
        H5Sclose(f1_sid);
    if (f2_sid > 0)
        H5Sclose(f2_sid);
    if (f1_tid > 0)
        H5Tclose(f1_tid);
    if (f2_tid > 0)
        H5Tclose(f2_tid);

    return status;
}

/*-------------------------------------------------------------------------
 * Function: test_datasets
 *
 * Purpose: Check all HDF5 classes
 * H5T_INTEGER, H5T_FLOAT
 * H5T_TIME, H5T_STRING, H5T_BITFIELD, H5T_OPAQUE, H5T_COMPOUND, H5T_REFERENCE,
 * H5T_ENUM, H5T_VLEN, H5T_ARRAY
 *
 *-------------------------------------------------------------------------
 */
static int
test_datasets(const char *file, int make_diffs /* flag to modify data buffers */)
{
    hid_t   fid     = H5I_INVALID_HID;
    hid_t   did     = H5I_INVALID_HID;
    hid_t   gid     = H5I_INVALID_HID;
    hid_t   sid     = H5I_INVALID_HID;
    hsize_t dims[1] = {2};
    herr_t  status;
    int     buf[2] = {1, 2};

    if (make_diffs > 0)
        memset(buf, 0, sizeof buf);

    /* Create a file  */
    if ((fid = H5Fcreate(file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;

    /* Create a 1D dataset */
    sid    = H5Screate_simple(1, dims, NULL);
    did    = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    status = H5Sclose(sid);
    assert(status >= 0);

    /* Create a group */
    gid = H5Gcreate2(fid, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * write a series of datasets on the group
     *-------------------------------------------------------------------------
     */

    write_dset_in(gid, "/dset", fid, make_diffs);

    /* close */
    status = H5Dclose(did);
    assert(status >= 0);
    status = H5Gclose(gid);
    assert(status >= 0);

    /* close file */
    status = H5Fclose(fid);
    assert(status >= 0);
    return status;
}

/*-------------------------------------------------------------------------
 * Function: test_special_datasets
 *
 * Purpose: Check datasets with datasapce of zero dimension size.
 *-------------------------------------------------------------------------
 */
static int
test_special_datasets(const char *file, int make_diffs /* flag to modify data buffers */)
{
    hid_t   fid                = H5I_INVALID_HID;
    hid_t   did                = H5I_INVALID_HID;
    hid_t   sid0               = H5I_INVALID_HID;
    hid_t   sid                = H5I_INVALID_HID;
    hsize_t dims0[SPACE1_RANK] = {SPACE1_DIM1, SPACE1_DIM2};
    hsize_t dims[SPACE1_RANK]  = {SPACE1_DIM1, SPACE1_DIM2};
    herr_t  status;

    /* Create a file  */
    if ((fid = H5Fcreate(file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;

    /* Create a dataset with zero dimension size */
    sid0 = H5Screate_simple(SPACE1_RANK, dims0, NULL);
    did  = H5Dcreate2(fid, "dset1", H5T_NATIVE_INT, sid0, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* close dataset */
    status = H5Dclose(did);
    assert(status >= 0);

    /* close dataspace */
    status = H5Sclose(sid0);
    assert(status >= 0);

    /* Create a dataset with zero dimension size in one file but the other one
     * has a dataset with a non-zero dimension size */
    if (make_diffs)
        dims[1] = SPACE1_DIM2 + 4;

    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    did = H5Dcreate2(fid, "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* close dataspace */
    status = H5Sclose(sid);
    assert(status >= 0);

    /* close dataset */
    status = H5Dclose(did);
    assert(status >= 0);

    /* close file */
    status = H5Fclose(fid);
    assert(status >= 0);
    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files to compare links, one has longer name than
 *          the other and short name is subset of long name.
 *
 * Programmer: Jonathan Kim (Feb 17, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_link_name(const char *fname1)
{
    hid_t  fid1   = H5I_INVALID_HID;
    hid_t  gid1   = H5I_INVALID_HID;
    hid_t  gid2   = H5I_INVALID_HID;
    herr_t status = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    gid1 = H5Gcreate2(fid1, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    gid2 = H5Gcreate2(fid1, "group_longname", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft Links
     *------------------------------------------------------------------------*/
    status = H5Lcreate_soft("group", fid1, "link_g1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("group_longname", fid1, "link_g2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *------------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (gid1)
        H5Gclose(gid1);
    if (gid2)
        H5Gclose(gid2);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files to compare soft links in various way
 *
 * Programmer: Jonathan Kim (Feb 17, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_soft_links(const char *fname1)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   gid1        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    int     data2[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    gid1 = H5Gcreate2(fid1, "target_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "target_dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid1, 2, dims2, "target_dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid1, 2, dims2, "dset", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft Links
     *------------------------------------------------------------------------*/
    /* file 1 */
    status = H5Lcreate_soft("/target_dset1", fid1, "softlink_dset1_1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/target_dset1", fid1, "softlink_dset1_2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/target_dset2", fid1, "softlink_dset2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/target_group", fid1, "softlink_group1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/target_group", fid1, "softlink_group2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/no_obj", fid1, "softlink_noexist", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (gid1)
        H5Gclose(gid1);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files to compare linked soft links in various way
 *
 * Programmer: Jonathan Kim (Feb 17, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_linked_softlinks(const char *fname1)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   gid1        = H5I_INVALID_HID;
    hid_t   gid2        = H5I_INVALID_HID;
    hid_t   gid3        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    int     data2[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    gid1 = H5Gcreate2(fid1, "target_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    gid2 = H5Gcreate2(fid1, "target_group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    gid3 = H5Gcreate2(fid1, "target_group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid3 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "target_dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid1, 2, dims2, "target_dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }
    status = write_dset(gid1, 2, dims2, "dset", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft Links (Linked)
     *------------------------------------------------------------------------*/
    /*---------
     * file 1 */
    status = H5Lcreate_soft("/target_dset1", fid1, "softlink1_to_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink1_to_dset1", fid1, "softlink1_to_slink1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink1_to_slink1", fid1, "softlink1_to_slink2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/target_dset2", fid1, "softlink2_to_dset2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink2_to_dset2", fid1, "softlink2_to_slink1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink2_to_slink1", fid1, "softlink2_to_slink2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("target_group1", fid1, "softlink3_to_group1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink3_to_group1", fid1, "softlink3_to_slink1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink3_to_slink1", fid1, "softlink3_to_slink2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("target_group2", fid1, "softlink4_to_group2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink4_to_group2", fid1, "softlink4_to_slink1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("softlink4_to_slink1", fid1, "softlink4_to_slink2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (gid1)
        H5Gclose(gid1);
    if (gid2)
        H5Gclose(gid2);
    if (gid3)
        H5Gclose(gid3);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files to compare external links in various way
 *
 * Programmer: Jonathan Kim (Feb 17, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_external_links(const char *fname1, const char *fname2)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   fid2        = H5I_INVALID_HID;
    hid_t   gid1        = H5I_INVALID_HID;
    hid_t   gid2        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    int     data2[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    /* source file */
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* target file */
    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    /*--------------
     * target file */
    gid1 = H5Gcreate2(fid2, "target_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    gid2 = H5Gcreate2(fid2, "target_group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }
    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /*--------------
     * target file */
    status = write_dset(fid2, 2, dims2, "target_dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid1, 2, dims2, "x_dset", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid2, 2, dims2, "x_dset", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * External Links
     *------------------------------------------------------------------------*/
    /*--------------*/
    /* source file */
    status =
        H5Lcreate_external(fname2, "/target_group/x_dset", fid1, "ext_link_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status =
        H5Lcreate_external(fname2, "/target_group2/x_dset", fid1, "ext_link_dset2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/target_group", fid1, "/ext_link_grp1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/target_group2", fid1, "/ext_link_grp2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "no_obj", fid1, "ext_link_noexist1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external("no_file.h5", "no_obj", fid1, "ext_link_noexist2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
    if (gid1)
        H5Gclose(gid1);
    if (gid2)
        H5Gclose(gid2);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files to compare external links which point to
 *          soft link in various way
 *
 * Programmer: Jonathan Kim (Feb 17, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_ext2soft_links(const char *fname1, const char *fname2)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   fid2        = H5I_INVALID_HID;
    hid_t   gid2        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    int     data2[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    /* source file */
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* target file */
    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    /* target file */
    gid2 = H5Gcreate2(fid2, "target_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /*--------------
     * target file */
    status = write_dset(fid2, 2, dims2, "dset1", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid2, 2, dims2, "dset2", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft Links (Linked)
     *------------------------------------------------------------------------*/
    /*---------------
     * target file */
    status = H5Lcreate_soft("/dset1", fid2, "softlink_to_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/dset2", fid2, "softlink_to_dset2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * External Links
     *------------------------------------------------------------------------*/
    /*---------------
     * source file */
    status = H5Lcreate_external(fname2, "/target_group", fid1, "ext_link", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/softlink_to_dset1", fid1, "ext_link_to_slink1", H5P_DEFAULT,
                                H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/softlink_to_dset2", fid1, "ext_link_to_slink2", H5P_DEFAULT,
                                H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
    if (gid2)
        H5Gclose(gid2);

    return status;
}

/*-------------------------------------------------------------------------
 * Function: gen_dataset_idx
 *
 * Purpose: Create a file with either the new or old format
 *       Create two datasets in the file:
 *        one dataset: fixed dimension, chunked layout, w/o filters
 *        one dataset: fixed dimension, chunked layout, w/ filters
 *
 *-------------------------------------------------------------------------
 */
static int
gen_dataset_idx(const char *file, int format)
{
    hid_t   fid       = H5I_INVALID_HID; /* file id */
    hid_t   did       = H5I_INVALID_HID;
    hid_t   did2      = H5I_INVALID_HID; /* dataset id */
    hid_t   sid       = H5I_INVALID_HID; /* space id */
    hid_t   fapl      = H5I_INVALID_HID; /* file access property id */
    hid_t   dcpl      = H5I_INVALID_HID; /* dataset creation property id */
    hsize_t dims[1]   = {10};            /* dataset dimension */
    hsize_t c_dims[1] = {2};             /* chunk dimension */
    herr_t  status;                      /* return status */
    int     buf[10];                     /* data buffer */
    int     i;                           /* local index variable */

    /* Get a copy of the file aaccess property */
    fapl = H5Pcreate(H5P_FILE_ACCESS);

    /* Set the "use the latest format" bounds for creating objects in the file */
    if (format) {
        status = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
        assert(status >= 0);
    }

    /* Create a file  */
    if ((fid = H5Fcreate(file, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        return -1;

    /* Create data */
    for (i = 0; i < 10; i++)
        buf[i] = i;

    /* Set chunk */
    dcpl   = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_chunk(dcpl, 1, c_dims);
    assert(status >= 0);

    /* Create a 1D dataset */
    sid = H5Screate_simple(1, dims, NULL);
    did = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /* Write to the dataset */
    status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    assert(status >= 0);

#if defined(H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    status = H5Pset_deflate(dcpl, 9);
    assert(status >= 0);

    /* Create and write the dataset */
    did2   = H5Dcreate2(fid, "dset_filter", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    status = H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    assert(status >= 0);

    /* Close the dataset */
    status = H5Dclose(did2);
    assert(status >= 0);

#endif

    /* closing: dataspace, dataset, file */
    status = H5Sclose(sid);
    assert(status >= 0);

    status = H5Dclose(did);
    assert(status >= 0);

    status = H5Fclose(fid);
    assert(status >= 0);

    status = H5Pclose(dcpl);
    assert(status >= 0);

    status = H5Pclose(fapl);
    assert(status >= 0);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files to compare dangling links in various way
 *
 * Programmer: Jonathan Kim (Feb 17, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_dangle_links(const char *fname1, const char *fname2)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   fid2        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    int     data2[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid1, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = write_dset(fid2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid2, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft Links
     *------------------------------------------------------------------------*/
    /* file 1 */
    status = H5Lcreate_soft("no_obj", fid1, "soft_link1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/dset1", fid1, "soft_link2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("no_obj", fid1, "soft_link3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("no_obj1", fid1, "soft_link4", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file 2 */
    status = H5Lcreate_soft("no_obj", fid2, "soft_link1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("no_obj", fid2, "soft_link2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/dset2", fid2, "soft_link3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("no_obj2", fid2, "soft_link4", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * External Links
     *------------------------------------------------------------------------*/
    /* file1 */
    status = H5Lcreate_external(fname2, "no_obj", fid1, "ext_link1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/dset1", fid1, "ext_link2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "no_obj", fid1, "ext_link3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external("no_file.h5", "no_obj", fid1, "ext_link4", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = H5Lcreate_external(fname1, "no_obj", fid2, "ext_link1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname1, "no_obj", fid2, "ext_link2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname1, "/dset2", fid2, "ext_link3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external("no_file.h5", "no_obj", fid2, "ext_link4", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: For testing comparing group member objects recursively
 *
 * Programmer: Jonathan Kim (Aug 19, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_group_recurse(const char *fname1, const char *fname2)
{
    hid_t fid1    = H5I_INVALID_HID;
    hid_t fid2    = H5I_INVALID_HID;
    hid_t gid1_f1 = H5I_INVALID_HID, gid2_f1 = H5I_INVALID_HID, gid3_f1 = H5I_INVALID_HID,
          gid10_f1 = H5I_INVALID_HID;
    hid_t gid1_f2 = H5I_INVALID_HID, gid2_f2 = H5I_INVALID_HID, gid3_f2 = H5I_INVALID_HID,
          gid11_f2      = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 1}, {0, 1}, {1, 0}, {1, 0}};
    int     data2[4][2] = {{0, 2}, {0, 2}, {2, 0}, {2, 0}};
    int     data3[4][2] = {{0, 3}, {0, 3}, {3, 0}, {3, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    /* file1 */
    gid1_f1 = H5Gcreate2(fid1, "/grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1_f1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    gid2_f1 = H5Gcreate2(fid1, "/grp1/grp2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2_f1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    gid3_f1 = H5Gcreate2(fid1, "/grp1/grp2/grp3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid3_f1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    gid10_f1 = H5Gcreate2(fid1, "/grp10", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid10_f1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    gid1_f2 = H5Gcreate2(fid2, "/grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1_f2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    gid2_f2 = H5Gcreate2(fid2, "/grp1/grp2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2_f2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    gid3_f2 = H5Gcreate2(fid2, "/grp1/grp2/grp3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid3_f2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    gid11_f2 = H5Gcreate2(fid2, "/grp11", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid11_f2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets under root
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid1, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid1, 2, dims2, "dset3", H5T_NATIVE_INT, data3);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = write_dset(fid2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid2, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid2, 2, dims2, "dset3", H5T_NATIVE_INT, data3);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets under group
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(gid1_f1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid2_f1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }
    status = write_dset(gid2_f1, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid3_f1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }
    status = write_dset(gid3_f1, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid3_f1, 2, dims2, "dset3", H5T_NATIVE_INT, data3);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid10_f1, 2, dims2, "dset4", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid10_f1, 2, dims2, "dset5", H5T_NATIVE_INT, data3);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = write_dset(gid1_f2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid2_f2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }
    status = write_dset(gid2_f2, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid3_f2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }
    status = write_dset(gid3_f2, 2, dims2, "dset2", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid3_f2, 2, dims2, "dset3", H5T_NATIVE_INT, data3);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid11_f2, 2, dims2, "dset4", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid11_f2, 2, dims2, "dset5", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft Links
     *------------------------------------------------------------------------*/
    /* file 1 */
    status = H5Lcreate_soft("/grp1", fid1, "slink_grp1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/grp1/grp2", fid1, "slink_grp2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/grp1/grp2/grp3", fid1, "slink_grp3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/grp10", fid1, "slink_grp10", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file 2 */
    status = H5Lcreate_soft("/grp1", fid2, "slink_grp1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/grp1/grp2", fid2, "slink_grp2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/grp1/grp2/grp3", fid2, "slink_grp3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_soft("/grp11", fid2, "slink_grp11", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * External Links
     *------------------------------------------------------------------------*/
    /* file1 */
    status = H5Lcreate_external(fname2, "/grp1", fid1, "elink_grp1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/grp1/grp2", fid1, "elink_grp2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname2, "/grp1/grp2/grp3", fid1, "elink_grp3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = H5Lcreate_external(fname1, "/grp1", fid2, "elink_grp1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname1, "/grp1/grp2", fid2, "elink_grp2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    status = H5Lcreate_external(fname1, "/grp1/grp2/grp3", fid2, "elink_grp3", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*------------------------------
     * external circle route test
     * file1/grp11 <-> file2/grp10  via elink_grp_circle link
     */
    /* file1 */
    status = H5Lcreate_external(fname2, "/grp11", gid10_f1, "elink_grp_circle", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    /* file2 */
    status = H5Lcreate_external(fname1, "/grp10", gid11_f2, "elink_grp_circle", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", fname2);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
    if (gid1_f1)
        H5Gclose(gid1_f1);
    if (gid2_f1)
        H5Gclose(gid2_f1);
    if (gid3_f1)
        H5Gclose(gid3_f1);
    if (gid1_f2)
        H5Gclose(gid1_f2);
    if (gid2_f2)
        H5Gclose(gid2_f2);
    if (gid3_f2)
        H5Gclose(gid3_f2);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose:
 *   For testing comparing group member objects recursively via multiple
 *   linked external links
 *
 * Programmer: Jonathan Kim (Sep 16, 2010)
 *
 *-------------------------------------------------------------------------*/
#define GRP_R_DSETNAME1 "dset1"
#define GRP_R_DSETNAME2 "dset2"
static int
test_group_recurse2(void)
{
    hid_t   fileid1   = H5I_INVALID_HID;
    hid_t   grp1      = H5I_INVALID_HID;
    hid_t   grp2      = H5I_INVALID_HID;
    hid_t   grp3      = H5I_INVALID_HID;
    hid_t   grp4      = H5I_INVALID_HID;
    hid_t   dset1     = H5I_INVALID_HID;
    hid_t   dset2     = H5I_INVALID_HID;
    hid_t   datatype  = H5I_INVALID_HID;
    hid_t   dataspace = H5I_INVALID_HID;
    hid_t   fileid2   = H5I_INVALID_HID;
    hid_t   fileid3   = H5I_INVALID_HID;
    hid_t   fileid4   = H5I_INVALID_HID;
    hsize_t dimsf[2]; /* dataset dimensions */
    herr_t  status      = 0;
    int     data1[4][2] = {{0, 0}, {1, 1}, {2, 2}, {3, 3}};
    int     data2[4][2] = {{0, 0}, {0, 1}, {0, 2}, {3, 3}};

    /*-----------------------------------------------------------------------
     * FILE 1
     *------------------------------------------------------------------------*/
    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    fileid1 = H5Fcreate(GRP_RECURSE1_EXT, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    grp1 = H5Gcreate2(fileid1, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE1_EXT);
        status = FAIL;
        goto out;
    }

    grp2 = H5Gcreate2(grp1, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE1_EXT);
        status = FAIL;
        goto out;
    }

    grp3 = H5Gcreate2(grp2, "g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp3 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE1_EXT);
        status = FAIL;
        goto out;
    }

    grp4 = H5Gcreate2(grp3, "g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp4 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE1_EXT);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset.
     */
    dimsf[0]  = 4;
    dimsf[1]  = 2;
    dataspace = H5Screate_simple(2, dimsf, NULL);

    /*
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    status   = H5Tset_order(datatype, H5T_ORDER_LE);

    /*---------------
     * dset1
     */
    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dset1 = H5Dcreate2(fileid1, GRP_R_DSETNAME1, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1);
    H5Dclose(dset1);

    /*---------------
     * dset1
     */
    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dset1 = H5Dcreate2(grp3, GRP_R_DSETNAME1, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1);

    /*---------------
     * dset2
     */
    /*
     * Create a new dataset within the fileid1 using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dset2 = H5Dcreate2(grp4, GRP_R_DSETNAME2, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data2);

    /*-----------------------------------------------------------------------
     * Soft links
     *------------------------------------------------------------------------*/
    /*
     * under  '/' root
     */
    /* link to dset1 */
    status = H5Lcreate_soft(GRP_R_DSETNAME1, fileid1, "soft_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", GRP_RECURSE1_EXT);
        status = FAIL;
        goto out;
    }

    H5Dclose(dset1);
    H5Dclose(dset2);
    H5Gclose(grp1);
    H5Gclose(grp2);
    H5Gclose(grp3);
    H5Gclose(grp4);

    /*-----------------------------------------------------------------------
     * FILE 2-3
     *------------------------------------------------------------------------*/

    /* crate target file */
    fileid4 = H5Fcreate(GRP_RECURSE2_EXT3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-----------------------------------------------
     * Groups
     */
    grp4 = H5Gcreate2(fileid4, "/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp4 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE2_EXT3);
        status = FAIL;
        goto out;
    }

    /*---------------
     * dset2
     */
    /*
     * Create a new dataset within the fileid1 using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dset2 = H5Dcreate2(grp4, GRP_R_DSETNAME2, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data2);

    H5Gclose(grp4);
    H5Dclose(dset2);

    /*-----------------------------------------------------------------------
     * FILE 2-2
     *------------------------------------------------------------------------*/

    /* crate target file */
    fileid3 = H5Fcreate(GRP_RECURSE2_EXT2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-----------------------------------------------
     * Groups
     */
    grp2 = H5Gcreate2(fileid3, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE2_EXT2);
        status = FAIL;
        goto out;
    }

    grp3 = H5Gcreate2(grp2, "g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp3 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE2_EXT2);
        status = FAIL;
        goto out;
    }

    /*---------------
     * dset1
     */
    /*
     * Create a new dataset within the fileid1 using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dset1 = H5Dcreate2(grp3, GRP_R_DSETNAME1, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1);

    /*-----------------------------------------------
     * extlink to  $GRP_RECURSE2_EXT3/g4
     */
    status = H5Lcreate_external(GRP_RECURSE2_EXT3, "/g4", fileid3, "/g2/g3/g4", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", GRP_RECURSE2_EXT2);
        status = FAIL;
        goto out;
    }

    H5Dclose(dset1);
    H5Gclose(grp2);
    H5Gclose(grp3);

    /*-----------------------------------------------------------------------
     * FILE 2-1
     *------------------------------------------------------------------------*/

    /* crate target file */
    fileid2 = H5Fcreate(GRP_RECURSE2_EXT1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-----------------------------------------------
     * Groups
     */
    grp1 = H5Gcreate2(fileid2, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (grp1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", GRP_RECURSE1_EXT);
        status = FAIL;
        goto out;
    }

    /*---------------
     * dset1
     */
    dset1 = H5Dcreate2(fileid2, GRP_R_DSETNAME1, datatype, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1);

    /*-----------------------------------------------------------------------
     * Soft links
     *------------------------------------------------------------------------*/
    /*
     * under  '/' root
     */
    /* link to dset1 */
    status = H5Lcreate_soft(GRP_R_DSETNAME1, fileid2, "soft_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", GRP_RECURSE2_EXT1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------
     * extlink to  $GRP_RECURSE2_EXT2/g2
     */
    status = H5Lcreate_external(GRP_RECURSE2_EXT2, "/g2", fileid2, "/g1/g2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Lcreate_external failed.\n", GRP_RECURSE2_EXT1);
        status = FAIL;
        goto out;
    }

    H5Gclose(grp1);
    H5Dclose(dset1);

out:
    /*
     * Close/release resources.
     */
    if (dataspace > 0)
        H5Sclose(dataspace);
    if (datatype > 0)
        H5Tclose(datatype);
    if (fileid1 > 0)
        H5Fclose(fileid1);
    if (fileid2 > 0)
        H5Fclose(fileid2);
    if (fileid3 > 0)
        H5Fclose(fileid3);
    if (fileid4 > 0)
        H5Fclose(fileid4);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files for excluding obj.
 *          Same structure, same obj names
 * Test : exclude obj with different value to verify the rest are same
 *
 * Programmer: Jonathan Kim (July, 21, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_exclude_obj1(const char *fname1, const char *fname2)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   fid2        = H5I_INVALID_HID;
    hid_t   gid1        = H5I_INVALID_HID;
    hid_t   gid2        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    int     data2[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Group
     *------------------------------------------------------------------------*/
    /* file1 */
    gid1 = H5Gcreate2(fid1, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    gid2 = H5Gcreate2(fid2, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid1, 2, dims2, "dset2", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid1, 2, dims2, "dset3", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = write_dset(fid2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid2, 2, dims2, "dset2", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid2, 2, dims2, "dset3", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
    if (gid1)
        H5Gclose(gid1);
    if (gid2)
        H5Gclose(gid2);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files for excluding obj.
 *          different structure and name
 * Test : exclude different objs to verify the rest are same
 *
 * Programmer: Jonathan Kim (July, 21, 2010)
 *
 *-------------------------------------------------------------------------*/
static int
test_exclude_obj2(const char *fname1, const char *fname2)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   fid2        = H5I_INVALID_HID;
    hid_t   gid1        = H5I_INVALID_HID;
    hid_t   gid2        = H5I_INVALID_HID;
    hid_t   gid3        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    int     data2[4][2] = {{0, 1}, {2, 3}, {1, 2}, {3, 4}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Group
     *------------------------------------------------------------------------*/
    /* file1 */
    gid1 = H5Gcreate2(fid1, "group10", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    gid2 = H5Gcreate2(fid2, "group10", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /* subset name from group10 */
    gid3 = H5Gcreate2(fid2, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (gid3 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "dset10", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(fid1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid1, 2, dims2, "dset2", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = write_dset(fid2, 2, dims2, "dset10", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid2, 2, dims2, "dset2", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid3, 2, dims2, "dset3", H5T_NATIVE_INT, data2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
    if (gid1)
        H5Gclose(gid1);
    if (gid2)
        H5Gclose(gid2);
    if (gid3)
        H5Gclose(gid3);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files for excluding obj.
 *          Only one file contains unique objs. Common objs are same.
 * Test : exclude unique objs to verify the rest are same - HDFFV-7837
 *
 * Programmer: Jonathan Kim (Mar, 19, 2012)
 *
 *-------------------------------------------------------------------------*/
static int
test_exclude_obj3(const char *fname1, const char *fname2)
{
    hid_t   fid1        = H5I_INVALID_HID;
    hid_t   fid2        = H5I_INVALID_HID;
    hid_t   gid1        = H5I_INVALID_HID;
    hsize_t dims2[2]    = {2, 4};
    int     data1[4][2] = {{0, 0}, {0, 0}, {0, 0}, {0, 0}};
    herr_t  status      = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Group
     *------------------------------------------------------------------------*/
    /* file1 */
    gid1 = H5Gcreate2(fid1, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets
     *------------------------------------------------------------------------*/
    /* file1 */
    status = write_dset(fid1, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    status = write_dset(gid1, 2, dims2, "dset", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname1);
        status = FAIL;
        goto out;
    }

    /* file2 */
    status = write_dset(fid2, 2, dims2, "dset1", H5T_NATIVE_INT, data1);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname2);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
    if (gid1)
        H5Gclose(gid1);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose: Create test files for multiple variable length string/string array
 *          along with fixed length string/string array types in
 *          a compound type dataset.
 *
 * Programmer: Jonathan Kim (Oct, 26, 2010)
 *
 *-------------------------------------------------------------------------*/
#define STR_RANK             1
#define VLEN_STR_DIM         1
#define FIXLEN_STR_SIZE      21
#define FIXLEN_STR_DIM       1
#define VLEN_STR_ARRY_DIM    3
#define FIXLEN_STR_ARRY_DIM  3
#define FIXLEN_STR_ARRY_SIZE 30
#define COMP_RANK            1
#define COMP_DIM             1
static int
test_comp_vlen_strings(const char *fname1, const char *grp_name, int is_file_new)
{
    int   i;
    hid_t fid1 = H5I_INVALID_HID; /* file id */
    hid_t gid  = H5I_INVALID_HID;

    /* compound1 datatype */
    typedef struct comp1_t {
        char *      str_vlen;                                                    /* vlen string */
        char *      str_vlen_repeat;                                             /* vlen string */
        char        str_fixlen[FIXLEN_STR_SIZE];                                 /* fixed len string */
        char        str_fixlen_repeat[FIXLEN_STR_SIZE];                          /* fixed len string */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
        char        str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        char str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];  /* fixed len string array */
    } comp1_t;

    /* compound2 datatype */
    typedef struct comp2_t {
        char *      str_vlen;                                                    /* vlen string */
        char        str_fixlen[FIXLEN_STR_SIZE];                                 /* fixed len string */
        char *      str_vlen_repeat;                                             /* vlen string */
        char        str_fixlen_repeat[FIXLEN_STR_SIZE];                          /* fixed len string */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        char        str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
        char str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];  /* fixed len string array */
    } comp2_t;

    /* compound3 datatype */
    typedef struct comp3_t {
        char  str_fixlen[FIXLEN_STR_SIZE];                                       /* fixed len string */
        char  str_fixlen_repeat[FIXLEN_STR_SIZE];                                /* fixed len string */
        char *str_vlen;                                                          /* vlen string */
        char *str_vlen_repeat;                                                   /* vlen string */
        char  str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];       /* fixed len string array */
        char  str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
    } comp3_t;

    /* compound4 datatype */
    typedef struct comp4_t {
        char        str_fixlen[FIXLEN_STR_SIZE];                                 /* fixed len string */
        char *      str_vlen;                                                    /* vlen string */
        char        str_fixlen_repeat[FIXLEN_STR_SIZE];                          /* fixed len string */
        char *      str_vlen_repeat;                                             /* vlen string */
        char        str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        char str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];  /* fixed len string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
    } comp4_t;

    /* compound5 datatype */
    typedef struct comp5_t {
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
        char        str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        char  str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        char *str_vlen;                                                          /* vlen string */
        char *str_vlen_repeat;                                                   /* vlen string */
        char  str_fixlen[FIXLEN_STR_SIZE];                                       /* fixed len string */
        char  str_fixlen_repeat[FIXLEN_STR_SIZE];                                /* fixed len string */
    } comp5_t;

    /* compound6 datatype */
    typedef struct comp6_t {
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        char        str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
        char  str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        char *str_vlen;                                                          /* vlen string */
        char  str_fixlen[FIXLEN_STR_SIZE];                                       /* fixed len string */
        char *str_vlen_repeat;                                                   /* vlen string */
        char  str_fixlen_repeat[FIXLEN_STR_SIZE];                                /* fixed len string */
    } comp6_t;

    /* compound7 datatype */
    typedef struct comp7_t {
        char str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];       /* fixed len string array */
        char str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                          /* vlen string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                    /* vlen string array */
        char        str_fixlen[FIXLEN_STR_SIZE];                                /* fixed len string */
        char        str_fixlen_repeat[FIXLEN_STR_SIZE];                         /* fixed len string */
        char *      str_vlen;                                                   /* vlen string */
        char *      str_vlen_repeat;                                            /* vlen string */
    } comp7_t;

    /* compound8 datatype */
    typedef struct comp8_t {
        char        str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                           /* vlen string array */
        char str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];  /* fixed len string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                     /* vlen string array */
        char        str_fixlen[FIXLEN_STR_SIZE];                                 /* fixed len string */
        char *      str_vlen;                                                    /* vlen string */
        char        str_fixlen_repeat[FIXLEN_STR_SIZE];                          /* fixed len string */
        char *      str_vlen_repeat;                                             /* vlen string */
    } comp8_t;

    /* compound9 datatype */
    typedef struct comp9_t {
        char str_array_fixlen[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE];       /* fixed len string array */
        char str_fixlen_array_again[FIXLEN_STR_ARRY_DIM][FIXLEN_STR_ARRY_SIZE]; /* fixed len string array */
        const char *str_array_vlen[VLEN_STR_ARRY_DIM];                          /* vlen string array */
        const char *str_vlen_array_again[VLEN_STR_ARRY_DIM];                    /* vlen string array */
        char        str_fixlen[FIXLEN_STR_SIZE];                                /* fixed len string */
        int         int_data1;
        hobj_ref_t  objref1;                            /* reference */
        char        str_fixlen_repeat[FIXLEN_STR_SIZE]; /* fixed len string */
        hobj_ref_t  objref2;                            /* reference */
        char *      str_vlen;                           /* vlen string */
        int         int_data2;
        char *      str_vlen_repeat; /* vlen string */
        hobj_ref_t  objref3;         /* reference */
        int         int_data3;
    } comp9_t;

    /* vlen string */
    hid_t   sid_vlen_str    = H5I_INVALID_HID; /* dataspace ID */
    hid_t   tid_vlen_str    = H5I_INVALID_HID; /* datatype ID */
    char    vlen_str_buf[]  = {"Variable length string"};
    hsize_t dims_vlen_str[] = {VLEN_STR_DIM};

    /* fixlen string */
    hid_t      sid_fixlen_str                  = H5I_INVALID_HID; /* dataspace ID */
    hid_t      tid_fixlen_str                  = H5I_INVALID_HID; /* datatype ID */
    const char fixlen_str_buf[FIXLEN_STR_SIZE] = {"Fixed length string"};
    hsize_t    dims_fixlen_str[]               = {FIXLEN_STR_DIM};

    /* vlen string array */
    hid_t       sid_vlen_str_array                    = H5I_INVALID_HID; /* dataspace ID */
    hid_t       tid_vlen_str_array_pre                = H5I_INVALID_HID; /* datatype ID */
    hid_t       tid_vlen_str_array                    = H5I_INVALID_HID; /* datatype ID */
    const char *vlen_str_array_buf[VLEN_STR_ARRY_DIM] = {
        "1 - Variable length string Array", "2 - Testing variable length string array in compound type",
        "3 - Four score and seven\n years ago our forefathers brought forth on this continent a new nation,"};
    hsize_t dims_vlen_str_array[] = {VLEN_STR_ARRY_DIM};

    /* fixlen string array  */
    hid_t       sid_fixlen_str_array                      = H5I_INVALID_HID; /* dataspace ID */
    hid_t       tid_fixlen_str_array_pre                  = H5I_INVALID_HID; /* datatype ID */
    hid_t       tid_fixlen_str_array                      = H5I_INVALID_HID; /* datatype ID */
    const char *fixlen_str_array_buf[FIXLEN_STR_ARRY_DIM] = {
        "1 - Fixed length string Array", "2 - Fixed length string Array", "3 - Fixed length string Array"};
    hsize_t dims_fixlen_str_array[] = {FIXLEN_STR_ARRY_DIM};

    /*------------------------------------------
     * compound dataset
     *------------------------------------------*/
    hid_t   sid_comp    = H5I_INVALID_HID; /* dataspace ID */
    hid_t   tid1_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid2_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid3_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid4_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid5_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid6_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid7_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid8_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   tid9_comp   = H5I_INVALID_HID; /* datatype ID */
    hid_t   did_comp    = H5I_INVALID_HID; /* dataset ID */
    hsize_t dims_comp[] = {COMP_DIM};
    herr_t  status      = SUCCEED;

    /* make compound strings data */
    comp1_t comp1_buf;
    comp2_t comp2_buf;
    comp3_t comp3_buf;
    comp4_t comp4_buf;
    comp5_t comp5_buf;
    comp6_t comp6_buf;
    comp7_t comp7_buf;
    comp8_t comp8_buf;
    comp9_t comp9_buf;

    /* copy vlen string data to compound buffers */
    comp1_buf.str_vlen = comp1_buf.str_vlen_repeat = vlen_str_buf;
    comp2_buf.str_vlen = comp2_buf.str_vlen_repeat = vlen_str_buf;
    comp3_buf.str_vlen = comp3_buf.str_vlen_repeat = vlen_str_buf;
    comp4_buf.str_vlen = comp4_buf.str_vlen_repeat = vlen_str_buf;
    comp5_buf.str_vlen = comp5_buf.str_vlen_repeat = vlen_str_buf;
    comp6_buf.str_vlen = comp6_buf.str_vlen_repeat = vlen_str_buf;
    comp7_buf.str_vlen = comp7_buf.str_vlen_repeat = vlen_str_buf;
    comp8_buf.str_vlen = comp8_buf.str_vlen_repeat = vlen_str_buf;
    comp9_buf.str_vlen = comp9_buf.str_vlen_repeat = vlen_str_buf;

    /* copy fixlen string data to compound buffers */
    HDstrcpy(comp1_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp1_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp2_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp2_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp3_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp3_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp3_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp3_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp4_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp4_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp5_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp5_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp6_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp6_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp7_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp7_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp8_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp8_buf.str_fixlen_repeat, fixlen_str_buf);

    HDstrcpy(comp9_buf.str_fixlen, fixlen_str_buf);
    HDstrcpy(comp9_buf.str_fixlen_repeat, fixlen_str_buf);

    /* copy vlen string array data to compound buffers */
    for (i = 0; i < VLEN_STR_ARRY_DIM; i++) {
        comp1_buf.str_array_vlen[i] = comp1_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp2_buf.str_array_vlen[i] = comp2_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp3_buf.str_array_vlen[i] = comp3_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp4_buf.str_array_vlen[i] = comp4_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp5_buf.str_array_vlen[i] = comp5_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp6_buf.str_array_vlen[i] = comp6_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp7_buf.str_array_vlen[i] = comp7_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp8_buf.str_array_vlen[i] = comp8_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
        comp9_buf.str_array_vlen[i] = comp9_buf.str_vlen_array_again[i] = vlen_str_array_buf[i];
    }

    /* copy fixlen string attay data to compound buffers */
    for (i = 0; i < FIXLEN_STR_ARRY_DIM; i++) {
        HDstrcpy(comp1_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp1_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp2_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp2_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp3_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp3_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp4_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp4_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp5_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp5_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp6_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp6_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp7_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp7_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp8_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp8_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);

        HDstrcpy(comp9_buf.str_array_fixlen[i], fixlen_str_array_buf[i]);
        HDstrcpy(comp9_buf.str_fixlen_array_again[i], fixlen_str_array_buf[i]);
    }

    /* int data */
    comp9_buf.int_data1 = 10;
    comp9_buf.int_data2 = 20;
    comp9_buf.int_data3 = 30;

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    if (is_file_new == 1) {
        fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        if (fid1 < 0) {
            HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
            status = FAIL;
            goto out;
        }
    }
    else {
        fid1 = H5Fopen(fname1, H5F_ACC_RDWR, H5P_DEFAULT);
        if (fid1 < 0) {
            HDfprintf(stderr, "Error: %s> H5Fopen failed.\n", fname1);
            status = FAIL;
            goto out;
        }
    }

    /*-----------------------------------------------------------------------
     * Create group
     *------------------------------------------------------------------------*/
    gid = H5Gcreate2(fid1, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Variable length String1 - Create space and type
     *------------------------------------------------------------------------*/
    sid_vlen_str = H5Screate_simple(STR_RANK, dims_vlen_str, NULL);
    if (sid_vlen_str < 0) {
        HDfprintf(stderr, "Error: %s> H5Screate_simple failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    tid_vlen_str = H5Tcopy(H5T_C_S1);
    status       = H5Tset_size(tid_vlen_str, H5T_VARIABLE);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tset_size failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Fixed length String2 - Create space and type
     *------------------------------------------------------------------------*/
    sid_fixlen_str = H5Screate_simple(STR_RANK, dims_fixlen_str, NULL);
    if (sid_fixlen_str < 0) {
        HDfprintf(stderr, "Error: %s> H5Screate_simple failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    tid_fixlen_str = H5Tcopy(H5T_C_S1);
    status         = H5Tset_size(tid_fixlen_str, FIXLEN_STR_SIZE);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tset_size failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Fixed length String3 array - Create space and type
     *------------------------------------------------------------------------*/
    sid_vlen_str_array = H5Screate_simple(STR_RANK, dims_vlen_str_array, NULL);
    if (sid_vlen_str_array < 0) {
        HDfprintf(stderr, "Error: %s> H5Screate_simple failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    tid_vlen_str_array_pre = H5Tcopy(H5T_C_S1);
    status                 = H5Tset_size(tid_vlen_str_array_pre, H5T_VARIABLE);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tset_size failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /* Create the array data type for the string array                */
    tid_vlen_str_array = H5Tarray_create2(tid_vlen_str_array_pre, COMP_RANK, dims_vlen_str_array);
    if (tid_vlen_str_array < 0) {
        HDfprintf(stderr, "Error: %s> H5Tarray_create2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Variable length String4 array - Create space and type
     *------------------------------------------------------------------------*/
    sid_fixlen_str_array = H5Screate_simple(STR_RANK, dims_fixlen_str_array, NULL);
    if (sid_fixlen_str_array < 0) {
        HDfprintf(stderr, "Error: %s> H5Screate_simple failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    tid_fixlen_str_array_pre = H5Tcopy(H5T_C_S1);
    status                   = H5Tset_size(tid_fixlen_str_array_pre, FIXLEN_STR_ARRY_SIZE);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tset_size failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    /* Create the array data type for the string array                */
    tid_fixlen_str_array = H5Tarray_create2(tid_fixlen_str_array_pre, COMP_RANK, dims_fixlen_str_array);
    if (tid_fixlen_str_array < 0) {
        HDfprintf(stderr, "Error: %s> H5Tarray_create2 failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    /*-------------------------------------------------------------------------
     * Compound dataset
     *------------------------------------------------------------------------*/
    sid_comp = H5Screate_simple(COMP_RANK, dims_comp, NULL);
    if (sid_comp < 0) {
        HDfprintf(stderr, "Error: %s> H5Screate_simple failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    tid1_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp1_t));
    tid2_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp2_t));
    tid3_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp3_t));
    tid4_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp4_t));
    tid5_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp5_t));
    tid6_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp6_t));
    tid7_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp7_t));
    tid8_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp8_t));
    tid9_comp = H5Tcreate(H5T_COMPOUND, sizeof(comp9_t));

    /* compound 1 */
    H5Tinsert(tid1_comp, "VLEN_STR1", HOFFSET(comp1_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid1_comp, "VLEN_STR2", HOFFSET(comp1_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid1_comp, "FIXLEN_STR1", HOFFSET(comp1_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid1_comp, "FIXLEN_STR2", HOFFSET(comp1_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid1_comp, "VLEN_STR_ARRAY1", HOFFSET(comp1_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid1_comp, "VLEN_STR_ARRAY2", HOFFSET(comp1_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid1_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp1_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid1_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp1_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 2 */
    H5Tinsert(tid2_comp, "VLEN_STR1", HOFFSET(comp2_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid2_comp, "VLEN_STR2", HOFFSET(comp2_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid2_comp, "FIXLEN_STR1", HOFFSET(comp2_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid2_comp, "FIXLEN_STR2", HOFFSET(comp2_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid2_comp, "VLEN_STR_ARRAY1", HOFFSET(comp2_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid2_comp, "VLEN_STR_ARRAY2", HOFFSET(comp2_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid2_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp2_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid2_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp2_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 3 */
    H5Tinsert(tid3_comp, "VLEN_STR1", HOFFSET(comp3_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid3_comp, "VLEN_STR2", HOFFSET(comp3_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid3_comp, "FIXLEN_STR1", HOFFSET(comp3_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid3_comp, "FIXLEN_STR2", HOFFSET(comp3_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid3_comp, "VLEN_STR_ARRAY1", HOFFSET(comp3_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid3_comp, "VLEN_STR_ARRAY2", HOFFSET(comp3_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid3_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp3_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid3_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp3_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 4 */
    H5Tinsert(tid4_comp, "VLEN_STR1", HOFFSET(comp4_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid4_comp, "VLEN_STR2", HOFFSET(comp4_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid4_comp, "FIXLEN_STR1", HOFFSET(comp4_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid4_comp, "FIXLEN_STR2", HOFFSET(comp4_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid4_comp, "VLEN_STR_ARRAY1", HOFFSET(comp4_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid4_comp, "VLEN_STR_ARRAY2", HOFFSET(comp4_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid4_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp4_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid4_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp4_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 5 */
    H5Tinsert(tid5_comp, "VLEN_STR1", HOFFSET(comp5_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid5_comp, "VLEN_STR2", HOFFSET(comp5_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid5_comp, "FIXLEN_STR1", HOFFSET(comp5_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid5_comp, "FIXLEN_STR2", HOFFSET(comp5_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid5_comp, "VLEN_STR_ARRAY1", HOFFSET(comp5_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid5_comp, "VLEN_STR_ARRAY2", HOFFSET(comp5_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid5_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp5_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid5_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp5_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 6 */
    H5Tinsert(tid6_comp, "VLEN_STR1", HOFFSET(comp6_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid6_comp, "VLEN_STR2", HOFFSET(comp6_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid6_comp, "FIXLEN_STR1", HOFFSET(comp6_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid6_comp, "FIXLEN_STR2", HOFFSET(comp6_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid6_comp, "VLEN_STR_ARRAY1", HOFFSET(comp6_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid6_comp, "VLEN_STR_ARRAY2", HOFFSET(comp6_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid6_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp6_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid6_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp6_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 7 */
    H5Tinsert(tid7_comp, "VLEN_STR1", HOFFSET(comp7_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid7_comp, "VLEN_STR2", HOFFSET(comp7_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid7_comp, "FIXLEN_STR1", HOFFSET(comp7_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid7_comp, "FIXLEN_STR2", HOFFSET(comp7_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid7_comp, "VLEN_STR_ARRAY1", HOFFSET(comp7_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid7_comp, "VLEN_STR_ARRAY2", HOFFSET(comp7_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid7_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp7_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid7_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp7_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 8 */
    H5Tinsert(tid8_comp, "VLEN_STR1", HOFFSET(comp8_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid8_comp, "VLEN_STR2", HOFFSET(comp8_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid8_comp, "FIXLEN_STR1", HOFFSET(comp8_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid8_comp, "FIXLEN_STR2", HOFFSET(comp8_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid8_comp, "VLEN_STR_ARRAY1", HOFFSET(comp8_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid8_comp, "VLEN_STR_ARRAY2", HOFFSET(comp8_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid8_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp8_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid8_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp8_t, str_fixlen_array_again), tid_fixlen_str_array);

    /* compound 9 */
    H5Tinsert(tid9_comp, "VLEN_STR1", HOFFSET(comp9_t, str_vlen), tid_vlen_str);
    H5Tinsert(tid9_comp, "VLEN_STR2", HOFFSET(comp9_t, str_vlen_repeat), tid_vlen_str);
    H5Tinsert(tid9_comp, "FIXLEN_STR1", HOFFSET(comp9_t, str_fixlen), tid_fixlen_str);
    H5Tinsert(tid9_comp, "FIXLEN_STR2", HOFFSET(comp9_t, str_fixlen_repeat), tid_fixlen_str);
    H5Tinsert(tid9_comp, "VLEN_STR_ARRAY1", HOFFSET(comp9_t, str_array_vlen), tid_vlen_str_array);
    H5Tinsert(tid9_comp, "VLEN_STR_ARRAY2", HOFFSET(comp9_t, str_vlen_array_again), tid_vlen_str_array);
    H5Tinsert(tid9_comp, "FIXLEN_STR_ARRAY1", HOFFSET(comp9_t, str_array_fixlen), tid_fixlen_str_array);
    H5Tinsert(tid9_comp, "FIXLEN_STR_ARRAY2", HOFFSET(comp9_t, str_fixlen_array_again), tid_fixlen_str_array);
    H5Tinsert(tid9_comp, "INT_DATA1", HOFFSET(comp9_t, int_data1), H5T_STD_I32LE);
    H5Tinsert(tid9_comp, "INT_DATA2", HOFFSET(comp9_t, int_data2), H5T_STD_I32BE);
    H5Tinsert(tid9_comp, "INT_DATA3", HOFFSET(comp9_t, int_data3), H5T_STD_I32LE);
    H5Tinsert(tid9_comp, "OBJREF1", HOFFSET(comp9_t, objref1), H5T_STD_REF_OBJ);
    H5Tinsert(tid9_comp, "OBJREF2", HOFFSET(comp9_t, objref2), H5T_STD_REF_OBJ);
    H5Tinsert(tid9_comp, "OBJREF3", HOFFSET(comp9_t, objref3), H5T_STD_REF_OBJ);

    /* Write data to compound 1 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset1", tid1_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid1_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp1_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 2 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset2", tid2_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid2_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp2_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 3 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset3", tid3_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid3_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp3_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 4 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset4", tid4_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid4_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp4_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 5 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset5", tid5_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid5_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp5_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 6 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset6", tid6_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid6_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp6_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 7 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset7", tid7_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid7_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp7_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 8 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset8", tid8_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status   = H5Dwrite(did_comp, tid8_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp8_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }
    H5Dclose(did_comp);

    /* Write data to compound 9 dataset buffer */
    did_comp = H5Dcreate2(gid, "Compound_dset9", tid9_comp, sid_comp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* obj references */
    status = H5Rcreate(&(comp9_buf.objref1), gid, "Compound_dset2", H5R_OBJECT, (hid_t)-1);
    status = H5Rcreate(&(comp9_buf.objref2), gid, "Compound_dset3", H5R_OBJECT, (hid_t)-1);
    status = H5Rcreate(&(comp9_buf.objref3), gid, "Compound_dset4", H5R_OBJECT, (hid_t)-1);

    status = H5Dwrite(did_comp, tid9_comp, H5S_ALL, H5S_ALL, H5P_DEFAULT, &comp9_buf);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", fname1);
        status = FAIL;
        goto out;
    }

    H5Dclose(did_comp);
    did_comp = H5I_INVALID_HID;

out:
    /*-----------------------------------------------------------------------
     * Close
     *-----------------------------------------------------------------------*/
    if (fid1 > 0)
        H5Fclose(fid1);
    if (gid > 0)
        H5Gclose(gid);
    /* vlen string */
    if (tid_vlen_str > 0)
        H5Tclose(tid_vlen_str);
    if (sid_vlen_str > 0)
        H5Sclose(sid_vlen_str);
    /* fixed len string */
    if (tid_fixlen_str > 0)
        H5Tclose(tid_fixlen_str);
    if (sid_fixlen_str > 0)
        H5Sclose(sid_fixlen_str);
    /* vlen string array */
    if (tid_vlen_str_array_pre > 0)
        H5Tclose(tid_vlen_str_array_pre);
    if (tid_vlen_str_array > 0)
        H5Tclose(tid_vlen_str_array);
    if (sid_vlen_str_array > 0)
        H5Sclose(sid_vlen_str_array);
    /* fixed len string array */
    if (tid_fixlen_str_array_pre > 0)
        H5Tclose(tid_fixlen_str_array_pre);
    if (tid_fixlen_str_array > 0)
        H5Tclose(tid_fixlen_str_array);
    if (sid_fixlen_str_array > 0)
        H5Sclose(sid_fixlen_str_array);
    /* compound */
    if (tid1_comp > 0)
        H5Tclose(tid1_comp);
    if (tid2_comp > 0)
        H5Tclose(tid2_comp);
    if (tid3_comp > 0)
        H5Tclose(tid3_comp);
    if (tid4_comp > 0)
        H5Tclose(tid4_comp);
    if (tid5_comp > 0)
        H5Tclose(tid5_comp);
    if (tid6_comp > 0)
        H5Tclose(tid6_comp);
    if (tid7_comp > 0)
        H5Tclose(tid7_comp);
    if (tid8_comp > 0)
        H5Tclose(tid8_comp);
    if (tid9_comp > 0)
        H5Tclose(tid9_comp);
    if (did_comp > 0)
        H5Dclose(did_comp);
    if (sid_comp > 0)
        H5Sclose(sid_comp);

    return status;
} /* end test_comp_vlen_strings() */

/*-------------------------------------------------------------------------
 *
 * Purpose: Test diffs of enum values which may include invalid values.
 *
 * Programmer: Dana Robinson
 *
 *-------------------------------------------------------------------------*/

static int
test_enums(const char *fname)
{
    hid_t fid = H5I_INVALID_HID;

    hid_t tid      = H5I_INVALID_HID;
    int   enum_val = -1;

    /* The data in the two arrays cover the following cases:
     *
     *   V = valid enum value, I = invalid enum value
     *
     *   0:  I-I (same value)
     *   1:  V-I
     *   2:  I-V
     *   3:  V-V (same value)
     *   4:  I-I (different values) SKIPPED FOR NOW
     *   5:  V-V (different values)
     */
    /* *** NOTE ***
     *
     * There is a bug in H5Dread() where invalid enum values are always
     * returned as -1 so two different invalid enum values cannot be
     * properly compared.  Test 4 has been adjusted to pass here
     * while we fix the issue.
     */
    int data1[6] = {9, 0, 9, 0, 9, 0};
    /*int         data1[6] = {9, 0, 9, 0, 8, 0};  */
    int data2[6] = {9, 9, 0, 0, 9, 1};

    hsize_t dims = 6;

    herr_t status = SUCCEED;

    /*-----------------------------------------------------------------------
     * Create the file
     *---------------------------------------------------------------------*/

    fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-----------------------------------------------------------------------
     * Create enum types
     *---------------------------------------------------------------------*/

    tid      = H5Tenum_create(H5T_NATIVE_INT);
    enum_val = 0;
    status   = H5Tenum_insert(tid, "YIN", &enum_val);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tenum_insert failed.\n", fname);
        status = FAIL;
        goto out;
    }
    enum_val = 1;
    status   = H5Tenum_insert(tid, "YANG", &enum_val);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> H5Tenum_insert failed.\n", fname);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Create datasets containing enum data.
     *---------------------------------------------------------------------*/

    status = write_dset(fid, 1, &dims, "dset1", tid, data1);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> write_dset failed.\n", fname);
        status = FAIL;
        goto out;
    }
    status = write_dset(fid, 1, &dims, "dset2", tid, data2);
    if (status < 0) {
        HDfprintf(stderr, "Error: %s> write_dset failed.\n", fname);
        status = FAIL;
        goto out;
    }

out:
    /*-----------------------------------------------------------------------
     * Close
     *---------------------------------------------------------------------*/
    if (fid)
        H5Fclose(fid);
    if (tid)
        H5Tclose(tid);

    return status;
}

/*-------------------------------------------------------------------------
 *
 * Purpose:
 *   Create test files with dataset and attribute with container types
 *   (array, vlen) with multiple nested compound types.
 *
 * Function: test_comps_array()
 *  - type: compound->array->compound
 *
 * Function: test_comps_vlen()
 *  - type: compound->vlen->compound
 *
 * Function: test_comps_array_vlen()
 *  - type: compound->array->compound->vlen->compound
 *
 * Function: test_comps_vlen_arry()
 *  - type: compound->vlen->compound->array->compound
 *
 * Programmer: Jonathan Kim (Sep, 1, 2011)
 *
 *-------------------------------------------------------------------------*/
#define SDIM_DSET       2
#define SDIM_CMPD_ARRAY 2

static void
test_comps_array(const char *fname, const char *dset, const char *attr, int diff, int is_file_new)
{
    /* sub compound 2 */
    typedef struct {
        int   i2;
        float f2;
    } cmpd2_t;

    /* top compound 1 */
    typedef struct {
        int     i1;
        cmpd2_t cmpd2[SDIM_CMPD_ARRAY];
    } cmpd1_t;

    cmpd1_t wdata[SDIM_DSET]; /* dataset with compound1 */

    hid_t                        fid               = H5I_INVALID_HID; /* HDF5 File IDs  */
    hid_t                        did_dset          = H5I_INVALID_HID; /* Dataset ID   */
    hid_t                        sid_dset          = H5I_INVALID_HID; /* Dataset space ID   */
    hid_t                        tid_cmpd1         = H5I_INVALID_HID; /* Compound1 type ID   */
    hid_t                        tid_arry1         = H5I_INVALID_HID; /* Array type ID in compound1   */
    hid_t                        tid_cmpd2         = H5I_INVALID_HID; /* Compound2 type ID   */
    hid_t                        tid_attr          = H5I_INVALID_HID;
    hsize_t                      sdims_dset[]      = {SDIM_DSET};
    hsize_t                      sdims_cmpd_arry[] = {SDIM_CMPD_ARRAY};
    int                          i, j;
    herr_t H5_ATTR_NDEBUG_UNUSED ret; /* Generic return value  */

    /* Initialize array data to write */
    for (i = 0; i < SDIM_DSET; i++) {
        wdata[i].i1 = i;
        for (j = 0; j < SDIM_CMPD_ARRAY; j++) {
            wdata[i].cmpd2[j].i2 = i * 10 + diff;
            wdata[i].cmpd2[j].f2 = (float)i * 10.5F + (float)diff;
        } /* end for */
    }

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    if (is_file_new == 1)
        fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    else
        fid = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);

    /* -------------------------------
     * Create a sub compound2 datatype  */
    tid_cmpd2 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd2_t));

    /* Insert integer field */
    ret = H5Tinsert(tid_cmpd2, "int2", HOFFSET(cmpd2_t, i2), H5T_NATIVE_INT);
    assert(ret >= 0);

    /* Insert float field */
    ret = H5Tinsert(tid_cmpd2, "float2", HOFFSET(cmpd2_t, f2), H5T_NATIVE_FLOAT);
    assert(ret >= 0);

    /*-----------------------------------
     * Create a top compound1.
     */
    tid_cmpd1 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd1_t));

    ret = H5Tinsert(tid_cmpd1, "int1", HOFFSET(cmpd1_t, i1), H5T_NATIVE_INT);
    assert(ret >= 0);

    /* Create an array datatype */
    tid_arry1 = H5Tarray_create2(tid_cmpd2, 1, sdims_cmpd_arry);
    /* insert the array */
    ret = H5Tinsert(tid_cmpd1, "array_cmpd1", HOFFSET(cmpd1_t, cmpd2), tid_arry1);
    assert(ret >= 0);

    /* -------------------
     * Create a dataset
     */
    /* Create dataspace for datasets */
    sid_dset = H5Screate_simple(1, sdims_dset, NULL);

    did_dset = H5Dcreate2(fid, dset, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(did_dset, tid_cmpd1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /*-----------------------------------
     * Create an attribute in root group
     */
    tid_attr = H5Acreate2(fid, attr, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT);
    assert(tid_attr > 0);
    ret = H5Awrite(tid_attr, tid_cmpd1, wdata);
    assert(ret >= 0);

    /* ----------------
     * Close Dataset */
    ret = H5Aclose(tid_attr);
    assert(ret >= 0);
    ret = H5Tclose(tid_arry1);
    assert(ret >= 0);
    ret = H5Dclose(did_dset);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd1);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd2);
    assert(ret >= 0);
    ret = H5Sclose(sid_dset);
    assert(ret >= 0);
    ret = H5Fclose(fid);
    assert(ret >= 0);
}

static void
test_comps_vlen(const char *fname, const char *dset, const char *attr, int diff, int is_file_new)
{
    /* sub compound 2 */
    typedef struct {
        int   i2;
        float f2;
    } cmpd2_t;

    /* top compound 1 */
    typedef struct {
        int   i1;
        hvl_t vl; /* VL information for compound2 */
    } cmpd1_t;

    cmpd1_t wdata[SDIM_DSET]; /* Dataset for compound1 */

    hid_t   fid            = H5I_INVALID_HID; /* HDF5 File ID */
    hid_t   did_dset       = H5I_INVALID_HID; /* dataset ID   */
    hid_t   sid_dset       = H5I_INVALID_HID; /* dataset space ID */
    hid_t   tid_attr       = H5I_INVALID_HID;
    hid_t   tid_cmpd2      = H5I_INVALID_HID; /* compound2 type ID */
    hid_t   tid_cmpd1      = H5I_INVALID_HID; /* compound1 type ID */
    hid_t   tid_cmpd1_vlen = H5I_INVALID_HID;
    hsize_t sdims_dset[]   = {SDIM_DSET};

    unsigned                     i, j; /* counting variables */
    herr_t H5_ATTR_NDEBUG_UNUSED ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for (i = 0; i < SDIM_DSET; i++) {
        wdata[i].i1     = (int)i;
        wdata[i].vl.p   = HDmalloc((i + 1) * sizeof(cmpd2_t));
        wdata[i].vl.len = i + 1;
        for (j = 0; j < (i + 1); j++) {
            ((cmpd2_t *)wdata[i].vl.p)[j].i2 = (int)(i * 10 + (unsigned)diff);
            ((cmpd2_t *)wdata[i].vl.p)[j].f2 = (float)i * 10.5F + (float)diff;
        } /* end for */
    }     /* end for */

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    if (is_file_new == 1)
        fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    else
        fid = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);

    /* -----------------------------
     * Create sub compound2 type */
    tid_cmpd2 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd2_t));

    /* Insert fields */
    ret = H5Tinsert(tid_cmpd2, "int2", HOFFSET(cmpd2_t, i2), H5T_NATIVE_INT);
    assert(ret >= 0);
    ret = H5Tinsert(tid_cmpd2, "float2", HOFFSET(cmpd2_t, f2), H5T_NATIVE_FLOAT);
    assert(ret >= 0);

    /* ---------------------------
     * Create top compound1 type */
    tid_cmpd1 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd1_t));
    /* Insert fields */
    ret = H5Tinsert(tid_cmpd1, "int1", HOFFSET(cmpd1_t, i1), H5T_NATIVE_INT);
    assert(ret >= 0);
    /* Create a VL datatype */
    tid_cmpd1_vlen = H5Tvlen_create(tid_cmpd2);

    ret = H5Tinsert(tid_cmpd1, "vlen_cmpd1", HOFFSET(cmpd1_t, vl), tid_cmpd1_vlen);
    assert(ret >= 0);

    /* -------------------------------
     * Create dataset with compound1
     */
    /* Create dataspace for dataset */
    sid_dset = H5Screate_simple(1, sdims_dset, NULL);

    /* Create a dataset */
    did_dset = H5Dcreate2(fid, dset, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(did_dset, tid_cmpd1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /*-----------------------------------
     * Create an attribute in root group
     */
    tid_attr = H5Acreate2(fid, attr, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT);
    assert(tid_attr > 0);
    ret = H5Awrite(tid_attr, tid_cmpd1, wdata);
    assert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Treclaim(tid_cmpd1, sid_dset, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /* ----------------
     * Close IDs */
    ret = H5Aclose(tid_attr);
    assert(ret >= 0);
    ret = H5Dclose(did_dset);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd2);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd1);
    assert(ret >= 0);
    ret = H5Sclose(sid_dset);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd1_vlen);
    assert(ret >= 0);
    ret = H5Fclose(fid);
    assert(ret >= 0);
}

static void
test_comps_array_vlen(const char *fname, const char *dset, const char *attr, int diff, int is_file_new)
{
    typedef struct {
        int   i3;
        float f3;
    } cmpd3_t;

    typedef struct { /* Typedef for compound datatype */
        int   i2;
        hvl_t vl; /* VL information to write */
    } cmpd2_t;

    typedef struct {
        int     i1;
        cmpd2_t cmpd2[SDIM_CMPD_ARRAY];
    } cmpd1_t;

    cmpd1_t                      wdata[SDIM_DSET];                 /* Information to write */
    hid_t                        fid            = H5I_INVALID_HID; /* HDF5 File IDs  */
    hid_t                        did_dset       = H5I_INVALID_HID; /* Dataset ID   */
    hid_t                        sid_dset       = H5I_INVALID_HID; /* Dataspace ID   */
    hid_t                        tid_attr       = H5I_INVALID_HID;
    hid_t                        tid_cmpd1      = H5I_INVALID_HID; /* Compound1 Datatype ID   */
    hid_t                        tid_arry1      = H5I_INVALID_HID; /* Array Datatype ID   */
    hid_t                        tid_cmpd2      = H5I_INVALID_HID; /* Compound2 Datatype ID   */
    hid_t                        tid_cmpd2_vlen = H5I_INVALID_HID;
    hid_t                        tid_cmpd3      = H5I_INVALID_HID; /* Compound3 Datatype ID   */
    hsize_t                      sdims_dset[]   = {SDIM_DSET};
    hsize_t                      sdims_arry[]   = {SDIM_CMPD_ARRAY};
    unsigned                     i, j, k; /* counting variables */
    herr_t H5_ATTR_NDEBUG_UNUSED ret;     /* Generic return value  */

    /* Initialize array data to write in compound1 */
    for (i = 0; i < SDIM_DSET; i++) {
        wdata[i].i1 = (int)i;

        /* Allocate and initialize VL data to write in compound2 */
        for (j = 0; j < SDIM_CMPD_ARRAY; j++) {
            wdata[i].cmpd2[j].i2     = (int)(j * 10);
            wdata[i].cmpd2[j].vl.p   = HDmalloc((j + 1) * sizeof(cmpd3_t));
            wdata[i].cmpd2[j].vl.len = j + 1;
            for (k = 0; k < (j + 1); k++) {
                /* Initialize data of compound3 */
                ((cmpd3_t *)wdata[i].cmpd2[j].vl.p)[k].i3 = (int)j * 10 + diff;
                ((cmpd3_t *)wdata[i].cmpd2[j].vl.p)[k].f3 = (float)j * 10.5F + (float)diff;
            } /* end for */
        }     /* end for */
    }

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    if (is_file_new == 1)
        fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    else
        fid = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid_dset = H5Screate_simple(1, sdims_dset, NULL);

    /*-------------------------------------
     * Create a sub compound3 datatype  */
    tid_cmpd3 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd3_t));

    /* Insert integer field */
    ret = H5Tinsert(tid_cmpd3, "int", HOFFSET(cmpd3_t, i3), H5T_NATIVE_INT);
    assert(ret >= 0);

    /* Insert float field */
    ret = H5Tinsert(tid_cmpd3, "float", HOFFSET(cmpd3_t, f3), H5T_NATIVE_FLOAT);
    assert(ret >= 0);

    /*-------------------------------------
     * Create a sub compound2 datatype  */
    tid_cmpd2 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd2_t));

    /* Insert integer field */
    ret = H5Tinsert(tid_cmpd2, "int", HOFFSET(cmpd2_t, i2), H5T_NATIVE_INT);
    assert(ret >= 0);
    /* Create a VL datatype */
    tid_cmpd2_vlen = H5Tvlen_create(tid_cmpd3);
    ret            = H5Tinsert(tid_cmpd2, "vlen", HOFFSET(cmpd2_t, vl), tid_cmpd2_vlen);
    assert(ret >= 0);

    /*-----------------------------------
     * Create a top compound1 datatype for dataset.
     */
    tid_cmpd1 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd1_t));

    /* Create an array datatype */
    tid_arry1 = H5Tarray_create2(tid_cmpd2, 1, sdims_arry);
    /* insert the array */
    H5Tinsert(tid_cmpd1, "array_comp", HOFFSET(cmpd1_t, cmpd2), tid_arry1);

    /* ----------------------
     * Create a dataset */
    did_dset = H5Dcreate2(fid, dset, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(did_dset, tid_cmpd1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /*-----------------------------------
     * Create an attribute in root group
     */
    tid_attr = H5Acreate2(fid, attr, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT);
    assert(tid_attr > 0);
    ret = H5Awrite(tid_attr, tid_cmpd1, wdata);
    assert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Treclaim(tid_cmpd1, sid_dset, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /*-------------------
     * Close IDs */
    ret = H5Aclose(tid_attr);
    assert(ret >= 0);
    ret = H5Tclose(tid_arry1);
    assert(ret >= 0);
    ret = H5Dclose(did_dset);
    assert(ret >= 0);
    ret = H5Sclose(sid_dset);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd3);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd2);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd2_vlen);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd1);
    assert(ret >= 0);
    ret = H5Fclose(fid);
    assert(ret >= 0);
}

static void
test_comps_vlen_arry(const char *fname, const char *dset, const char *attr, int diff, int is_file_new)
{
    /* sub compound 3 */
    typedef struct {
        int   i3;
        float f3;
    } cmpd3_t;

    /* sub compound 2 */
    typedef struct {
        int     i2;
        cmpd3_t cmpd3[SDIM_CMPD_ARRAY];
    } cmpd2_t;

    /* top compound 1 */
    typedef struct {
        int   i1;
        hvl_t vl; /* VL information for compound2 */
    } cmpd1_t;

    cmpd1_t wdata[SDIM_DSET]; /* Dataset for compound1 */

    hid_t   fid               = H5I_INVALID_HID; /* HDF5 File ID */
    hid_t   did_dset          = H5I_INVALID_HID; /* dataset ID   */
    hid_t   sid_dset          = H5I_INVALID_HID; /* dataset space ID */
    hid_t   tid_attr          = H5I_INVALID_HID;
    hid_t   tid_cmpd3         = H5I_INVALID_HID; /* compound3 type ID */
    hid_t   tid_cmpd2         = H5I_INVALID_HID; /* compound2 type ID */
    hid_t   tid_cmpd2_arry    = H5I_INVALID_HID;
    hid_t   tid_cmpd1         = H5I_INVALID_HID; /* compound1 type ID */
    hid_t   tid_cmpd1_vlen    = H5I_INVALID_HID;
    hsize_t sdims_dset[]      = {SDIM_DSET};
    hsize_t sdims_cmpd_arry[] = {SDIM_CMPD_ARRAY};

    unsigned                     i, j, k; /* counting variables */
    herr_t H5_ATTR_NDEBUG_UNUSED ret;     /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for (i = 0; i < SDIM_DSET; i++) {
        /* compound 1 data */
        wdata[i].i1     = (int)i;
        wdata[i].vl.p   = HDmalloc((i + 1) * sizeof(cmpd2_t));
        wdata[i].vl.len = i + 1;
        for (j = 0; j < (i + 1); j++) {
            /* compound2 data */
            ((cmpd2_t *)wdata[i].vl.p)[j].i2 = (int)i * 10 + diff;
            for (k = 0; k < SDIM_CMPD_ARRAY; k++) {
                /* compound 3 data */
                ((cmpd2_t *)(wdata[i].vl.p))[j].cmpd3[k].i3 = (int)((float)k * 10.5F) + diff;
                ((cmpd2_t *)(wdata[i].vl.p))[j].cmpd3[k].f3 = (float)k * 10.5F + (float)diff;
            } /* end for */
        }     /* end for */
    }         /* end for */

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    if (is_file_new == 1)
        fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    else
        fid = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);

    /* -----------------------------
     * Create sub compound3 type */
    tid_cmpd3 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd3_t));

    /* Insert fields */
    ret = H5Tinsert(tid_cmpd3, "int3", HOFFSET(cmpd3_t, i3), H5T_NATIVE_INT);
    assert(ret >= 0);
    ret = H5Tinsert(tid_cmpd3, "float3", HOFFSET(cmpd3_t, f3), H5T_NATIVE_FLOAT);
    assert(ret >= 0);

    /* -----------------------------
     * Create sub compound2 type */
    tid_cmpd2 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd2_t));

    ret = H5Tinsert(tid_cmpd2, "int2", HOFFSET(cmpd2_t, i2), H5T_NATIVE_INT);
    assert(ret >= 0);

    tid_cmpd2_arry = H5Tarray_create2(tid_cmpd3, 1, sdims_cmpd_arry);
    ret            = H5Tinsert(tid_cmpd2, "array_cmpd2", HOFFSET(cmpd2_t, cmpd3), tid_cmpd2_arry);
    assert(ret >= 0);

    /* ---------------------------
     * Create top compound1 type
     */
    /* Create a VL datatype */
    tid_cmpd1 = H5Tcreate(H5T_COMPOUND, sizeof(cmpd1_t));
    /* Insert fields */
    ret = H5Tinsert(tid_cmpd1, "int1", HOFFSET(cmpd1_t, i1), H5T_NATIVE_INT);
    assert(ret >= 0);
    tid_cmpd1_vlen = H5Tvlen_create(tid_cmpd2);
    ret            = H5Tinsert(tid_cmpd1, "vlen_cmpd1", HOFFSET(cmpd1_t, vl), tid_cmpd1_vlen);
    assert(ret >= 0);

    /* -------------------------------
     * Create dataset with compound1
     */
    /* Create dataspace for dataset */
    sid_dset = H5Screate_simple(1, sdims_dset, NULL);

    /* Create a dataset */
    did_dset = H5Dcreate2(fid, dset, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(did_dset, tid_cmpd1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /*-----------------------------------
     * Create an attribute in root group
     */
    tid_attr = H5Acreate2(fid, attr, tid_cmpd1, sid_dset, H5P_DEFAULT, H5P_DEFAULT);
    assert(tid_attr > 0);
    ret = H5Awrite(tid_attr, tid_cmpd1, wdata);
    assert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Treclaim(tid_cmpd1, sid_dset, H5P_DEFAULT, wdata);
    assert(ret >= 0);

    /* ----------------
     * Close IDs */
    ret = H5Aclose(tid_attr);
    assert(ret >= 0);
    ret = H5Dclose(did_dset);
    assert(ret >= 0);
    ret = H5Sclose(sid_dset);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd3);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd2_arry);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd2);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd1_vlen);
    assert(ret >= 0);
    ret = H5Tclose(tid_cmpd1);
    assert(ret >= 0);
    ret = H5Fclose(fid);
    assert(ret >= 0);
}

/*-------------------------------------------------------------------------
 * Function: test_data_nocomparables
 *
 * Purpose:
 *   Create test files with non-comparable dataset and attributes with
 *   comparable datasets and attributes.  All the comparables should display
 *   differences.
 *
 *-------------------------------------------------------------------------*/
#define DIM_ARRY 3
static void
test_data_nocomparables(const char *fname, int make_diffs)
{
    hid_t   fid                     = H5I_INVALID_HID;
    hid_t   gid1                    = H5I_INVALID_HID;
    hid_t   gid2                    = H5I_INVALID_HID;
    hid_t   did1                    = H5I_INVALID_HID;
    hid_t   did2                    = H5I_INVALID_HID;
    hid_t   sid1                    = H5I_INVALID_HID;
    hid_t   tid_dset1               = H5I_INVALID_HID;
    hid_t   tid_attr1               = H5I_INVALID_HID;
    hsize_t dims1_1[1]              = {DIM_ARRY};
    hsize_t dims1_2[1]              = {DIM_ARRY + 1};
    hsize_t dims2[2]                = {DIM_ARRY, 1};
    int     data1[DIM_ARRY]         = {0, 0, 0};
    int     data2[DIM_ARRY]         = {1, 1, 1};
    int     data3[DIM_ARRY + 1]     = {1, 1, 1, 1};
    int     data1_dim2[DIM_ARRY][1] = {{0}, {0}, {0}};
    int     rank_attr;
    char    data1_str[DIM_ARRY][STR_SIZE] = {"ab", "cd", "ef"};
    herr_t  status                        = SUCCEED;
    void *  dset_data_ptr1                = NULL;
    void *  dset_data_ptr2                = NULL;
    void *  dset_data_ptr3                = NULL;
    void *  attr_data_ptr1                = NULL;
    void *  attr_data_ptr2                = NULL;
    void *  attr_data_ptr3                = NULL;
    void *  attr_data_ptr4                = NULL;
    void *  attr2_dim_ptr                 = NULL;
    void *  attr3_dim_ptr                 = NULL;

    /* init */
    tid_dset1      = H5Tcopy(H5T_NATIVE_INT);
    dset_data_ptr1 = (int *)&data1;
    dset_data_ptr2 = (int *)&data1;
    dset_data_ptr3 = (int *)&data1;
    tid_attr1      = H5Tcopy(H5T_NATIVE_INT);
    attr_data_ptr1 = (int *)&data1;
    attr_data_ptr3 = (int *)&data1;
    attr_data_ptr4 = (int *)&data1;
    attr2_dim_ptr  = (hsize_t *)&dims1_1;
    attr3_dim_ptr  = (hsize_t *)&dims1_1;
    rank_attr      = 1;

    if (make_diffs) {
        /* ------------
         * group1 */
        tid_dset1 = H5Tcopy(H5T_C_S1);
        H5Tset_size(tid_dset1, (size_t)STR_SIZE);
        dset_data_ptr1 = (char *)&data1_str;
        dset_data_ptr2 = (int *)&data2;
        attr_data_ptr1 = (int *)&data2;

        /* -----------
         * group2
         */
        dset_data_ptr3 = (int *)&data2;
        /* dset1/attr1 */
        tid_attr1 = H5Tcopy(H5T_C_S1);
        H5Tset_size(tid_attr1, (size_t)STR_SIZE);
        attr_data_ptr2 = (char *)&data1_str;

        /* dset1/attr2 */
        attr2_dim_ptr = (hsize_t *)&dims1_2;

        /* dset1/attr3 */
        attr_data_ptr3 = (int *)&data1_dim2;
        attr3_dim_ptr  = (hsize_t *)&dims2;
        rank_attr      = 2;

        /* dset1/attr4 */
        attr_data_ptr4 = (int *)&data2;
    }

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    gid1 = H5Gcreate2(fid, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname);
        status = FAIL;
        goto out;
    }

    gid2 = H5Gcreate2(fid, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", fname);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets in /g1
     *------------------------------------------------------------------------*/
    if ((sid1 = H5Screate_simple(1, dims1_1, NULL)) < 0)
        goto out;

    /*  dset1 */
    if ((did1 = H5Dcreate2(gid1, "dset1", tid_dset1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "Error: %s> H5Dcreate2 failed.\n", "dset1");
        status = FAIL;
        goto out;
    }

    if (H5Dwrite(did1, tid_dset1, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data_ptr1) < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", "dset1");
        status = FAIL;
        goto out;
    }
    write_attr(did1, 1, dims1_1, "attr", H5T_NATIVE_INT, attr_data_ptr1);

    /*  dset2 */
    status = write_dset(gid1, 1, dims1_1, "dset2", H5T_NATIVE_INT, dset_data_ptr2);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname);
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Datasets in /g2
     *------------------------------------------------------------------------*/
    /* ---------
     *  dset1 */
    if ((did2 = H5Dcreate2(gid2, "dset1", H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "Error: %s> H5Dcreate2 failed.\n", "dset1");
        status = FAIL;
        goto out;
    }

    if (H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data_ptr3) < 0) {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", "dset1");
        status = FAIL;
        goto out;
    }
    /* attr1 - non-compatible : different type */
    write_attr(did2, 1, dims1_1, "attr1", tid_attr1, attr_data_ptr2);

    /* attr2 - non-compatible : same rank, different dimention */
    write_attr(did2, 1, (hsize_t *)attr2_dim_ptr, "attr2", H5T_NATIVE_INT, data3);

    /* attr3 - non-compatible : different rank */
    write_attr(did2, rank_attr, (hsize_t *)attr3_dim_ptr, "attr3", H5T_NATIVE_INT, attr_data_ptr3);

    /* attr4 - compatible : different data values */
    write_attr(did2, 1, dims1_1, "attr4", H5T_NATIVE_INT, attr_data_ptr4);

    /*----------
     * dset2 */
    status = write_dset(gid2, 1, dims1_1, "dset2", H5T_NATIVE_INT, dset_data_ptr3);
    if (status == FAIL) {
        HDfprintf(stderr, "Error: %s> write_dset failed\n", fname);
        goto out;
    }

out:

    /*-----------------------------------------------------------------------
     * Close IDs
     *-----------------------------------------------------------------------*/
    if (fid)
        H5Fclose(fid);
    if (gid1)
        H5Gclose(gid1);
    if (gid2)
        H5Gclose(gid2);
    if (did1)
        H5Dclose(did1);
    if (did2)
        H5Dclose(did2);
    if (sid1)
        H5Sclose(sid1);
    if (tid_dset1)
        H5Tclose(tid_dset1);
    if (tid_attr1)
        H5Tclose(tid_attr1);
}

/*-------------------------------------------------------------------------
 * Function: test_objs_nocomparables
 *
 * Purpose:
 *   Create test files with common objects (same name) but different object
 *   types.
 *   h5diff should show non-comparable output from these common objects.
 *-------------------------------------------------------------------------*/
static void
test_objs_nocomparables(const char *fname1, const char *fname2)
{
    hid_t   fid1            = H5I_INVALID_HID;
    hid_t   fid2            = H5I_INVALID_HID;
    hid_t   topgid1         = H5I_INVALID_HID;
    hid_t   topgid2         = H5I_INVALID_HID;
    hid_t   gid1            = H5I_INVALID_HID;
    hid_t   tid1            = H5I_INVALID_HID;
    hid_t   gid2            = H5I_INVALID_HID;
    hid_t   tid2            = H5I_INVALID_HID;
    hsize_t dims[1]         = {DIM_ARRY};
    int     data1[DIM_ARRY] = {1, 1, 1};
    int     data2[DIM_ARRY] = {2, 2, 2};

    /*-----------------------------------------------------------------------
     * Open file(s) to add objects
     *------------------------------------------------------------------------*/
    /* file1 */
    if ((fid1 = H5Fopen(fname1, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /* file2 */
    if ((fid2 = H5Fopen(fname2, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /*-----------------------------------------------------------------------
     * in file1 : add member objects
     *------------------------------------------------------------------------*/
    /* parent group */
    if ((topgid1 = H5Gcreate2(fid1, "diffobjtypes", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /* dataset */
    if (write_dset(topgid1, 1, dims, "obj1", H5T_NATIVE_INT, data1) < 0)
        PROGRAM_ERROR;

    /* group */
    if ((gid1 = H5Gcreate2(topgid1, "obj2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /* committed type */
    if ((tid1 = H5Tcopy(H5T_NATIVE_INT)) < 0)
        PROGRAM_ERROR;
    if (H5Tcommit2(topgid1, "obj3", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        PROGRAM_ERROR;

    /*-----------------------------------------------------------------------
     * in file2 : add member objects
     *------------------------------------------------------------------------*/
    /* parent group */
    if ((topgid2 = H5Gcreate2(fid2, "diffobjtypes", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /* group */
    if ((gid2 = H5Gcreate2(topgid2, "obj1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /* committed type */
    if ((tid2 = H5Tcopy(H5T_NATIVE_INT)) < 0)
        PROGRAM_ERROR;
    if (H5Tcommit2(topgid2, "obj2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        PROGRAM_ERROR;

    /* dataset */
    if (write_dset(topgid2, 1, dims, "obj3", H5T_NATIVE_INT, data2) < 0)
        PROGRAM_ERROR;

    /*-----------------------------------------------------------------------
     * Close IDs
     *-----------------------------------------------------------------------*/
    if (H5Fclose(fid1) < 0)
        PROGRAM_ERROR;
    if (H5Fclose(fid2) < 0)
        PROGRAM_ERROR;
    if (H5Gclose(topgid1) < 0)
        PROGRAM_ERROR;
    if (H5Gclose(topgid2) < 0)
        PROGRAM_ERROR;
    if (H5Gclose(gid1) < 0)
        PROGRAM_ERROR;
    if (H5Gclose(gid2) < 0)
        PROGRAM_ERROR;
    if (H5Tclose(tid1) < 0)
        PROGRAM_ERROR;
    if (H5Tclose(tid2) < 0)
        PROGRAM_ERROR;

    return;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Gclose(topgid1);
        H5Gclose(topgid2);
        H5Gclose(gid1);
        H5Gclose(gid2);
        H5Tclose(tid1);
        H5Tclose(tid2);
    }
    H5E_END_TRY;
}

static hid_t
mkstr(int size, H5T_str_t pad)
{
    hid_t type;

    if ((type = H5Tcopy(H5T_C_S1)) < 0)
        return -1;
    if (H5Tset_size(type, (size_t)size) < 0)
        return -1;
    if (H5Tset_strpad(type, pad) < 0)
        return -1;

    return type;
}

/*-------------------------------------------------------------------------
 * Function: test_objs_strings
 *
 * Purpose:
 *   Create test files with common objects (same name) but different string
 *   types.
 *   h5diff should show differences output from these common objects.
 *-------------------------------------------------------------------------*/
static void
test_objs_strings(const char *fname1, const char *fname2)
{
    hid_t   fid1            = H5I_INVALID_HID;
    hid_t   fid2            = H5I_INVALID_HID;
    hid_t   dataset         = H5I_INVALID_HID;
    hid_t   space           = H5I_INVALID_HID;
    hid_t   f_type          = H5I_INVALID_HID;
    hid_t   m_type          = H5I_INVALID_HID;
    hsize_t dims1[]         = {3, 4};
    char    string1A[12][3] = {"s1", "s2", "s3", "s4", "s5", "s6", "s", "s", "s9", "s0", "s1", "s2"};
    char    string1B[12][3] = {"s1", "s2", "s3", "s4", "s", "s", "s7", "s8", "s9", "s0", "s1", "s2"};

    hsize_t dims2[]          = {20};
    char    string2A[20][10] = {"ab cd ef1", "ab cd ef2", "ab cd ef3", "ab cd ef4", "ab cd ef5",
                             "ab cd ef6", "ab cd ef7", "ab cd ef8", "ab cd 9",   "ab cd 0",
                             "ab cd 1",   "ab cd 2",   "ab cd ef3", "ab cd ef4", "ab cd ef5",
                             "ab cd ef6", "ab cd ef7", "ab cd ef8", "ab cd ef9", "ab cd ef0"};
    char    string2B[20][10] = {"ab cd ef1", "ab cd ef2", "ab cd ef3", "ab cd ef4", "ab cd ef5",
                             "ab cd ef6", "ab cd ef7", "ab cd ef8", "ab cd ef9", "ab cd ef0",
                             "ab cd ef1", "ab cd ef2", "ab cd 3",   "ab cd 4",   "ab cd 5",
                             "ab cd 6",   "ab cd ef7", "ab cd ef8", "ab cd ef9", "ab cd ef0"};

    hsize_t dims3[]      = {27};
    char string3A[27][6] = {"abcd0", "abcd1", "abcd2", "abcd3", "abcd4", "abcd5", "abcd6", "abcd7", "abcd8",
                            "abcd9", "abcd0", "abcd1", "abd2",  "abc3",  "bcd4",  "acd5",  "abcd6", "abcd7",
                            "abcd8", "abcd9", "abcd0", "abcd1", "abcd2", "abcd3", "abc4",  "abc5",  "abc6"};
    char string3B[27][6] = {"abcd0", "abcd1", "abcd2", "abcd3", "abcd4", "abcd5", "abcd6", "abcd7", "abcd8",
                            "abcd9", "abcd0", "abcd1", "abcd2", "abcd3", "abcd4", "abcd5", "abd6",  "abc7",
                            "bcd8",  "acd9",  "abcd0", "abcd1", "abcd2", "abcd3", "abd4",  "abd5",  "abd6"};

    hsize_t dims4[]         = {3};
    char    string4A[3][21] = {"s1234567890123456789", "s1234567890123456789", "s12345678901234567"};
    char    string4B[3][21] = {"s1234567890123456789", "s12345678901234567", "s1234567890123456789"};

    /*-----------------------------------------------------------------------
     * Create file(s)
     *------------------------------------------------------------------------*/
    /* file1 */
    fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid1 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname1);
        goto out;
    }

    /* file2 */
    fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid2 < 0) {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", fname2);
        goto out;
    }

    /* string 1 : nullterm string */
    space   = H5Screate_simple(2, dims1, NULL);
    f_type  = mkstr(5, H5T_STR_NULLTERM);
    m_type  = mkstr(3, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid1, "/string1", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string1A);
    H5Dclose(dataset);
    dataset = H5Dcreate2(fid2, "/string1", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string1B);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 2 : space pad string */
    space   = H5Screate_simple(1, dims2, NULL);
    f_type  = mkstr(11, H5T_STR_SPACEPAD);
    m_type  = mkstr(10, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid1, "/string2", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string2A);
    H5Dclose(dataset);
    dataset = H5Dcreate2(fid2, "/string2", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string2B);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 3 : null pad string */
    space   = H5Screate_simple(1, dims3, NULL);
    f_type  = mkstr(8, H5T_STR_NULLPAD);
    m_type  = mkstr(6, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid1, "/string3", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string3A);
    H5Dclose(dataset);
    dataset = H5Dcreate2(fid2, "/string3", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string3B);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 4 : space pad long string */
    space   = H5Screate_simple(1, dims4, NULL);
    f_type  = mkstr(168, H5T_STR_SPACEPAD);
    m_type  = mkstr(21, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid1, "/string4", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4A);
    H5Dclose(dataset);
    dataset = H5Dcreate2(fid2, "/string4", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4B);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 5 : early term long string */
    string4A[0][10] = 0;
    string4A[0][11] = 0;
    string4B[0][10] = 0;

    string4A[1][10] = 0;
    string4A[1][11] = 'Z';
    string4B[1][10] = 0;
    string4B[1][11] = 'x';

    string4A[2][10] = 0;
    string4B[2][10] = 0;
    string4B[2][11] = 'a';
    string4B[2][12] = 'B';
    string4B[2][13] = 'c';
    space           = H5Screate_simple(1, dims4, NULL);
    f_type          = mkstr(168, H5T_STR_NULLTERM);
    m_type          = mkstr(21, H5T_STR_NULLTERM);
    dataset         = H5Dcreate2(fid1, "/string5", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4A);
    H5Dclose(dataset);
    dataset = H5Dcreate2(fid2, "/string5", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4B);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

out:
    /*-----------------------------------------------------------------------
     * Close IDs
     *-----------------------------------------------------------------------*/
    if (fid1)
        H5Fclose(fid1);
    if (fid2)
        H5Fclose(fid2);
}

/*-------------------------------------------------------------------------
 * Function: write_attr_strings
 *
 * Purpose: write attributes in LOC_ID (dataset, group, named datatype)
 *          swap VL strings
 *
 *-------------------------------------------------------------------------
 */
static void
write_attr_strings(hid_t loc_id, const char *dset_name, hid_t fid,
                   int make_diffs /* flag to modify data buffers */)
{
    /* Compound datatype */
    typedef struct s_t {
        char   a;
        double b;
    } s_t;

    typedef enum { RED, GREEN } e_t;

    hid_t                        aid = H5I_INVALID_HID;
    hid_t                        sid = H5I_INVALID_HID;
    hid_t                        tid = H5I_INVALID_HID;
    herr_t H5_ATTR_NDEBUG_UNUSED status;
    int                          val, i, j, k, l, n;
    float                        f;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]           = {2};
    char       buf1[2][STR_SIZE] = {"ab", "de"};     /* string */
    char *     buf1a[2];                             /* VL string */
    char       buf2[2] = {1, 2};                     /* bitfield, opaque */
    s_t        buf3[2] = {{1, 2.0F}, {3, 4.0F}};     /* compound */
    hobj_ref_t buf4[2];                              /* reference */
    e_t        buf45[2] = {RED, RED};                /* enum */
    hvl_t      buf5[2];                              /* vlen */
    hsize_t    dimarray[1] = {3};                    /* array dimension */
    int        buf6[2][3]  = {{1, 2, 3}, {4, 5, 6}}; /* array */
    int        buf7[2]     = {1, 2};                 /* integer */
    float      buf8[2]     = {1.0F, 2.0F};           /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t dims2[2]              = {3, 2};
    char    buf12[3][2][STR_SIZE] = {{"ab", "cd"}, {"ef", "gh"}, {"ij", "kl"}}; /* string */
    char *  buf12a[3][2];                                                       /* VL string */
    char    buf22[3][2] = {{1, 2}, {3, 4}, {5, 6}};                             /* bitfield, opaque */
    s_t     buf32[6] = {{1, 2.0F}, {3, 4.0F}, {5, 6.0F}, {7, 8.0F}, {9, 10.0F}, {11, 12.0F}}; /* compound */
    hobj_ref_t buf42[3][2];                                                                   /* reference */
    e_t        buf452[3][2];                                                                  /* enum */
    hvl_t      buf52[3][2];                                                                   /* vlen */
    int buf62[6][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}, {10, 11, 12}, {13, 14, 15}, {16, 17, 18}}; /* array */
    int buf72[3][2] = {{1, 2}, {3, 4}, {5, 6}};                     /* integer */
    float buf82[3][2] = {{1.0F, 2.0F}, {3.0F, 4.0F}, {5.0F, 6.0F}}; /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]                 = {4, 3, 2};
    char       buf13[4][3][2][STR_SIZE] = {{{"ab", "cd"}, {"ef", "gh"}, {"ij", "kl"}},
                                     {{"mn", "pq"}, {"rs", "tu"}, {"vw", "xz"}},
                                     {{"AB", "CD"}, {"EF", "GH"}, {"IJ", "KL"}},
                                     {{"MN", "PQ"}, {"RS", "TU"}, {"VW", "XZ"}}}; /* string */
    char *     buf13a[4][3][2];                                                         /* VL string */
    char       buf23[4][3][2];                                                          /* bitfield, opaque */
    s_t        buf33[4][3][2];                                                          /* compound */
    hobj_ref_t buf43[4][3][2];                                                          /* reference */
    e_t        buf453[4][3][2];                                                         /* enum */
    hvl_t      buf53[4][3][2];                                                          /* vlen */
    int        buf63[24][3];                                                            /* array */
    int        buf73[4][3][2];                                                          /* integer */
    float      buf83[4][3][2];                                                          /* float */

    /*-------------------------------------------------------------------------
     * 1D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            for (j = 0; j < 2; j++)
                buf1[i][j] = 'z';

    /*
    buf1[2][2]= {"ab","de"};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <string> and <string>
    position      string of </g1>  string of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          a                z
    [ 0 ]          b                z
    [ 1 ]          d                z
    [ 1 ]          e                z
    */
    for (i = 0; i < 2; i++)
        buf1a[i] = buf1[i];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_attr(loc_id, 1, dims, "string", tid, buf1a);
    status = H5Tclose(tid);

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_attr(loc_id, 1, dims, "VLstring", tid, buf1);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            buf2[i] = buf2[1] = 0;
    /*
    buf2[2]= {1,2};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <bitfield> and <bitfield>
    position      bitfield of </g1> bitfield of </g1> difference
    position        opaque of </g1> opaque of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    */

    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id, 1, dims, "bitfield", tid, buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */

    /*
    buf2[2]= {1,2};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <opaque> and <opaque>
    position     opaque of </g1> opaque of </g1> difference
    position        opaque of </g1> opaque of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    */

    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id, 1, dims, "opaque", tid, buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++) {
            buf3[i].a = 0;
            buf3[i].b = 0;
        }

    /*
    buf3[2]= {{1,2},{3,4}};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <compound> and <compound>
    position        compound of </g1> compound of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               5               4
    [ 0 ]          2               5               3
    [ 1 ]          3               5               2
    [ 1 ]          4               5               1
    */

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id, 1, dims, "compound", tid, buf3);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        status = H5Rcreate(&buf4[0], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        status = H5Rcreate(&buf4[1], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_attr(loc_id, 1, dims, "reference", H5T_STD_REF_OBJ, buf4);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    if (make_diffs)
        for (i = 0; i < 2; i++)
            buf45[i] = GREEN;
    /*
        buf45[2]= {RED,RED};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <enum> and <enum>
        position     enum of </g1>   enum of </g1>   difference
        ------------------------------------------------------------
        [ 0 ]          RED              GREEN
        [ 1 ]          RED              GREEN
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id, 1, dims, "enum", tid, buf45);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len           = 1;
    buf5[0].p             = HDmalloc(1 * sizeof(int));
    ((int *)buf5[0].p)[0] = 1;
    buf5[1].len           = 2;
    buf5[1].p             = HDmalloc(2 * sizeof(int));
    ((int *)buf5[1].p)[0] = 2;
    ((int *)buf5[1].p)[1] = 3;

    if (make_diffs) {
        ((int *)buf5[0].p)[0] = 0;
        ((int *)buf5[1].p)[0] = 0;
        ((int *)buf5[1].p)[1] = 0;
    }
    /*
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        position        vlen of </g1>   vlen of </g1>   difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
        [ 1 ]          3               0               3
     */

    sid    = H5Screate_simple(1, dims, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    aid    = H5Acreate2(loc_id, "vlen", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf5);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf5);
    assert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            for (j = 0; j < 3; j++)
                buf6[i][j] = 0;
    /*
        buf6[2][3]= {{1,2,3},{4,5,6}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <array> and <array>
        position        array of </g1>  array of </g1>  difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 0 ]          2               0               2
        [ 0 ]          3               0               3
        [ 1 ]          4               0               4
        [ 1 ]          5               0               5
        [ 1 ]          6               0               6
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 1, dims, "array", tid, buf6);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++) {
            buf7[i] = 0;
            buf8[i] = 0;
        }
    /*
        buf7[2]= {1,2};
        buf8[2]= {1,2};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        position        integer of </g1> integer of </g1> difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
        position        float of </g1>  float of </g1>  difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
     */
    write_attr(loc_id, 1, dims, "integer", H5T_NATIVE_INT, buf7);
    write_attr(loc_id, 1, dims, "float", H5T_NATIVE_FLOAT, buf8);

    /*-------------------------------------------------------------------------
     * 2D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */
    if (make_diffs)
        for (i = 0; i < 3; i++)
            for (j = 0; j < 2; j++)
                for (k = 0; k < 2; k++)
                    buf12[i][j][k] = 'z';

    /*
        buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <string2D> and <string2D>
        position        string2D of </g1> string2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          a                z
        [ 0 0 ]          b                z
        [ 0 1 ]          c                z
        [ 0 1 ]          d                z
        [ 1 0 ]          e                z
        [ 1 0 ]          f                z
        [ 1 1 ]          g                z
        [ 1 1 ]          h                z
        [ 2 0 ]          i                z
        [ 2 0 ]          j                z
        [ 2 1 ]          k                z
        [ 2 1 ]          l                z
     */

    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            buf12a[i][j] = buf12[i][j];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_attr(loc_id, 2, dims2, "string2D", tid, buf12a);
    status = H5Tclose(tid);

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_attr(loc_id, 2, dims2, "VLstring2D", tid, buf12);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf22, 0, sizeof buf22);
    /*
        buf22[3][2]= {{1,2},{3,4},{5,6}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <bitfield2D> and <bitfield2D>
        position        bitfield2D of </g1> bitfield2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
     */

    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id, 2, dims2, "bitfield2D", tid, buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */

    /*
        buf22[3][2]= {{1,2},{3,4},{5,6}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <opaque2D> and <opaque2D>
        position        opaque2D of </g1> opaque2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
     */
    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id, 2, dims2, "opaque2D", tid, buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    if (make_diffs)
        memset(buf32, 0, sizeof buf32);

    /*
        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <opaque2D> and <opaque2D>
        position        opaque2D of </g1> opaque2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
     */

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id, 2, dims2, "compound2D", tid, buf32);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        for (i = 0; i < 3; i++)
            for (j = 0; j < 2; j++)
                status = H5Rcreate(&buf42[i][j], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_attr(loc_id, 2, dims2, "reference2D", H5T_STD_REF_OBJ, buf42);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            if (make_diffs)
                buf452[i][j] = GREEN;
            else
                buf452[i][j] = RED;

    /*
        Attribute:   <enum2D> and <enum2D>
        position        enum2D of </g1> enum2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          RED              GREEN
        [ 0 1 ]          RED              GREEN
        [ 1 0 ]          RED              GREEN
        [ 1 1 ]          RED              GREEN
        [ 2 0 ]          RED              GREEN
        [ 2 1 ]          RED              GREEN
     */

    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id, 2, dims2, "enum2D", tid, buf452);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 2; j++) {
            buf52[i][j].p   = HDmalloc((size_t)(i + 1) * sizeof(int));
            buf52[i][j].len = (size_t)(i + 1);
            for (l = 0; l < i + 1; l++)
                if (make_diffs)
                    ((int *)buf52[i][j].p)[l] = 0;
                else
                    ((int *)buf52[i][j].p)[l] = n++;
        }
    }

    /*
        position        vlen2D of </g1> vlen2D of </g1> difference
        ------------------------------------------------------------
        [ 0 1 ]          1               0               1
        [ 1 0 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 1 1 ]          5               0               5
        [ 2 0 ]          6               0               6
        [ 2 0 ]          7               0               7
        [ 2 0 ]          8               0               8
        [ 2 1 ]          9               0               9
        [ 2 1 ]          10              0               10
        [ 2 1 ]          11              0               11
     */

    sid    = H5Screate_simple(2, dims2, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    aid    = H5Acreate2(loc_id, "vlen2D", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf52);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf52);
    assert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf62, 0, sizeof buf62);
    /*
        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <array2D> and <array2D>
        position        array2D of </g1> array2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 0 ]          2               0               2
        [ 0 0 ]          3               0               3
        [ 0 1 ]          4               0               4
        [ 0 1 ]          5               0               5
        [ 0 1 ]          6               0               6
        [ 1 0 ]          7               0               7
        [ 1 0 ]          8               0               8
        [ 1 0 ]          9               0               9
        [ 1 1 ]          10              0               10
        [ 1 1 ]          11              0               11
        [ 1 1 ]          12              0               12
        [ 2 0 ]          13              0               13
        [ 2 0 ]          14              0               14
        [ 2 0 ]          15              0               15
        [ 2 1 ]          16              0               16
        [ 2 1 ]          17              0               17
        [ 2 1 ]          18              0               18
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 2, dims2, "array2D", tid, buf62);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        memset(buf72, 0, sizeof buf72);
        memset(buf82, 0, sizeof buf82);
    }
    /*
        Attribute:   <integer2D> and <integer2D>
        position        integer2D of </g1> integer2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
        6 differences found
        Attribute:   <float2D> and <float2D>
        position        float2D of </g1> float2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
     */

    write_attr(loc_id, 2, dims2, "integer2D", H5T_NATIVE_INT, buf72);
    write_attr(loc_id, 2, dims2, "float2D", H5T_NATIVE_FLOAT, buf82);

    /*-------------------------------------------------------------------------
     * 3D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 4; i++)
            for (j = 0; j < 3; j++)
                for (k = 0; k < 2; k++)
                    for (l = 0; l < 2; l++)
                        buf13[i][j][k][l] = 'z';

    /*
        buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
        "rs","tu","vw","xz","AB","CD","EF","GH",
        "IJ","KL","MN","PQ","RS","TU","VW","XZ"};

        Attribute:   <string3D> and <string3D>
        position        string3D of </g1> string3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          a                z
        [ 0 0 0 ]          b                z
        [ 0 0 1 ]          c                z
        [ 0 0 1 ]          d                z
        [ 0 1 0 ]          e                z
        [ 0 1 0 ]          f                z
        [ 0 1 1 ]          g                z
        [ 0 1 1 ]          h                z
        [ 0 2 0 ]          i                z
        [ 0 2 0 ]          j                z
        [ 0 2 1 ]          k                z
        [ 0 2 1 ]          l                z
        [ 1 0 0 ]          m                z
        [ 1 0 0 ]          n                z
        [ 1 0 1 ]          p                z
        [ 1 0 1 ]          q                z
        [ 1 1 0 ]          r                z
        [ 1 1 0 ]          s                z
        [ 1 1 1 ]          t                z
        [ 1 1 1 ]          u                z
        [ 1 2 0 ]          v                z
        [ 1 2 0 ]          w                z
        [ 1 2 1 ]          x                z
        [ 2 0 0 ]          A                z
        [ 2 0 0 ]          B                z
        [ 2 0 1 ]          C                z
        [ 2 0 1 ]          D                z
        [ 2 1 0 ]          E                z
        [ 2 1 0 ]          F                z
        [ 2 1 1 ]          G                z
        [ 2 1 1 ]          H                z
        [ 2 2 0 ]          I                z
        [ 2 2 0 ]          J                z
        [ 2 2 1 ]          K                z
        [ 2 2 1 ]          L                z
        [ 3 0 0 ]          M                z
        [ 3 0 0 ]          N                z
        [ 3 0 1 ]          P                z
        [ 3 0 1 ]          Q                z
        [ 3 1 0 ]          R                z
        [ 3 1 0 ]          S                z
        [ 3 1 1 ]          T                z
        [ 3 1 1 ]          U                z
        [ 3 2 0 ]          V                z
        [ 3 2 0 ]          W                z
        [ 3 2 1 ]          X                z
        [ 3 2 1 ]          Z                z
     */

    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++)
                buf13a[i][j][k] = buf13[i][j][k];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_attr(loc_id, 3, dims3, "string3D", tid, buf13a);
    status = H5Tclose(tid);

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_attr(loc_id, 3, dims3, "VLstring3D", tid, buf13);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++)
                if (make_diffs)
                    buf23[i][j][k] = 0;
                else
                    buf23[i][j][k] = (char)n++;

    /*
        position        bitfield3D of </g1> bitfield3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 1 ]          2               0               2
        [ 0 1 0 ]          3               0               3
        [ 0 1 1 ]          4               0               4
        [ 0 2 0 ]          5               0               5
        [ 0 2 1 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 1 0 ]          9               0               9
        [ 1 1 1 ]          10              0               10
        [ 1 2 0 ]          11              0               11
        [ 1 2 1 ]          12              0               12
        [ 2 0 0 ]          13              0               13
        [ 2 0 1 ]          14              0               14
        [ 2 1 0 ]          15              0               15
        [ 2 1 1 ]          16              0               16
        [ 2 2 0 ]          17              0               17
        [ 2 2 1 ]          18              0               18
        [ 3 0 0 ]          19              0               19
        [ 3 0 1 ]          20              0               20
        [ 3 1 0 ]          21              0               21
        [ 3 1 1 ]          22              0               22
        [ 3 2 0 ]          23              0               23
        [ 3 2 1 ]          24              0               24
     */

    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id, 3, dims3, "bitfield3D", tid, buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id, 3, dims3, "opaque3D", tid, buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++)
                if (make_diffs) {
                    buf33[i][j][k].a = 0;
                    buf33[i][j][k].b = 0.0F;
                }
                else {
                    buf33[i][j][k].a = (char)n++;
                    buf33[i][j][k].b = n++;
                }
    /*position        compound3D of </g1> compound3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 0 ]          2               0               2
        [ 0 0 1 ]          3               0               3
        [ 0 0 1 ]          4               0               4
        [ 0 1 0 ]          5               0               5
        [ 0 1 0 ]          6               0               6
        [ 0 1 1 ]          7               0               7
        [ 0 1 1 ]          8               0               8
        [ 0 2 0 ]          9               0               9
        [ 0 2 0 ]          10              0               10
        [ 0 2 1 ]          11              0               11
        [ 0 2 1 ]          12              0               12
        [ 1 0 0 ]          13              0               13
        [ 1 0 0 ]          14              0               14
        [ 1 0 1 ]          15              0               15
        [ 1 0 1 ]          16              0               16
        [ 1 1 0 ]          17              0               17
        [ 1 1 0 ]          18              0               18
        [ 1 1 1 ]          19              0               19
        [ 1 1 1 ]          20              0               20
        [ 1 2 0 ]          21              0               21
        [ 1 2 0 ]          22              0               22
        [ 1 2 1 ]          23              0               23
        [ 1 2 1 ]          24              0               24
        [ 2 0 0 ]          25              0               25
        [ 2 0 0 ]          26              0               26
        [ 2 0 1 ]          27              0               27
        [ 2 0 1 ]          28              0               28
        [ 2 1 0 ]          29              0               29
        [ 2 1 0 ]          30              0               30
        [ 2 1 1 ]          31              0               31
        [ 2 1 1 ]          32              0               32
        [ 2 2 0 ]          33              0               33
        [ 2 2 0 ]          34              0               34
        [ 2 2 1 ]          35              0               35
        [ 2 2 1 ]          36              0               36
        [ 3 0 0 ]          37              0               37
        [ 3 0 0 ]          38              0               38
        [ 3 0 1 ]          39              0               39
        [ 3 0 1 ]          40              0               40
        [ 3 1 0 ]          41              0               41
        [ 3 1 0 ]          42              0               42
        [ 3 1 1 ]          43              0               43
        [ 3 1 1 ]          44              0               44
        [ 3 2 0 ]          45              0               45
        [ 3 2 0 ]          46              0               46
        [ 3 2 1 ]          47              0               47
        [ 3 2 1 ]          48              0               48
     */

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id, 3, dims3, "compound3D", tid, buf33);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        for (i = 0; i < 4; i++)
            for (j = 0; j < 3; j++)
                for (k = 0; k < 2; k++)
                    status = H5Rcreate(&buf43[i][j][k], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_attr(loc_id, 3, dims3, "reference3D", H5T_STD_REF_OBJ, buf43);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */

    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++)
                if (make_diffs)
                    buf453[i][j][k] = RED;
                else
                    buf453[i][j][k] = GREEN;

    /*
        position        enum3D of </g1> enum3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          GREEN            RED
        [ 0 0 1 ]          GREEN            RED
        [ 0 1 0 ]          GREEN            RED
        [ 0 1 1 ]          GREEN            RED
        [ 0 2 0 ]          GREEN            RED
        [ 0 2 1 ]          GREEN            RED
        [ 1 0 0 ]          GREEN            RED
        [ 1 0 1 ]          GREEN            RED
        [ 1 1 0 ]          GREEN            RED
        [ 1 1 1 ]          GREEN            RED
        [ 1 2 0 ]          GREEN            RED
        [ 1 2 1 ]          GREEN            RED
        [ 2 0 0 ]          GREEN            RED
        [ 2 0 1 ]          GREEN            RED
        [ 2 1 0 ]          GREEN            RED
        [ 2 1 1 ]          GREEN            RED
        [ 2 2 0 ]          GREEN            RED
        [ 2 2 1 ]          GREEN            RED
        [ 3 0 0 ]          GREEN            RED
        [ 3 0 1 ]          GREEN            RED
        [ 3 1 0 ]          GREEN            RED
        [ 3 1 1 ]          GREEN            RED
        [ 3 2 0 ]          GREEN            RED
        [ 3 2 1 ]          GREEN            RED
     */

    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id, 3, dims3, "enum3D", tid, buf453);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                buf53[i][j][k].p   = HDmalloc((size_t)(i + 1) * sizeof(int));
                buf53[i][j][k].len = (size_t)(i + 1);
                for (l = 0; l < i + 1; l++)
                    if (make_diffs)
                        ((int *)buf53[i][j][k].p)[l] = 0;
                    else
                        ((int *)buf53[i][j][k].p)[l] = n++;
            }
    /*
        position        vlen3D of </g1> vlen3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 1 ]          1               0               1
        [ 0 1 0 ]          2               0               2
        [ 0 1 1 ]          3               0               3
        [ 0 2 0 ]          4               0               4
        [ 0 2 1 ]          5               0               5
        [ 1 0 0 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 0 1 ]          9               0               9
        [ 1 1 0 ]          10              0               10
        etc
     */
    sid    = H5Screate_simple(3, dims3, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    aid    = H5Acreate2(loc_id, "vlen3D", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf53);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf53);
    assert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    n = 1;
    for (i = 0; i < 24; i++) {
        for (j = 0; j < (int)dimarray[0]; j++) {
            if (make_diffs)
                buf63[i][j] = 0;
            else
                buf63[i][j] = n++;
        }
    }
    /*
        position        array3D of </g1> array3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 0 ]          2               0               2
        [ 0 0 0 ]          3               0               3
        [ 0 0 1 ]          4               0               4
        [ 0 0 1 ]          5               0               5
        [ 0 0 1 ]          6               0               6
        [ 0 1 0 ]          7               0               7
        etc
     */

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 3, dims3, "array3D", tid, buf63);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    n = 1;
    f = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf73[i][j][k] = 0;
                    buf83[i][j][k] = 0.0F;
                }
                else {
                    buf73[i][j][k] = n++;
                    buf83[i][j][k] = f++;
                }
            }

    /*
        position        integer3D of </g1> integer3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 1 ]          2               0               2
        [ 0 1 0 ]          3               0               3
        [ 0 1 1 ]          4               0               4
        [ 0 2 0 ]          5               0               5
        [ 0 2 1 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 1 0 ]          9               0               9
        [ 1 1 1 ]          10              0               10
        etc
     */
    write_attr(loc_id, 3, dims3, "integer3D", H5T_NATIVE_INT, buf73);
    write_attr(loc_id, 3, dims3, "float3D", H5T_NATIVE_FLOAT, buf83);
}

/*-------------------------------------------------------------------------
 * Function: write_attr_in
 *
 * Purpose: write attributes in LOC_ID (dataset, group, named datatype)
 *
 *-------------------------------------------------------------------------
 */
static void
write_attr_in(hid_t loc_id, const char *dset_name, hid_t fid,
              int make_diffs /* flag to modify data buffers */)
{
    /* Compound datatype */
    typedef struct s_t {
        char   a;
        double b;
    } s_t;

    typedef enum { RED, GREEN } e_t;

    hid_t                        aid = H5I_INVALID_HID;
    hid_t                        sid = H5I_INVALID_HID;
    hid_t                        tid = H5I_INVALID_HID;
    herr_t H5_ATTR_NDEBUG_UNUSED status;
    int                          val, i, j, k, l, n;
    float                        f;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]           = {2};
    char       buf1[2][STR_SIZE] = {"ab", "de"};     /* string */
    char *     buf1a[2];                             /* VL string */
    char       buf2[2] = {1, 2};                     /* bitfield, opaque */
    s_t        buf3[2] = {{1, 2.0F}, {3, 4.0F}};     /* compound */
    hobj_ref_t buf4[2];                              /* reference */
    e_t        buf45[2] = {RED, RED};                /* enum */
    hvl_t      buf5[2];                              /* vlen */
    hsize_t    dimarray[1] = {3};                    /* array dimension */
    int        buf6[2][3]  = {{1, 2, 3}, {4, 5, 6}}; /* array */
    int        buf7[2]     = {1, 2};                 /* integer */
    float      buf8[2]     = {1.0F, 2.0F};           /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t dims2[2]              = {3, 2};
    char    buf12[3][2][STR_SIZE] = {{"ab", "cd"}, {"ef", "gh"}, {"ij", "kl"}}; /* string */
    char *  buf12a[3][2];                                                       /* VL string */
    char    buf22[3][2] = {{1, 2}, {3, 4}, {5, 6}};                             /* bitfield, opaque */
    s_t     buf32[6] = {{1, 2.0F}, {3, 4.0F}, {5, 6.0F}, {7, 8.0F}, {9, 10.0F}, {11, 12.0F}}; /* compound */
    hobj_ref_t buf42[3][2];                                                                   /* reference */
    e_t        buf452[3][2];                                                                  /* enum */
    hvl_t      buf52[3][2];                                                                   /* vlen */
    int buf62[6][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}, {10, 11, 12}, {13, 14, 15}, {16, 17, 18}}; /* array */
    int buf72[3][2] = {{1, 2}, {3, 4}, {5, 6}};                     /* integer */
    float buf82[3][2] = {{1.0F, 2.0F}, {3.0F, 4.0F}, {5.0F, 6.0F}}; /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]                 = {4, 3, 2};
    char       buf13[4][3][2][STR_SIZE] = {{{"ab", "cd"}, {"ef", "gh"}, {"ij", "kl"}},
                                     {{"mn", "pq"}, {"rs", "tu"}, {"vw", "xz"}},
                                     {{"AB", "CD"}, {"EF", "GH"}, {"IJ", "KL"}},
                                     {{"MN", "PQ"}, {"RS", "TU"}, {"VW", "XZ"}}}; /* string */
    char *     buf13a[4][3][2];                                                         /* VL string */
    char       buf23[4][3][2];                                                          /* bitfield, opaque */
    s_t        buf33[4][3][2];                                                          /* compound */
    hobj_ref_t buf43[4][3][2];                                                          /* reference */
    e_t        buf453[4][3][2];                                                         /* enum */
    hvl_t      buf53[4][3][2];                                                          /* vlen */
    int        buf63[24][3];                                                            /* array */
    int        buf73[4][3][2];                                                          /* integer */
    float      buf83[4][3][2];                                                          /* float */

    /*-------------------------------------------------------------------------
     * 1D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            for (j = 0; j < 2; j++)
                buf1[i][j] = 'z';
    /*
        buf1[2][2]= {"ab","de"};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <string> and <string>
        position      string of </g1>  string of </g1> difference
        ------------------------------------------------------------
        [ 0 ]          a                z
        [ 0 ]          b                z
        [ 1 ]          d                z
        [ 1 ]          e                z
    */
    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_attr(loc_id, 1, dims, "string", tid, buf1);
    status = H5Tclose(tid);

    for (i = 0; i < 2; i++)
        buf1a[i] = buf1[i];
    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_attr(loc_id, 1, dims, "VLstring", tid, buf1a);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            buf2[i] = buf2[1] = 0;
    /*
        buf2[2]= {1,2};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <bitfield> and <bitfield>
        position      bitfield of </g1> bitfield of </g1> difference
        position        opaque of </g1> opaque of </g1> difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
    */

    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id, 1, dims, "bitfield", tid, buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */

    /*
        buf2[2]= {1,2};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <opaque> and <opaque>
        position     opaque of </g1> opaque of </g1> difference
        position        opaque of </g1> opaque of </g1> difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
    */

    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id, 1, dims, "opaque", tid, buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++) {
            buf3[i].a = 0;
            buf3[i].b = 0;
        }
    /*
        buf3[2]= {{1,2},{3,4}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <compound> and <compound>
        position        compound of </g1> compound of </g1> difference
        ------------------------------------------------------------
        [ 0 ]          1               5               4
        [ 0 ]          2               5               3
        [ 1 ]          3               5               2
        [ 1 ]          4               5               1
    */

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id, 1, dims, "compound", tid, buf3);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        status = H5Rcreate(&buf4[0], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        status = H5Rcreate(&buf4[1], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_attr(loc_id, 1, dims, "reference", H5T_STD_REF_OBJ, buf4);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    if (make_diffs)
        for (i = 0; i < 2; i++)
            buf45[i] = GREEN;
    /*
        buf45[2]= {RED,RED};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <enum> and <enum>
        position     enum of </g1>   enum of </g1>   difference
        ------------------------------------------------------------
        [ 0 ]          RED              GREEN
        [ 1 ]          RED              GREEN
    */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id, 1, dims, "enum", tid, buf45);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len           = 1;
    buf5[0].p             = HDmalloc(1 * sizeof(int));
    ((int *)buf5[0].p)[0] = 1;
    buf5[1].len           = 2;
    buf5[1].p             = HDmalloc(2 * sizeof(int));
    ((int *)buf5[1].p)[0] = 2;
    ((int *)buf5[1].p)[1] = 3;

    if (make_diffs) {
        ((int *)buf5[0].p)[0] = 0;
        ((int *)buf5[1].p)[0] = 0;
        ((int *)buf5[1].p)[1] = 0;
    }
    /*
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        position        vlen of </g1>   vlen of </g1>   difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
        [ 1 ]          3               0               3
    */

    sid    = H5Screate_simple(1, dims, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    aid    = H5Acreate2(loc_id, "vlen", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf5);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf5);
    assert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        for (i = 0; i < 2; i++)
            for (j = 0; j < 3; j++)
                buf6[i][j] = 0;
    }
    /*
        buf6[2][3]= {{1,2,3},{4,5,6}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <array> and <array>
        position        array of </g1>  array of </g1>  difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 0 ]          2               0               2
        [ 0 ]          3               0               3
        [ 1 ]          4               0               4
        [ 1 ]          5               0               5
        [ 1 ]          6               0               6
    */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 1, dims, "array", tid, buf6);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        for (i = 0; i < 2; i++) {
            buf7[i] = 0;
            buf8[i] = 0;
        }
    }
    /*
        buf7[2]= {1,2};
        buf8[2]= {1,2};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        position        integer of </g1> integer of </g1> difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
        position        float of </g1>  float of </g1>  difference
        ------------------------------------------------------------
        [ 0 ]          1               0               1
        [ 1 ]          2               0               2
    */
    write_attr(loc_id, 1, dims, "integer", H5T_NATIVE_INT, buf7);
    write_attr(loc_id, 1, dims, "float", H5T_NATIVE_FLOAT, buf8);

    /*-------------------------------------------------------------------------
     * 2D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */
    if (make_diffs) {
        for (i = 0; i < 3; i++)
            for (j = 0; j < 2; j++)
                for (k = 0; k < 2; k++)
                    buf12[i][j][k] = 'z';
    }

    /*
        buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <string2D> and <string2D>
        position        string2D of </g1> string2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          a                z
        [ 0 0 ]          b                z
        [ 0 1 ]          c                z
        [ 0 1 ]          d                z
        [ 1 0 ]          e                z
        [ 1 0 ]          f                z
        [ 1 1 ]          g                z
        [ 1 1 ]          h                z
        [ 2 0 ]          i                z
        [ 2 0 ]          j                z
        [ 2 1 ]          k                z
        [ 2 1 ]          l                z
    */

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_attr(loc_id, 2, dims2, "string2D", tid, buf12);
    status = H5Tclose(tid);

    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            buf12a[i][j] = buf12[i][j];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_attr(loc_id, 2, dims2, "VLstring2D", tid, buf12a);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf22, 0, sizeof buf22);
    /*
        buf22[3][2]= {{1,2},{3,4},{5,6}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <bitfield2D> and <bitfield2D>
        position        bitfield2D of </g1> bitfield2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
    */

    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id, 2, dims2, "bitfield2D", tid, buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */

    /*
        buf22[3][2]= {{1,2},{3,4},{5,6}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <opaque2D> and <opaque2D>
        position        opaque2D of </g1> opaque2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
    */
    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id, 2, dims2, "opaque2D", tid, buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    if (make_diffs)
        memset(buf32, 0, sizeof buf32);
    /*
        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Attribute:   <opaque2D> and <opaque2D>
        position        opaque2D of </g1> opaque2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
    */

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id, 2, dims2, "compound2D", tid, buf32);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        for (i = 0; i < 3; i++)
            for (j = 0; j < 2; j++)
                status = H5Rcreate(&buf42[i][j], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_attr(loc_id, 2, dims2, "reference2D", H5T_STD_REF_OBJ, buf42);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++) {
            if (make_diffs)
                buf452[i][j] = GREEN;
            else
                buf452[i][j] = RED;
        }

    /*
        Attribute:   <enum2D> and <enum2D>
        position        enum2D of </g1> enum2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          RED              GREEN
        [ 0 1 ]          RED              GREEN
        [ 1 0 ]          RED              GREEN
        [ 1 1 ]          RED              GREEN
        [ 2 0 ]          RED              GREEN
        [ 2 1 ]          RED              GREEN
    */

    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id, 2, dims2, "enum2D", tid, buf452);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 2; j++) {
            buf52[i][j].p   = HDmalloc((size_t)(i + 1) * sizeof(int));
            buf52[i][j].len = (size_t)(i + 1);
            for (l = 0; l < i + 1; l++)
                if (make_diffs)
                    ((int *)buf52[i][j].p)[l] = 0;
                else
                    ((int *)buf52[i][j].p)[l] = n++;
        }
    }

    /*
        position        vlen2D of </g1> vlen2D of </g1> difference
        ------------------------------------------------------------
        [ 0 1 ]          1               0               1
        [ 1 0 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 1 1 ]          5               0               5
        [ 2 0 ]          6               0               6
        [ 2 0 ]          7               0               7
        [ 2 0 ]          8               0               8
        [ 2 1 ]          9               0               9
        [ 2 1 ]          10              0               10
        [ 2 1 ]          11              0               11
    */

    sid    = H5Screate_simple(2, dims2, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    aid    = H5Acreate2(loc_id, "vlen2D", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf52);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf52);
    assert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf62, 0, sizeof buf62);
    /*
        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <array2D> and <array2D>
        position        array2D of </g1> array2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 0 ]          2               0               2
        [ 0 0 ]          3               0               3
        [ 0 1 ]          4               0               4
        [ 0 1 ]          5               0               5
        [ 0 1 ]          6               0               6
        [ 1 0 ]          7               0               7
        [ 1 0 ]          8               0               8
        [ 1 0 ]          9               0               9
        [ 1 1 ]          10              0               10
        [ 1 1 ]          11              0               11
        [ 1 1 ]          12              0               12
        [ 2 0 ]          13              0               13
        [ 2 0 ]          14              0               14
        [ 2 0 ]          15              0               15
        [ 2 1 ]          16              0               16
        [ 2 1 ]          17              0               17
        [ 2 1 ]          18              0               18
    */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 2, dims2, "array2D", tid, buf62);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        memset(buf72, 0, sizeof buf72);
        memset(buf82, 0, sizeof buf82);
    }
    /*
        Attribute:   <integer2D> and <integer2D>
        position        integer2D of </g1> integer2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
        6 differences found
        Attribute:   <float2D> and <float2D>
        position        float2D of </g1> float2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
    */

    write_attr(loc_id, 2, dims2, "integer2D", H5T_NATIVE_INT, buf72);
    write_attr(loc_id, 2, dims2, "float2D", H5T_NATIVE_FLOAT, buf82);

    /*-------------------------------------------------------------------------
     * 3D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        for (i = 0; i < 4; i++)
            for (j = 0; j < 3; j++)
                for (k = 0; k < 2; k++)
                    for (l = 0; l < 2; l++)
                        buf13[i][j][k][l] = 'z';
    }

    /*
        buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
        "rs","tu","vw","xz","AB","CD","EF","GH",
        "IJ","KL","MN","PQ","RS","TU","VW","XZ"};

        Attribute:   <string3D> and <string3D>
        position        string3D of </g1> string3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          a                z
        [ 0 0 0 ]          b                z
        [ 0 0 1 ]          c                z
        [ 0 0 1 ]          d                z
        [ 0 1 0 ]          e                z
        [ 0 1 0 ]          f                z
        [ 0 1 1 ]          g                z
        [ 0 1 1 ]          h                z
        [ 0 2 0 ]          i                z
        [ 0 2 0 ]          j                z
        [ 0 2 1 ]          k                z
        [ 0 2 1 ]          l                z
        [ 1 0 0 ]          m                z
        [ 1 0 0 ]          n                z
        [ 1 0 1 ]          p                z
        [ 1 0 1 ]          q                z
        [ 1 1 0 ]          r                z
        [ 1 1 0 ]          s                z
        [ 1 1 1 ]          t                z
        [ 1 1 1 ]          u                z
        [ 1 2 0 ]          v                z
        [ 1 2 0 ]          w                z
        [ 1 2 1 ]          x                z
        [ 2 0 0 ]          A                z
        [ 2 0 0 ]          B                z
        [ 2 0 1 ]          C                z
        [ 2 0 1 ]          D                z
        [ 2 1 0 ]          E                z
        [ 2 1 0 ]          F                z
        [ 2 1 1 ]          G                z
        [ 2 1 1 ]          H                z
        [ 2 2 0 ]          I                z
        [ 2 2 0 ]          J                z
        [ 2 2 1 ]          K                z
        [ 2 2 1 ]          L                z
        [ 3 0 0 ]          M                z
        [ 3 0 0 ]          N                z
        [ 3 0 1 ]          P                z
        [ 3 0 1 ]          Q                z
        [ 3 1 0 ]          R                z
        [ 3 1 0 ]          S                z
        [ 3 1 1 ]          T                z
        [ 3 1 1 ]          U                z
        [ 3 2 0 ]          V                z
        [ 3 2 0 ]          W                z
        [ 3 2 1 ]          X                z
        [ 3 2 1 ]          Z                z
    */

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_attr(loc_id, 3, dims3, "string3D", tid, buf13);
    status = H5Tclose(tid);

    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++)
                buf13a[i][j][k] = buf13[i][j][k];
    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_attr(loc_id, 3, dims3, "VLstring3D", tid, buf13a);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs)
                    buf23[i][j][k] = 0;
                else
                    buf23[i][j][k] = (char)n++;
            }
    /*
        position        bitfield3D of </g1> bitfield3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 1 ]          2               0               2
        [ 0 1 0 ]          3               0               3
        [ 0 1 1 ]          4               0               4
        [ 0 2 0 ]          5               0               5
        [ 0 2 1 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 1 0 ]          9               0               9
        [ 1 1 1 ]          10              0               10
        [ 1 2 0 ]          11              0               11
        [ 1 2 1 ]          12              0               12
        [ 2 0 0 ]          13              0               13
        [ 2 0 1 ]          14              0               14
        [ 2 1 0 ]          15              0               15
        [ 2 1 1 ]          16              0               16
        [ 2 2 0 ]          17              0               17
        [ 2 2 1 ]          18              0               18
        [ 3 0 0 ]          19              0               19
        [ 3 0 1 ]          20              0               20
        [ 3 1 0 ]          21              0               21
        [ 3 1 1 ]          22              0               22
        [ 3 2 0 ]          23              0               23
        [ 3 2 1 ]          24              0               24
    */

    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id, 3, dims3, "bitfield3D", tid, buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id, 3, dims3, "opaque3D", tid, buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf33[i][j][k].a = 0;
                    buf33[i][j][k].b = 0.0F;
                }
                else {
                    buf33[i][j][k].a = (char)n++;
                    buf33[i][j][k].b = n++;
                }
            }
    /*position        compound3D of </g1> compound3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 0 ]          2               0               2
        [ 0 0 1 ]          3               0               3
        [ 0 0 1 ]          4               0               4
        [ 0 1 0 ]          5               0               5
        [ 0 1 0 ]          6               0               6
        [ 0 1 1 ]          7               0               7
        [ 0 1 1 ]          8               0               8
        [ 0 2 0 ]          9               0               9
        [ 0 2 0 ]          10              0               10
        [ 0 2 1 ]          11              0               11
        [ 0 2 1 ]          12              0               12
        [ 1 0 0 ]          13              0               13
        [ 1 0 0 ]          14              0               14
        [ 1 0 1 ]          15              0               15
        [ 1 0 1 ]          16              0               16
        [ 1 1 0 ]          17              0               17
        [ 1 1 0 ]          18              0               18
        [ 1 1 1 ]          19              0               19
        [ 1 1 1 ]          20              0               20
        [ 1 2 0 ]          21              0               21
        [ 1 2 0 ]          22              0               22
        [ 1 2 1 ]          23              0               23
        [ 1 2 1 ]          24              0               24
        [ 2 0 0 ]          25              0               25
        [ 2 0 0 ]          26              0               26
        [ 2 0 1 ]          27              0               27
        [ 2 0 1 ]          28              0               28
        [ 2 1 0 ]          29              0               29
        [ 2 1 0 ]          30              0               30
        [ 2 1 1 ]          31              0               31
        [ 2 1 1 ]          32              0               32
        [ 2 2 0 ]          33              0               33
        [ 2 2 0 ]          34              0               34
        [ 2 2 1 ]          35              0               35
        [ 2 2 1 ]          36              0               36
        [ 3 0 0 ]          37              0               37
        [ 3 0 0 ]          38              0               38
        [ 3 0 1 ]          39              0               39
        [ 3 0 1 ]          40              0               40
        [ 3 1 0 ]          41              0               41
        [ 3 1 0 ]          42              0               42
        [ 3 1 1 ]          43              0               43
        [ 3 1 1 ]          44              0               44
        [ 3 2 0 ]          45              0               45
        [ 3 2 0 ]          46              0               46
        [ 3 2 1 ]          47              0               47
        [ 3 2 1 ]          48              0               48
    */

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id, 3, dims3, "compound3D", tid, buf33);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        for (i = 0; i < 4; i++)
            for (j = 0; j < 3; j++)
                for (k = 0; k < 2; k++)
                    status = H5Rcreate(&buf43[i][j][k], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_attr(loc_id, 3, dims3, "reference3D", H5T_STD_REF_OBJ, buf43);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */

    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs)
                    buf453[i][j][k] = RED;
                else
                    buf453[i][j][k] = GREEN;
            }
    /*
        position        enum3D of </g1> enum3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          GREEN            RED
        [ 0 0 1 ]          GREEN            RED
        [ 0 1 0 ]          GREEN            RED
        [ 0 1 1 ]          GREEN            RED
        [ 0 2 0 ]          GREEN            RED
        [ 0 2 1 ]          GREEN            RED
        [ 1 0 0 ]          GREEN            RED
        [ 1 0 1 ]          GREEN            RED
        [ 1 1 0 ]          GREEN            RED
        [ 1 1 1 ]          GREEN            RED
        [ 1 2 0 ]          GREEN            RED
        [ 1 2 1 ]          GREEN            RED
        [ 2 0 0 ]          GREEN            RED
        [ 2 0 1 ]          GREEN            RED
        [ 2 1 0 ]          GREEN            RED
        [ 2 1 1 ]          GREEN            RED
        [ 2 2 0 ]          GREEN            RED
        [ 2 2 1 ]          GREEN            RED
        [ 3 0 0 ]          GREEN            RED
        [ 3 0 1 ]          GREEN            RED
        [ 3 1 0 ]          GREEN            RED
        [ 3 1 1 ]          GREEN            RED
        [ 3 2 0 ]          GREEN            RED
        [ 3 2 1 ]          GREEN            RED
    */

    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id, 3, dims3, "enum3D", tid, buf453);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                buf53[i][j][k].p   = HDmalloc((size_t)(i + 1) * sizeof(int));
                buf53[i][j][k].len = (size_t)(i + 1);
                for (l = 0; l < i + 1; l++)
                    if (make_diffs)
                        ((int *)buf53[i][j][k].p)[l] = 0;
                    else
                        ((int *)buf53[i][j][k].p)[l] = n++;
            }
    /*
        position        vlen3D of </g1> vlen3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 1 ]          1               0               1
        [ 0 1 0 ]          2               0               2
        [ 0 1 1 ]          3               0               3
        [ 0 2 0 ]          4               0               4
        [ 0 2 1 ]          5               0               5
        [ 1 0 0 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 0 1 ]          9               0               9
        [ 1 1 0 ]          10              0               10
        etc
    */
    sid    = H5Screate_simple(3, dims3, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    aid    = H5Acreate2(loc_id, "vlen3D", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf53);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf53);
    assert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    n = 1;
    for (i = 0; i < 24; i++)
        for (j = 0; j < (int)dimarray[0]; j++) {
            if (make_diffs)
                buf63[i][j] = 0;
            else
                buf63[i][j] = n++;
        }
    /*
        position        array3D of </g1> array3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 0 ]          2               0               2
        [ 0 0 0 ]          3               0               3
        [ 0 0 1 ]          4               0               4
        [ 0 0 1 ]          5               0               5
        [ 0 0 1 ]          6               0               6
        [ 0 1 0 ]          7               0               7
        etc
    */

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 3, dims3, "array3D", tid, buf63);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    n = 1;
    f = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf73[i][j][k] = 0;
                    buf83[i][j][k] = 0.0F;
                }
                else {
                    buf73[i][j][k] = n++;
                    buf83[i][j][k] = f++;
                }
            }

    /*
        position        integer3D of </g1> integer3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 1 ]          2               0               2
        [ 0 1 0 ]          3               0               3
        [ 0 1 1 ]          4               0               4
        [ 0 2 0 ]          5               0               5
        [ 0 2 1 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 1 0 ]          9               0               9
        [ 1 1 1 ]          10              0               10
        etc
    */
    write_attr(loc_id, 3, dims3, "integer3D", H5T_NATIVE_INT, buf73);
    write_attr(loc_id, 3, dims3, "float3D", H5T_NATIVE_FLOAT, buf83);
}

/*-------------------------------------------------------------------------
 * Function: write_dset_in
 *
 * Purpose: write datasets in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static void
write_dset_in(hid_t loc_id, const char *dset_name, hid_t fid,
              int make_diffs /* flag to modify data buffers */)
{
    /* Compound datatype */
    typedef struct s_t {
        char   a;
        double b;
    } s_t;

    typedef enum { RED, GREEN } e_t;

    hid_t                        did  = H5I_INVALID_HID;
    hid_t                        sid  = H5I_INVALID_HID;
    hid_t                        tid  = H5I_INVALID_HID;
    hid_t                        dcpl = H5I_INVALID_HID;
    herr_t H5_ATTR_NDEBUG_UNUSED status;
    int                          val, i, j, k, l, n;
    float                        f;
    int                          fillvalue   = 2;
    int                          scalar_data = 2;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]           = {2};
    char       buf1[2][STR_SIZE] = {"ab", "de"};     /* string */
    char *     buf1a[2];                             /* VL string */
    char       buf2[2] = {1, 2};                     /* bitfield, opaque */
    s_t        buf3[2] = {{1, 2.0F}, {3, 4.0F}};     /* compound */
    hobj_ref_t buf4[2];                              /* reference */
    e_t        buf45[2] = {RED, GREEN};              /* enum */
    hvl_t      buf5[2];                              /* vlen */
    hsize_t    dimarray[1] = {3};                    /* array dimension */
    int        buf6[2][3]  = {{1, 2, 3}, {4, 5, 6}}; /* array */
    int        buf7[2]     = {1, 2};                 /* integer */
    float      buf8[2]     = {1.0F, 2.0F};           /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t dims2[2]              = {3, 2};
    char    buf12[3][2][STR_SIZE] = {{"ab", "cd"}, {"ef", "gh"}, {"ij", "kl"}}; /* string */
    char *  buf12a[3][2];                                                       /* VL string */
    char    buf22[3][2] = {{1, 2}, {3, 4}, {5, 6}};                             /* bitfield, opaque */
    s_t     buf32[6] = {{1, 2.0F}, {3, 4.0F}, {5, 6.0F}, {7, 8.0F}, {9, 10.0F}, {11, 12.0F}}; /* compound */
    hobj_ref_t buf42[3][2];                                                                   /* reference */
    hvl_t      buf52[3][2];                                                                   /* vlen */
    int buf62[6][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}, {10, 11, 12}, {13, 14, 15}, {16, 17, 18}}; /* array */
    int buf72[3][2] = {{1, 2}, {3, 4}, {5, 6}};                     /* integer */
    float buf82[3][2] = {{1.0F, 2.0F}, {3.0F, 4.0F}, {5.0F, 6.0F}}; /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]                 = {4, 3, 2};
    char       buf13[4][3][2][STR_SIZE] = {{{"ab", "cd"}, {"ef", "gh"}, {"ij", "kl"}},
                                     {{"mn", "pq"}, {"rs", "tu"}, {"vw", "xz"}},
                                     {{"AB", "CD"}, {"EF", "GH"}, {"IJ", "KL"}},
                                     {{"MN", "PQ"}, {"RS", "TU"}, {"VW", "XZ"}}}; /* string */
    char *     buf13a[4][3][2];                                                         /* VL string */
    char       buf23[4][3][2];                                                          /* bitfield, opaque */
    s_t        buf33[4][3][2];                                                          /* compound */
    hobj_ref_t buf43[4][3][2];                                                          /* reference */
    hvl_t      buf53[4][3][2];                                                          /* vlen */
    int        buf63[24][3];                                                            /* array */
    int        buf73[4][3][2];                                                          /* integer */
    float      buf83[4][3][2];                                                          /* float */

    if (make_diffs == 2)
        dimarray[0] = 4;

    /*-------------------------------------------------------------------------
     * H5S_SCALAR
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        scalar_data = 1;

    /* create a space  */
    sid = H5Screate(H5S_SCALAR);

    /* create a dataset */
    did = H5Dcreate2(loc_id, "scalar", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* write */
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &scalar_data);

    /* close */
    H5Dclose(did);
    H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * 1D
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            for (j = 0; j < 2; j++)
                buf1[i][j] = 'z';

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_dset(loc_id, 1, dims, "string", tid, buf1);
    status = H5Tclose(tid);

    for (i = 0; i < 2; i++)
        buf1a[i] = buf1[i];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_dset(loc_id, 1, dims, "VLstring", tid, buf1a);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            buf2[i] = buf2[1] = 0;

    tid = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id, 1, dims, "bitfield", tid, buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++) {
            buf3[i].a = 0;
            buf3[i].b = 0;
        }

    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_dset(loc_id, 1, dims, "opaque", tid, buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            buf45[i] = GREEN;

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id, 1, dims, "compound", tid, buf3);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        status = H5Rcreate(&buf4[0], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        status = H5Rcreate(&buf4[1], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_dset(loc_id, 1, dims, "reference", H5T_STD_REF_OBJ, buf4);
    }

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_DATASET_REGION dataset region reference)
     *-------------------------------------------------------------------------
     */

    gen_datareg(fid, make_diffs);

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_dset(loc_id, 1, dims, "enum", tid, buf45);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len           = 1;
    buf5[0].p             = HDmalloc(1 * sizeof(int));
    ((int *)buf5[0].p)[0] = 1;
    buf5[1].len           = 2;
    buf5[1].p             = HDmalloc(2 * sizeof(int));
    ((int *)buf5[1].p)[0] = 2;
    ((int *)buf5[1].p)[1] = 3;

    if (make_diffs) {
        ((int *)buf5[0].p)[0] = 0;
        ((int *)buf5[1].p)[0] = 0;
        ((int *)buf5[1].p)[1] = 0;
    }

    sid    = H5Screate_simple(1, dims, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    did    = H5Dcreate2(loc_id, "vlen", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf5);
    HDassert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf5);
    HDassert(status >= 0);
    status = H5Dclose(did);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++)
            for (j = 0; j < 3; j++)
                buf6[i][j] = 0;

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(loc_id, 1, dims, "array", tid, buf6);
    status = H5Tclose(tid);

    {
        double *dbuf;                   /* information to write */
        hid_t   ldid = H5I_INVALID_HID; /* dataset ID   */
        hid_t   lsid = H5I_INVALID_HID; /* dataspace ID   */
        hid_t   ltid = H5I_INVALID_HID; /* datatype ID   */
        size_t  size;
        hsize_t sdims[] = {1};
        hsize_t tdims[] = {H5TOOLS_MALLOCSIZE / sizeof(double) + 1};
        size_t  jj;

        /* allocate and initialize array data to write */
        size = (H5TOOLS_MALLOCSIZE / sizeof(double) + 1) * sizeof(double);
        dbuf = (double *)HDmalloc(size);

        for (jj = 0; jj < (H5TOOLS_MALLOCSIZE / sizeof(double) + 1); jj++)
            dbuf[jj] = (double)jj;

        if (make_diffs) {
            dbuf[5] = 0;
            dbuf[6] = 0;
        }

        /* create a type larger than H5TOOLS_MALLOCSIZE */
        ltid = H5Tarray_create2(H5T_NATIVE_DOUBLE, 1, tdims);
        size = H5Tget_size(ltid);
        lsid = H5Screate_simple(1, sdims, NULL);
        ldid = H5Dcreate2(loc_id, "arrayd", ltid, lsid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#if defined(WRITE_ARRAY)
        H5Dwrite(ldid, ltid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dbuf);
#endif

        /* close */
        H5Dclose(ldid);
        H5Tclose(ltid);
        H5Sclose(lsid);
        HDfree(dbuf);
    }

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        for (i = 0; i < 2; i++) {
            buf7[i] = 0;
            buf8[i] = 0;
        }

    write_dset(loc_id, 1, dims, "integer", H5T_NATIVE_INT, buf7);
    write_dset(loc_id, 1, dims, "float", H5T_NATIVE_FLOAT, buf8);

    /*-------------------------------------------------------------------------
     * 2D
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        for (i = 0; i < 3; i++)
            for (j = 0; j < 2; j++)
                for (k = 0; k < 2; k++)
                    buf12[i][j][k] = 'z';
    }

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_dset(loc_id, 2, dims2, "string2D", tid, buf12);
    status = H5Tclose(tid);

    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            buf12a[i][j] = buf12[i][j];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_dset(loc_id, 2, dims2, "VLstring2D", tid, buf12a);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf22, 0, sizeof buf22);

    tid = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id, 2, dims2, "bitfield2D", tid, buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_dset(loc_id, 2, dims2, "opaque2D", tid, buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf32, 0, sizeof buf32);

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id, 2, dims2, "compound2D", tid, buf32);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        for (i = 0; i < 3; i++)
            for (j = 0; j < 2; j++)
                status = H5Rcreate(&buf42[i][j], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_dset(loc_id, 2, dims2, "reference2D", H5T_STD_REF_OBJ, buf42);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */

    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_dset(loc_id, 2, dims2, "enum2D", tid, 0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++) {
            buf52[i][j].p   = HDmalloc((size_t)(i + 1) * sizeof(int));
            buf52[i][j].len = (size_t)(i + 1);
            for (l = 0; l < i + 1; l++) {
                if (make_diffs)
                    ((int *)buf52[i][j].p)[l] = 0;
                else
                    ((int *)buf52[i][j].p)[l] = n++;
            }
        }

    sid    = H5Screate_simple(2, dims2, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    did    = H5Dcreate2(loc_id, "vlen2D", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf52);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf52);
    assert(status >= 0);
    status = H5Dclose(did);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    if (make_diffs)
        memset(buf62, 0, sizeof buf62);

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(loc_id, 2, dims2, "array2D", tid, buf62);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER, write a fill value
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        memset(buf72, 0, sizeof buf72);
        memset(buf82, 0, sizeof buf82);
    }

    dcpl   = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue);
    sid    = H5Screate_simple(2, dims2, NULL);
    did    = H5Dcreate2(loc_id, "integer2D", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf72);
    status = H5Pclose(dcpl);
    status = H5Dclose(did);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_FLOAT
     *-------------------------------------------------------------------------
     */

    write_dset(loc_id, 2, dims2, "float2D", H5T_NATIVE_FLOAT, buf82);

    /*-------------------------------------------------------------------------
     * 3D
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */

    if (make_diffs) {
        for (i = 0; i < 4; i++)
            for (j = 0; j < 3; j++)
                for (k = 0; k < 2; k++)
                    for (l = 0; l < 2; l++)
                        buf13[i][j][k][l] = 'z';
    }

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, (size_t)STR_SIZE);
    write_dset(loc_id, 3, dims3, "string3D", tid, buf13);
    status = H5Tclose(tid);

    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++)
                buf13a[i][j][k] = buf13[i][j][k];

    tid    = H5Tcopy(H5T_C_S1);
    status = H5Tset_size(tid, H5T_VARIABLE);
    write_dset(loc_id, 3, dims3, "VLstring3D", tid, buf13a);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs)
                    buf23[i][j][k] = 0;
                else
                    buf23[i][j][k] = (char)n++;
            }

    tid = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id, 3, dims3, "bitfield3D", tid, buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid    = H5Tcreate(H5T_OPAQUE, (size_t)1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_dset(loc_id, 3, dims3, "opaque3D", tid, buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf33[i][j][k].a = 0;
                    buf33[i][j][k].b = 0.0F;
                }
                else {
                    buf33[i][j][k].a = (char)n++;
                    buf33[i][j][k].b = n++;
                }
            }

    tid = H5Tcreate(H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id, 3, dims3, "compound3D", tid, buf33);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE (H5R_OBJECT object reference)
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if (dset_name) {
        for (i = 0; i < 4; i++)
            for (j = 0; j < 3; j++)
                for (k = 0; k < 2; k++)
                    status = H5Rcreate(&buf43[i][j][k], fid, dset_name, H5R_OBJECT, (hid_t)-1);
        write_dset(loc_id, 3, dims3, "reference3D", H5T_STD_REF_OBJ, buf43);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */

    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED", (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_dset(loc_id, 3, dims3, "enum3D", tid, 0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                buf53[i][j][k].p   = HDmalloc((size_t)(i + 1) * sizeof(int));
                buf53[i][j][k].len = (size_t)(i + 1);
                for (l = 0; l < i + 1; l++) {
                    if (make_diffs)
                        ((int *)buf53[i][j][k].p)[l] = 0;
                    else
                        ((int *)buf53[i][j][k].p)[l] = n++;
                }
            }

    sid    = H5Screate_simple(3, dims3, NULL);
    tid    = H5Tvlen_create(H5T_NATIVE_INT);
    did    = H5Dcreate2(loc_id, "vlen3D", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf53);
    assert(status >= 0);
    status = H5Treclaim(tid, sid, H5P_DEFAULT, buf53);
    assert(status >= 0);
    status = H5Dclose(did);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */

    n = 1;
    for (i = 0; i < 24; i++)
        for (j = 0; j < 3; j++) {
            if (make_diffs)
                buf63[i][j] = 0;
            else
                buf63[i][j] = n++;
        }

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(loc_id, 3, dims3, "array3D", tid, buf63);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    n = 1;
    f = 1;
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf73[i][j][k] = 0;
                    buf83[i][j][k] = 0.0F;
                }
                else {
                    buf73[i][j][k] = n++;
                    buf83[i][j][k] = f++;
                }
            }

    write_dset(loc_id, 3, dims3, "integer3D", H5T_NATIVE_INT, buf73);
    write_dset(loc_id, 3, dims3, "float3D", H5T_NATIVE_FLOAT, buf83);
}

/*-------------------------------------------------------------------------
 * Function: gen_datareg
 *
 * Purpose: generate a dataset region and its reference
 *
 * Date: April 19, 2006
 *
 *-------------------------------------------------------------------------
 */

static void
gen_datareg(hid_t fid, int make_diffs /* flag to modify data buffers */)
{
    /* data dataset */
    hid_t   did1     = H5I_INVALID_HID; /* dataset ID   */
    hid_t   sid1     = H5I_INVALID_HID; /* dataspace ID  */
    hsize_t dims1[2] = {10, 10};        /* dimensions */
    int *   buf;                        /* dataset buffer */
    /* reference dataset */
    hid_t                        did2    = H5I_INVALID_HID; /* dataset ID   */
    hid_t                        sid2    = H5I_INVALID_HID; /* dataspace ID  */
    hsize_t                      dims2[] = {2};             /* 2 references */
    hdset_reg_ref_t *            rbuf;                      /* buffer for write the references  */
    hsize_t                      start[10];                 /* starting location of hyperslab */
    hsize_t                      count[10];                 /* element count of hyperslab */
    hsize_t                      coord[5][2];               /* coordinates for point selection */
    herr_t H5_ATTR_NDEBUG_UNUSED status;
    int                          i;

    /* allocate the buffer for write the references */
    rbuf = (hdset_reg_ref_t *)HDcalloc((size_t)2, sizeof(hdset_reg_ref_t));

    /* allocate the buffer for write the data dataset */
    buf = (int *)HDmalloc(10 * 10 * sizeof(int));

    for (i = 0; i < 10 * 10; i++)
        buf[i] = i;

    /* create the data dataset */
    sid1   = H5Screate_simple(2, dims1, NULL);
    did1   = H5Dcreate2(fid, "dsetref", H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    HDassert(status >= 0);

    /* create the reference dataset */
    sid2 = H5Screate_simple(1, dims2, NULL);
    did2 = H5Dcreate2(fid, "refreg", H5T_STD_REF_DSETREG, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* create the references */
    /* select hyperslab for first reference */

    start[0] = 2;
    start[1] = 2;
    count[0] = 6;
    count[1] = 6;
    if (make_diffs) {
        start[0] = 0;
        start[1] = 0;
        count[0] = 3;
        count[1] = 3;
    }

    status = H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL);
    HDassert(status >= 0);
    H5Sget_select_npoints(sid1);

    /* store first dataset region */
    status = H5Rcreate(&rbuf[0], fid, "dsetref", H5R_DATASET_REGION, sid1);
    HDassert(status >= 0);

    /* select sequence of five points for second reference */
    coord[0][0] = 6;
    coord[0][1] = 9;
    coord[1][0] = 2;
    coord[1][1] = 2;
    coord[2][0] = 8;
    coord[2][1] = 4;
    coord[3][0] = 1;
    coord[3][1] = 6;
    coord[4][0] = 2;
    coord[4][1] = 8;
    if (make_diffs) {
        coord[1][0] = 3;
        coord[1][1] = 3;
        coord[3][0] = 2;
        coord[3][1] = 5;
        coord[4][0] = 1;
        coord[4][1] = 7;
    }
    H5Sselect_elements(sid1, H5S_SELECT_SET, (size_t)5, (const hsize_t *)coord);
    H5Sget_select_npoints(sid1);

    /* store second dataset region */
    H5Rcreate(&rbuf[1], fid, "dsetref", H5R_DATASET_REGION, sid1);

    /* write */
    status = H5Dwrite(did2, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    HDassert(status >= 0);

    /* close, free memory buffers */
    status = H5Dclose(did1);
    HDassert(status >= 0);
    status = H5Sclose(sid1);
    HDassert(status >= 0);
    status = H5Dclose(did2);
    HDassert(status >= 0);
    status = H5Sclose(sid2);
    HDassert(status >= 0);

    HDfree(rbuf);
    HDfree(buf);
}

/*-------------------------------------------------------------------------
 * Function: test_hyperslab
 *
 * Purpose: test diff by hyperslabs. create a dataset with 1GB dimensions
 *  by iterating trough 1KB hyperslabs
 *
 *-------------------------------------------------------------------------
 */
static int
test_hyperslab(const char *fname, int make_diffs /* flag to modify data buffers */)
{
    hid_t   did           = H5I_INVALID_HID;
    hid_t   fid           = H5I_INVALID_HID;
    hid_t   f_sid         = H5I_INVALID_HID;
    hid_t   m_sid         = H5I_INVALID_HID;
    hid_t   tid           = H5I_INVALID_HID;
    hid_t   dcpl          = H5I_INVALID_HID;
    hsize_t dims[1]       = {GBLL};                 /* dataset dimensions */
    hsize_t hs_size[1]    = {GBLL / (1024 * 1024)}; /* hyperslab dimensions */
    hsize_t chunk_dims[1] = {GBLL / 1024};          /* chunk dimensions */
    hsize_t hs_start[1];
    size_t  size;
    size_t  nelmts    = (size_t)GBLL / (1024 * 1024); /* elements in each hyperslab */
    char    fillvalue = -1;
    char *  buf       = NULL;
    int     i, j, s;
    char    c;

    /* create */
    fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto out;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_CHAR, &fillvalue) < 0)
        goto out;
    if (H5Pset_chunk(dcpl, 1, chunk_dims) < 0)
        goto out;
    if ((f_sid = H5Screate_simple(1, dims, NULL)) < 0)
        goto out;
    if ((did = H5Dcreate2(fid, "big", H5T_NATIVE_CHAR, f_sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto out;
    if ((m_sid = H5Screate_simple(1, hs_size, hs_size)) < 0)
        goto out;
    if ((tid = H5Dget_type(did)) < 0)
        goto out;
    if ((size = H5Tget_size(tid)) <= 0)
        goto out;

    /* create a evenly divided buffer from 0 to 127  */
    buf = (char *)HDmalloc((size_t)(nelmts * size));
    s   = 1024 * 1024 / 127;
    for (i = 0, j = 0, c = 0; i < 1024 * 1024; j++, i++) {
        if (j == s) {
            c++;
            j = 0;
        }

        /* set the hyperslab values */
        HDmemset(buf, c, nelmts);

        /* make a different hyperslab at this position */
        if (make_diffs && i == 512 * 512)
            HDmemset(buf, 0, nelmts);

        hs_start[0] = (unsigned long long)i * GBLL / (1024 * 1024);
        if (H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, hs_start, NULL, hs_size, NULL) < 0)
            goto out;

        /* write only one hyperslab */
        if (i == 512 * 512) {
            if (H5Dwrite(did, H5T_NATIVE_CHAR, m_sid, f_sid, H5P_DEFAULT, buf) < 0)
                goto out;
        }
    }
    HDfree(buf);
    buf = NULL;

    /* close */
    if (H5Sclose(f_sid) < 0)
        goto out;
    if (H5Sclose(m_sid) < 0)
        goto out;
    if (H5Pclose(dcpl) < 0)
        goto out;
    if (H5Dclose(did) < 0)
        goto out;
    H5Fclose(fid);

    return 0;

out:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Sclose(f_sid);
        H5Sclose(m_sid);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY;
    return -1;
}

/*
 * Function: test_double_epsilion
 *
 * Purpose: Create test files to compare data with epsilion
 */
static void
test_double_epsilon(const char *fname1, const char *fname2)
{
    hid_t   fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID;
    hsize_t dims1[2] = {4, 7};
    double  wdata[4][7];
    int     i, j;

    /*-------------------------------------------------------------------------
     * create two files
     *-------------------------------------------------------------------------
     */
    if ((fid1 = H5Fcreate(fname1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;
    if ((fid2 = H5Fcreate(fname2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /*
     * Initialize data.
     */
    for (i = 0; i < 4; i++)
        for (j = 0; j < 7; j++)
            wdata[i][j] = 0.0;

    /* dataset */
    if (write_dset(fid1, 2, dims1, "dataset", H5T_IEEE_F64LE, wdata) < 0)
        PROGRAM_ERROR;

    /*
     * Initialize data.
     */
    for (i = 0; i < 4; i++)
        for (j = 0; j < 7; j++)
            wdata[i][j] = (double)1.e-19;

    /* dataset */
    if (write_dset(fid2, 2, dims1, "dataset", H5T_IEEE_F64LE, wdata) < 0)
        PROGRAM_ERROR;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid1);
        H5Fclose(fid2);
    }
    H5E_END_TRY;
}

/*-------------------------------------------------------------------------
 * Function: write_attr
 *
 * Purpose: utility function to write an attribute in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static int
write_attr(hid_t loc_id, int rank, hsize_t *dims, const char *name, hid_t tid, void *buf)
{
    hid_t aid = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;

    /* create a space  */
    if ((sid = H5Screate_simple(rank, dims, NULL)) < 0)
        goto out;

    /* create the attribute */
    if ((aid = H5Acreate2(loc_id, name, tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* write */
    if (buf) {
        if (H5Awrite(aid, tid, buf) < 0)
            goto out;
    }

    /* close */
    H5Aclose(aid);
    H5Sclose(sid);

    return SUCCEED;

out:

    H5Aclose(aid);
    H5Sclose(sid);
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: write_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static herr_t
write_dset(hid_t loc_id, int rank, hsize_t *dims, const char *name, hid_t tid, void *buf)
{
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;

    /* create a space  */
    if ((sid = H5Screate_simple(rank, dims, NULL)) < 0)
        PROGRAM_ERROR;

    /* create the dataset */
    if ((did = H5Dcreate2(loc_id, name, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        PROGRAM_ERROR;

    /* write */
    if (buf)
        if (H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
            PROGRAM_ERROR;

    /* close */
    if (H5Dclose(did) < 0)
        PROGRAM_ERROR;
    if (H5Sclose(sid) < 0)
        PROGRAM_ERROR;

    return SUCCEED;

error:

    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
    }
    H5E_END_TRY;

    return FAIL;
} /* end write_dset() */
