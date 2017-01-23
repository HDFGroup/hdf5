/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Generate the binary hdf5 files for the h5dump tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files in the local directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include <limits.h>

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"

#define FILE1 "tgroup.h5"
#define FILE2 "tdset.h5"
#define FILE3 "tattr.h5"
#define FILE4 "tslink.h5"
#define FILE4_1 "tsoftlinks.h5"
#define FILE5 "thlink.h5"
#define FILE6 "tcompound.h5"
#define FILE7 "tall.h5"
#define FILE8 "tdset2.h5"
#define FILE9 "tcompound2.h5"
#define FILE10 "tloop.h5"
#define FILE11 "tloop2.h5"
#define FILE12 "tmany.h5"
#define FILE13 "tstr.h5"
#define FILE14 "tstr2.h5"
#define FILE15 "tenum.h5"
#define FILE16 "tobjref.h5"
#define FILE17 "tdatareg.h5"
#define FILE18 "tnestedcomp.h5"
#define FILE19 "topaque.h5"
#define FILE20 "tbitfields.h5"
#define FILE21 "tvldtypes1.h5"
#define FILE22 "tvldtypes2.h5"
#define FILE23 "tvldtypes3.h5"
#define FILE24 "tvldtypes4.h5"
#define FILE25 "tarray1.h5"
#define FILE25_BIG "tarray1_big.h5"
#define FILE26 "tarray2.h5"
#define FILE27 "tarray3.h5"
#define FILE28 "tarray4.h5"
#define FILE29 "tarray5.h5"
#define FILE30 "tarray6.h5"
#define FILE31 "tarray7.h5"
#define FILE32 "tempty.h5"
#define FILE33  "tgrp_comments.h5"
#define FILE34  "tsplit_file"
#define FILE35  "tfamily%05d.h5"
#define FILE36  "tmulti"
#define FILE37  "tlarge_objname.h5"
#define FILE38  "tvlstr.h5"
#define FILE39  "tchar.h5"
#define FILE40  "tattr2.h5"
#define FILE41  "tcompound_complex.h5"
#define FILE42  "tnamed_dtype_attr.h5"
#define FILE43  "tvldtypes5.h5"
#define FILE44  "tfilters.h5"
#define FILE45  "tnullspace.h5"
#define FILE46  "tfcontents1.h5"
#define FILE47  "tfcontents2.h5"
#define FILE48  "tfvalues.h5"
#define FILE49  "tstr3.h5"
#define FILE50  "taindices.h5"
#define FILE51  "tlonglinks.h5"
#define FILE52  "tldouble.h5"
#define FILE53  "textlink.h5"
#define FILE54  "tudlink.h5"
#define FILE55  "tbinary.h5"
#define FILE56  "tbigdims.h5"
#define FILE57  "thyperslab.h5"
#define FILE58  "tordergr.h5"
#define FILE59  "torderattr.h5"
#define FILE60  "tfpformat.h5"
#define FILE61  "textlinksrc.h5"
#define FILE62  "textlinktar.h5"
#define FILE63  "textlinkfar.h5"
#define FILE64  "tattrreg.h5"
#define FILE65  "file_space.h5"
#define FILE66  "packedbits.h5"
#define FILE67  "zerodim.h5"
#define FILE68  "charsets.h5"
#define FILE68a "tdset_idx.h5"
#define FILE69  "tattrintsize.h5"
#define FILE70  "tcmpdintsize.h5"
#define FILE71  "tcmpdattrintsize.h5"
#define FILE72  "tnestedcmpddt.h5"
#define FILE73  "tscalarintsize.h5"
#define FILE74  "tscalarattrintsize.h5"
#define FILE75  "tscalarstring.h5"
#define FILE76  "tcmpdintarray.h5"
#define FILE77  "tcmpdints.h5"
#define FILE78  "tscalarintattrsize.h5"
#define FILE79  "tintsattrs.h5"
#define FILE80  "tbitnopaque.h5"
#define FILE81  "tints4dims.h5"
#define FILE82  "tcompound_complex2.h5"
#define FILE83  "tvlenstr_array.h5"
#define FILE84  "tudfilter.h5"

/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */

/* utility functions */
static int
make_dset(hid_t loc_id, const char *name, hid_t sid, hid_t tid, hid_t dcpl, void *buf);
static int
write_attr(hid_t loc_id, int rank, hsize_t *dims, const char *attr_name,
        hid_t tid, void *buf);
static int
write_dset( hid_t loc_id, int rank, hsize_t *dims, const char *dset_name,
        hid_t tid, void *buf );

/* a filter operation callback function */
static size_t
myfilter(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
        const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
        size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf);

/* a "set local" callback     */
static herr_t
set_local_myfilter(hid_t dcpl_id, hid_t tid, hid_t H5_ATTR_UNUSED sid);

#define MYFILTER_ID 405

/* This message derives from H5Z */
const H5Z_class2_t H5Z_MYFILTER[1] = {{
        H5Z_CLASS_T_VERS,
        MYFILTER_ID,               /* Filter id number      */
        1, 1,
        "myfilter",                /* Filter name for debugging */
        NULL,                /* The "can apply" callback     */
        set_local_myfilter,  /* The "set local" callback     */
        myfilter,            /* The actual filter function */
}};

#define H5Z_FILTER_DYNLIBUD      300
#define MULTIPLIER              3

static size_t H5Z_filter_dynlibud(unsigned int flags, size_t cd_nelmts,
                const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_DYNLIBUD[1] = {{
    H5Z_CLASS_T_VERS,                /* H5Z_class_t version             */
    H5Z_FILTER_DYNLIBUD,             /* Filter id number        */
    1, 1,                            /* Encoding and decoding enabled   */
    "dynlibud",                 /* Filter name for debugging    */
    NULL,                            /* The "can apply" callback        */
    NULL,                            /* The "set local" callback        */
    (H5Z_func_t)H5Z_filter_dynlibud,    /* The actual filter function    */
}};


/* A UD link traversal function.  Shouldn't actually be called. */
static hid_t UD_traverse(H5_ATTR_UNUSED const char * link_name, H5_ATTR_UNUSED hid_t cur_group,
        H5_ATTR_UNUSED const void * udata, H5_ATTR_UNUSED size_t udata_size, H5_ATTR_UNUSED hid_t lapl_id)
{
    return -1;
}

#define MY_LINKCLASS 187

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


#define LENSTR  50
#define LENSTR2  11

#define SPACE2_RANK 2
#define SPACE2_DIM1 10
#define SPACE2_DIM2 10

#define SPACE1_RANK 1
#define SPACE1_DIM1 4

#define DIM1  20
#define DIM2  10
#define CDIM1 DIM1/2
#define CDIM2 DIM2/2
#define RANK  2

/* Dataspace of 0 dimension size */
#define SPACE3_RANK 2
#define SPACE3_DIM1 0
#define SPACE3_DIM2 0

/* Element selection information */
#define POINT1_NPOINTS 10

typedef enum{
    RED,
    GREEN,
    BLUE,
    WHITE,
    BLACK
} enumtype;

/* Compound datatype */
typedef struct s1_t {
        unsigned int a;
        unsigned int b;
        float c;
} s1_t;


/* 1-D array datatype */
#define ARRAY1_RANK 1
#define ARRAY1_DIM1 4

/* 3-D array datatype */
#define ARRAY2_RANK 3
#define ARRAY2_DIM1 3
#define ARRAY2_DIM2 4
#define ARRAY2_DIM3 5

/* 2-D array datatype */
#define ARRAY3_RANK 2
#define ARRAY3_DIM1 6
#define ARRAY3_DIM2 3

/* VL string datatype name */
/* TODO remove complier error not used, remove the link when everything is OK */
/* #define VLSTR_TYPE      "vl_string_type" */

/* "File 41" macros */
/* Name of dataset to create in datafile                              */
#define F41_DATASETNAME   "CompoundComplex"
/* Dataset dimensions                                                 */
#define F41_LENGTH         6
#define F41_RANK           1
#define F41_ARRAY_RANK     1
#define F41_ARRAY_RANKd    2
#define F41_DIMb           4
#define F41_ARRAY_DIMc     6
#define F41_ARRAY_DIMd1    5
#define F41_ARRAY_DIMd2    6
#define F41_ARRAY_DIMf     10

/* "File 42" macros */
/* Name of dataset to create in datafile                              */
#define F42_DSETNAME "Dataset"
#define F42_TYPENAME "Datatype"
#define F42_ATTRNAME "Attribute"
#define F42_LINKNAME "Link_to_Datatype"

/* "File 43" macros */
/* Name of dataset to create in datafile                              */
#define F43_DSETNAME "Dataset"

/* "File 51" macros */
#define F51_MAX_NAME_LEN    ((64*1024)+1024)

/* "File 64" macros */
#define F64_FILE            "tarray8.h5"
#define F64_DATASET         "DS1"
#define F64_DIM0            1
#define F64_ARRAY_BUF_LEN   (4*1024)
#define F64_DIM1            (F64_ARRAY_BUF_LEN / sizeof(int) + 1)

/* File 65 macros */
#define STRATEGY  H5F_FILE_SPACE_AGGR_VFD  /* File space handling strategy */
#define THRESHOLD10   10          /* Free space section threshold */

/* "FILE66" macros and for FILE69 */
#define F66_XDIM        8
#define F66_DATASETU08        "DU08BITS"
#define F66_DATASETS08        "DS08BITS"
#define F66_YDIM8        8
#define F66_DATASETU16       "DU16BITS"
#define F66_DATASETS16       "DS16BITS"
#define F66_YDIM16        16
#define F66_DATASETU32       "DU32BITS"
#define F66_DATASETS32       "DS32BITS"
#define F66_YDIM32        32
#define F66_DATASETU64       "DU64BITS"
#define F66_DATASETS64       "DS64BITS"
#define F66_YDIM64      64
#define F66_DUMMYDBL        "DummyDBL"

/* Declarations for gent_dataset_idx() for "FILE68a" */
#define F68a_DSET_FIXED        "dset_fixed"
#define F68a_DSET_FIXED_FILTER    "dset_filter"
#define F68a_DSET_BTREE        "dset_btree"
#define F68a_DIM200        200
#define F68a_DIM100        100
#define F68a_DIM20        20
#define F68a_DIM10        10
#define F68a_CHUNK        5

/* "FILE70" macros and for FILE71 */
/* Name of dataset to create in datafile   */
#define F70_DATASETNAME   "CompoundIntSize"
#define F70_LENGTH      4
#define F70_RANK        1
#define F70_ARRAY_RANK  2
#define F70_XDIM        8
#define F70_DATASETU08        "DU08BITS"
#define F70_DATASETS08        "DS08BITS"
#define F70_YDIM8       8
#define F70_DATASETU16       "DU16BITS"
#define F70_DATASETS16       "DS16BITS"
#define F70_YDIM16      16
#define F70_DATASETU32       "DU32BITS"
#define F70_DATASETS32       "DS32BITS"
#define F70_YDIM32      32
#define F70_DATASETU64       "DU64BITS"
#define F70_DATASETS64       "DS64BITS"
#define F70_YDIM64      64
#define F70_DUMMYDBL        "DummyDBL"
/* Name of dataset to create in datafile   */
#define F71_DATASETNAME   "CompoundAttrIntSize"

/* "FILE73" macros and for FILE69 and FILE78 */
#define F73_ARRAY_RANK  2
#define F73_XDIM        8
#define F73_DATASETU08        "DU08BITS"
#define F73_DATASETS08        "DS08BITS"
#define F73_YDIM8       8
#define F73_DATASETU16       "DU16BITS"
#define F73_DATASETS16       "DS16BITS"
#define F73_YDIM16      16
#define F73_DATASETU32       "DU32BITS"
#define F73_DATASETS32       "DS32BITS"
#define F73_YDIM32      32
#define F73_DATASETU64       "DU64BITS"
#define F73_DATASETS64       "DS64BITS"
#define F73_YDIM64      64
#define F73_DUMMYDBL        "DummyDBL"

/* "FILE76 and FILE77 */
/* Name of dataset to create in datafile   */
#define F76_DATASETNAME   "CompoundIntArray"
#define F76_LENGTH      4
#define F76_RANK        1
#define F76_ARRAY_RANK  1
#define F76_DATASETU08        "DU08BITS"
#define F76_DATASETS08        "DS08BITS"
#define F76_DIM8       8
#define F76_DATASETU16       "DU16BITS"
#define F76_DATASETS16       "DS16BITS"
#define F76_DIM16      16
#define F76_DATASETU32       "DU32BITS"
#define F76_DATASETS32       "DS32BITS"
#define F76_DIM32      32
#define F76_DATASETU64       "DU64BITS"
#define F76_DATASETS64       "DS64BITS"
#define F76_DIM64      64
#define F76_DUMMYDBL        "DummyDBL"
/* Name of dataset to create in datafile   */
#define F77_DATASETNAME1   "CompoundInts"
#define F77_DATASETNAME2   "CompoundRInts"
#define F77_LENGTH      64

#define F80_DIM32      32

#define F81_DATASETNAME   "FourDimInts"
#define F81_RANK        4
#define F81_WDIM        10
#define F81_XDIM        8
#define F81_YDIM        6
#define F81_ZDIM        4

/* "File 82" macros */
/* Name of dataset to create in datafile                              */
#define F82_DATASETNAME    "CompoundComplex1D"
/* Dataset dimensions                                                 */
#define F82_DIM32          32
#define F82_RANK           1
/* #define F82_RANK2          2 */
/* #define F82_RANK3          3 */
/* #define F82_RANK4          4 */

/* "File 83" macros */
/* Name of dataset to create in datafile                              */
#define F83_DATASETNAME    "ScalarArrayOfVlenStr"
#define F83_DATASETNAME2   "CompoundArrayOfVlenStr"
/* Dataset dimensions                                                 */
#define F83_DIM            5
#define F83_RANK           1
#define F83_ARRAYDIM       3

static void
gent_group(void)
{
    hid_t fid, group;

    fid = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* / */
    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* /g1 */
    group = H5Gcreate2(fid, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g1/g1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* /g2 */
    group = H5Gcreate2(fid, "/g2/g2.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* /g3 */
    group = H5Gcreate2(fid, "/g3/g3.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3/g3.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3/g3.3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3/g3.4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* /g2/g2.1 */
    group = H5Gcreate2(fid, "/g2/g2.1/g2.1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2/g2.1/g2.1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2/g2.1/g2.1.3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    H5Fclose(fid);
}

static void
gent_dataset(void)
{
    hid_t fid, dataset, space;
    hsize_t dims[2];
    int dset1[10][20];
    double dset2[30][20];
    int i, j;

    fid = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* dset1 */
    dims[0] = 10; dims[1] = 20;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 10; i++)
        for(j = 0; j < 20; j++)
            dset1[i][j] = j + i;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);

    /* dset2 */
    dims[0] = 30; dims[1] = 20;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, "/dset2", H5T_IEEE_F64BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 30; i++)
        for(j = 0; j < 20; j++)
            dset2[i][j] = 0.0001F * (float)j + (float)i;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

static void
gent_dataset2(void)
{
    hid_t fid, dataset, space, create_plist;
    hsize_t dims[2];
    hsize_t maxdims[2];
    int dset1[10][20];
    double dset2[30][10];
    int i, j;

    fid = H5Fcreate(FILE8, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    create_plist = H5Pcreate(H5P_DATASET_CREATE);
    dims[0] = 5; dims[1] = 5;
    H5Pset_chunk(create_plist, 2, dims);

    /* dset1 */
    dims[0] = 10; dims[1] = 20;
    maxdims[0] = H5S_UNLIMITED; maxdims[1] = 20;
    space = H5Screate_simple(2, dims,  maxdims);
    dataset = H5Dcreate2(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    for(i = 0; i < 10; i++)
        for(j = 0; j < 20; j++)
            dset1[i][j] = j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);

    /* dset2 */
    dims[0] = 30; dims[1] = 10;
    maxdims[0] = 30; maxdims[1] = H5S_UNLIMITED;
    space = H5Screate_simple(2, dims, maxdims);
    dataset = H5Dcreate2(fid, "/dset2", H5T_IEEE_F64BE, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    for(i = 0; i < 30; i++)
        for(j = 0; j < 10; j++)
            dset2[i][j] = j;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Pclose(create_plist);
    H5Fclose(fid);
}

static void
gent_attribute(void)
{
    hid_t fid, root, space, attr, type;
    hsize_t dims[2];
    char buf[60];
    int i, data[10];
    double d[10];
    char string[]= "string attribute";
    int point = 100;

    fid = H5Fcreate(FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* attribute 1 */
    dims[0] = 24;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(root, "/attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
    sprintf(buf, "attribute of root group");
    H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
    H5Sclose(space);
    H5Aclose(attr);

    /* attribute 2 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(root, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 10; i++) data[i] = i+1;

    H5Awrite(attr, H5T_NATIVE_INT, data);
    H5Sclose(space);
    H5Aclose(attr);

    /* attribute 3 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(root, "attr3", H5T_IEEE_F64BE, space, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 10; i++) d[i] = 0.1F * (float)i;

    H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
    H5Sclose(space);
    H5Aclose(attr);

    /* attribute 4 */
    space = H5Screate(H5S_SCALAR);
    attr = H5Acreate2(root, "attr4", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT, &point);
    H5Sclose(space);
    H5Aclose(attr);

    /* attribute 5 */
    space = H5Screate(H5S_SCALAR);
    type = H5Tcopy(H5T_C_S1);
    H5Tset_size(type, 17);
    attr = H5Acreate2(root, "attr5", type, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, type, string);

    H5Tclose(type);
    H5Sclose(space);
    H5Aclose(attr);
    H5Gclose(root);
    H5Fclose(fid);
}

static void gent_softlink(void)
{
    hid_t fid, root;

    fid = H5Fcreate(FILE4, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);
    H5Lcreate_soft("somevalue", root, "slink1", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_soft("linkvalue", root, "slink2", H5P_DEFAULT, H5P_DEFAULT);

    H5Gclose(root);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function: gent_softlink2
 *
 * Purpose: Create soft links to various objects.
 * Return:
 *    SUCCEED
 *    FAIL
 * Programmer: Jonathan Kim
 * Date: May 26, 2010
 *-------------------------------------------------------------------------*/
#define NX 4
#define NY 2
static int gent_softlink2(void)
{
    hid_t       fileid1 = -1;
    hid_t       gid1 = -1, gid2 = -1;
    hid_t       datatype = -1;
    hid_t       dset1 = -1, dset2 = -1;
    hid_t       dataspace = -1;
    hsize_t     dimsf[2];              /* dataset dimensions */
    int data1[NX][NY] = {{0,0},{1,1},{2,2},{3,3}};
    int data2[NX][NY] = {{0,0},{0,1},{0,2},{3,3}};
    herr_t      status = SUCCEED;

    /*-----------------------------------------------------------------------
     * FILE
     *------------------------------------------------------------------------*/
    /* Create a new file */
    fileid1 = H5Fcreate(FILE4_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fileid1 < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Fcreate failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Groups
     *------------------------------------------------------------------------*/
    gid1 = H5Gcreate2(fileid1, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid1 < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    gid2 = H5Gcreate2(fileid1, "group_empty", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (gid2 < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Gcreate2 failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Named datatype
     *------------------------------------------------------------------------*/
    datatype = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tcommit2(fileid1, "dtype", datatype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Tcommit2 failed.\n", FILE4_1);
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
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(2, dimsf, NULL);

    /*
     * We will store little endian INT numbers.
     */

    /*---------------
     * dset1
     */
    /* Create a new dataset as sample object */
    dset1 = H5Dcreate2(fileid1, "/dset1", H5T_NATIVE_INT, dataspace,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dset1 < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Dcreate2 failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    status = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data1);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /*---------------
     * dset2
     */
    /* Create a new dataset as sample object */
    dset2 = H5Dcreate2(fileid1, "/dset2", H5T_NATIVE_INT, dataspace,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dset2 < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Dcreate2 failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    status = H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data2);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Dwrite failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------------------------------------
     * Soft links
     *------------------------------------------------------------------------*/
    /*
     * create various soft links under  '/' root
     */
    /* link to dset1 */
    status = H5Lcreate_soft("/dset1", fileid1, "soft_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* link to data type */
    status = H5Lcreate_soft("/dtype", fileid1, "soft_dtype", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* link to group1 */
    status = H5Lcreate_soft("/group1", fileid1, "soft_group1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* link to empty group */
    status = H5Lcreate_soft("/group_empty", fileid1, "soft_empty_grp", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* dangling link */
    status = H5Lcreate_soft("not_yet", fileid1, "soft_dangle", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /*-----------------------------------------
     * create various soft links under a group
     */
    /* link to dset1 */
    status = H5Lcreate_soft("/dset1", gid1, "soft_dset1", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* link to dset2 */
    status = H5Lcreate_soft("/dset2", gid1, "soft_dset2", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* link to data type */
    status = H5Lcreate_soft("/dtype", gid1, "soft_dtype", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* link to empty group */
    status = H5Lcreate_soft("/group_empty", gid1, "soft_empty_grp", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    /* dangling link  */
    status = H5Lcreate_soft("not_yet", gid1, "soft_dangle", H5P_DEFAULT, H5P_DEFAULT);
    if (status < 0)
    {
        HDfprintf(stderr, "Error: %s> H5Lcreate_soft failed.\n", FILE4_1);
        status = FAIL;
        goto out;
    }

    out:
    /*
     * Close/release resources.
     */
    if(dataspace >= 0 && H5Sclose(dataspace) < 0) {
        HDfprintf(stderr, "Error: %s> H5Sclose failed.\n", FILE4_1);
        status = FAIL;
    }
    if(gid1 >= 0 && H5Gclose(gid1) < 0) {
        HDfprintf(stderr, "Error: %s> H5Gclose failed.\n", FILE4_1);
        status = FAIL;
    }
    if(gid2 >= 0 && H5Gclose(gid2) < 0) {
        HDfprintf(stderr, "Error: %s> H5Gclose failed.\n", FILE4_1);
        status = FAIL;
    }
    if(datatype >= 0 && H5Tclose(datatype) < 0) {
        HDfprintf(stderr, "Error: %s> H5Tclose failed.\n", FILE4_1);
        status = FAIL;
    }
    if(dset1 >= 0 && H5Dclose(dset1) < 0) {
        HDfprintf(stderr, "Error: %s> H5Dclose failed.\n", FILE4_1);
        status = FAIL;
    }
    if(dset2 >= 0 && H5Dclose(dset2) < 0) {
        HDfprintf(stderr, "Error: %s> H5Dclose failed.\n", FILE4_1);
        status = FAIL;
    }
    if(fileid1 >= 0 && H5Fclose(fileid1) < 0) {
        HDfprintf(stderr, "Error: %s> H5Fclose failed.\n", FILE4_1);
        status = FAIL;
    }

    return status;
}

/*
            /

       /    |   \      the dataset is hardlinked to three names
                       /dset1, /g1/dset2, and /g1/g1.1/dset3
     dset1 g1    g2
                       /g2 and /g1/g1.1 are hardlinked to the same object.
          /  \
       dset2 g1.1
              |
             dset3
 */

static void gent_hardlink(void)
{
    hid_t fid, group, dataset, space;
    hsize_t dim = 5;
    int i, dset[5];

    fid = H5Fcreate(FILE5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    space = H5Screate_simple(1, &dim, NULL);
    dataset = H5Dcreate2(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 5; i++) dset[i] = i;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);

    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(group, "/dset1", H5L_SAME_LOC, "dset2", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(group, "/dset1", H5L_SAME_LOC, "dset3", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gopen2(fid, "/g1", H5P_DEFAULT);
    H5Lcreate_hard(group, "/g2", H5L_SAME_LOC, "g1.1", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* create a link to the root group */
    H5Lcreate_hard(fid, "/", H5L_SAME_LOC, "g3", H5P_DEFAULT, H5P_DEFAULT);
    H5Fclose(fid);
}

static void gent_extlink(void)
{
    hid_t fid;

    /* This external link will dangle, but that's okay */
    fid = H5Fcreate(FILE53, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external("filename", "objname", fid, "extlink1", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external("anotherfile", "anotherobj", fid, "extlink2", H5P_DEFAULT, H5P_DEFAULT);

    H5Fclose(fid);
}

static void gent_udlink(void)
{
    hid_t fid;
    char buf[4];

    H5Lregister(UD_link_class);

    /* This ud link will dangle, but that's okay */
    fid = H5Fcreate(FILE54, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_ud(fid, "udlink1", (H5L_type_t)MY_LINKCLASS, NULL, 0, H5P_DEFAULT, H5P_DEFAULT);
    HDstrcpy(buf, "foo");
    H5Lcreate_ud(fid, "udlink2", (H5L_type_t)MY_LINKCLASS, buf, 4, H5P_DEFAULT, H5P_DEFAULT);

    H5Fclose(fid);
}


/*
               /
     /     |       \     \
   dset1  group1  type1 type2
           |
          dset2

 */
static void gent_compound_dt(void) {       /* test compound data type */
    hid_t fid, group, dataset, space, space3, type, type2;
    hid_t array_dt;
    typedef struct {
            int a;
            float b;
            double c;
    } dset1_t;
    dset1_t dset1[5];

    typedef struct {
            int a;
            float b;
    } dset2_t;
    dset2_t dset2[5];

    typedef struct {
            int a[4];
            float b[5][6];
    } dset3_t;
    dset3_t dset3[3][6];

    typedef struct {
            int a;
            float b;
    } dset4_t;
    dset4_t dset4[5];

    typedef struct {
            int a;
            float b;
    } dset5_t;
    dset5_t dset5[5];

    int i, j, k, l;
    unsigned ndims;
    hsize_t dim[2];

    hsize_t sdim = 5;
    hsize_t dset3_dim[2];


    for(i = 0; i < (int)sdim; i++) {
        dset1[i].a = i;
        dset1[i].b = (float)(i*i);
        dset1[i].c = (float)(1.0F/(float)(i+1));

        dset2[i].a = i;
        dset2[i].b = (float)((float)i+ (float)i*0.1F);

        dset4[i].a = i;
        dset4[i].b = (float)(i+3);

        dset5[i].a = i;
        dset5[i].b = (float)((float)i*0.1F);
    }


    fid = H5Fcreate(FILE6, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    space = H5Screate_simple(1, &sdim, NULL);

    type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
    type2 = H5Tcreate(H5T_COMPOUND, sizeof(dset1[0]));
    H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
    H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);
    H5Tinsert(type2, "a_name", HOFFSET(dset1_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "b_name", HOFFSET(dset1_t, b), H5T_NATIVE_FLOAT);
    H5Tinsert(type2, "c_name", HOFFSET(dset1_t, c), H5T_NATIVE_DOUBLE);
    dataset = H5Dcreate2(fid, "/dset1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Tclose(type2);
    H5Tclose(type);
    H5Dclose(dataset);

    /* shared data type 1 */
    type = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
    H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
    H5Tcommit2(fid, "type1", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
    H5Tinsert(type2, "int_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "float_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT);
    group = H5Gcreate2(fid, "/group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    dataset = H5Dcreate2(group, "dset2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
    H5Tclose(type2);
    H5Tclose(type);
    H5Dclose(dataset);


    /* shared data type 2 */
    type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));
    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

    ndims = 1; dim[0] = 4;

    array_dt = H5Tarray_create2(H5T_STD_I32BE, ndims, dim);
    H5Tinsert(type, "int_array", HOFFSET(dset3_t, a), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, ndims, dim);
    H5Tinsert(type2, "int_array", HOFFSET(dset3_t, a), array_dt);
    H5Tclose(array_dt);

    ndims = 2; dim[0] = 5; dim[1] = 6;

    array_dt = H5Tarray_create2(H5T_IEEE_F32BE, ndims, dim);
    H5Tinsert(type, "float_array", HOFFSET(dset3_t, b), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, ndims, dim);
    H5Tinsert(type2, "float_array", HOFFSET(dset3_t, b), array_dt);
    H5Tclose(array_dt);

    H5Tcommit2(fid, "type2", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);


    dset3_dim[0] = 3;  dset3_dim[1] = 6;
    space3 = H5Screate_simple(2, dset3_dim, NULL);
    dataset = H5Dcreate2(group, "dset3", type, space3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < (int)dset3_dim[0]; i++)
        for(j = 0; j < (int)dset3_dim[1]; j++) {
            for(k = 0; k < 4; k++)
                dset3[i][j].a[k] = k + j + i;
            for(k = 0; k < 5; k++)
                for(l = 0; l < 6; l++)
                    dset3[i][j].b[k][l] = (float)((k + 1) + l + j + i);
        }
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);
    H5Sclose(space3);
    H5Tclose(type);
    H5Tclose(type2);
    H5Dclose(dataset);

    /* shared data type 3 */
    type = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
    H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
    H5Tcommit2(group, "type3", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tinsert(type2, "int", HOFFSET(dset4_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "float", HOFFSET(dset4_t, b), H5T_NATIVE_FLOAT);
    dataset = H5Dcreate2(group, "dset4", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

    H5Tclose(type);
    H5Tclose(type2);
    H5Dclose(dataset);
    H5Gclose(group);


    /* unamed data type */
    group = H5Gcreate2(fid, "/group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    type = H5Tcreate(H5T_COMPOUND, sizeof(dset5_t));
    H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
    H5Tcommit2(group, "type4", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    type2 = H5Tcreate(H5T_COMPOUND, sizeof(dset5_t));
    H5Tinsert(type2, "int", HOFFSET(dset5_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "float", HOFFSET(dset5_t, b), H5T_NATIVE_FLOAT);
    dataset = H5Dcreate2(group, "dset5", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

    H5Ldelete(group, "type4", H5P_DEFAULT);

    H5Tclose(type);
    H5Tclose(type2);
    H5Dclose(dataset);
    H5Sclose(space);
    H5Gclose(group);

    H5Fclose(fid);
}

/*
               /
     /     |       \     \
   dset1  group1  type1 type2
           |
          dset2

 */
static void gent_compound_dt2(void) {       /* test compound data type */
    hid_t fid, group, dataset, space, type, create_plist, type2;
    hid_t array_dt;

    typedef struct {
            int a;
            float b;
            double c;
    } dset1_t;
    dset1_t dset1[10];

    typedef struct {
            int a;
            float b;
    } dset2_t;
    dset2_t dset2[10];

    typedef struct {
            int a[4];
            float b[5][6];
    } dset3_t;

    typedef struct {
            int a;
            float b;
    } dset4_t;
    dset4_t dset4[10];

    typedef struct {
            int a;
            float b;
    } dset5_t;
    dset5_t dset5[10];

    int i;
    unsigned ndims;
    hsize_t dim[2];

    hsize_t sdim, maxdim;

    sdim = 10;
    for(i = 0; i < (int)sdim; i++) {
        dset1[i].a = i;
        dset1[i].b = (float)(i*i);
        dset1[i].c = (float)(1.0F / (float)(i+ 1));

        dset2[i].a = i;
        dset2[i].b = (float)((float)i + (float)i * 0.1F);

        dset4[i].a = i;
        dset4[i].b = (float)((float)i * 1.0F);

        dset5[i].a = i;
        dset5[i].b = (float)((float)i * 1.0F);
    }

    fid = H5Fcreate(FILE9, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    create_plist = H5Pcreate(H5P_DATASET_CREATE);

    sdim = 2;
    H5Pset_chunk(create_plist, 1, &sdim);

    sdim = 6;
    maxdim = H5S_UNLIMITED;

    space = H5Screate_simple(1, &sdim, &maxdim);

    type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

    H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32BE);
    H5Tinsert(type, "c_name", HOFFSET(dset1_t, c), H5T_IEEE_F64BE);

    dataset = H5Dcreate2(fid, "/dset1", type, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

    H5Tinsert(type2, "a_name", HOFFSET(dset1_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "b_name", HOFFSET(dset1_t, b), H5T_NATIVE_FLOAT);
    H5Tinsert(type2, "c_name", HOFFSET(dset1_t, c), H5T_NATIVE_DOUBLE);

    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

    H5Tclose(type);
    H5Tclose(type2);
    H5Sclose(space);
    H5Dclose(dataset);

    sdim = 6;
    maxdim = 10;

    space = H5Screate_simple(1, &sdim, &maxdim);

    /* shared data type 1 */
    type = H5Tcreate(H5T_COMPOUND, sizeof(dset2_t));
    H5Tinsert(type, "int_name", HOFFSET(dset2_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "float_name", HOFFSET(dset2_t, b), H5T_IEEE_F32BE);
    H5Tcommit2(fid, "type1", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate2(fid, "/group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    dataset = H5Dcreate2(group, "dset2", type, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2_t));
    H5Tinsert(type2, "int_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "float_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Tclose(type);
    H5Tclose(type2);
    H5Dclose(dataset);


    /* shared data type 2 */
    type = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

    ndims = 1; dim[0] = 4;
    array_dt = H5Tarray_create2(H5T_STD_I32BE, ndims, dim);
    H5Tinsert(type, "int_array", HOFFSET(dset3_t, a), array_dt);
    H5Tclose(array_dt);

    ndims = 2; dim[0] = 5; dim[1] = 6;
    array_dt = H5Tarray_create2(H5T_IEEE_F32BE, ndims, dim);
    H5Tinsert(type, "float_array", HOFFSET(dset3_t, b), array_dt);
    H5Tclose(array_dt);

    H5Tcommit2(fid, "type2", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(type);

    /* shared data type 3 */
    type = H5Tcreate(H5T_COMPOUND, sizeof(dset4_t));
    H5Tinsert(type, "int", HOFFSET(dset4_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "float", HOFFSET(dset4_t, b), H5T_IEEE_F32BE);
    H5Tcommit2(group, "type3", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    dataset = H5Dcreate2(group, "dset4", type, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset4_t));
    H5Tinsert(type2, "int", HOFFSET(dset4_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "float", HOFFSET(dset4_t, b), H5T_NATIVE_FLOAT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

    H5Tclose(type);
    H5Tclose(type2);
    H5Dclose(dataset);
    H5Gclose(group);


    /* unamed data type */
    group = H5Gcreate2(fid, "/group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    type = H5Tcreate(H5T_COMPOUND, sizeof(dset5_t));
    H5Tinsert(type, "int", HOFFSET(dset5_t, a), H5T_STD_I32BE);
    H5Tinsert(type, "float", HOFFSET(dset5_t, b), H5T_IEEE_F32BE);
    H5Tcommit2(group, "type4", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset5", type, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);
    type2 = H5Tcreate(H5T_COMPOUND, sizeof(dset5_t));
    H5Tinsert(type2, "int", HOFFSET(dset5_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "float", HOFFSET(dset5_t, b), H5T_NATIVE_FLOAT);
    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset5);

    H5Ldelete(group, "type4", H5P_DEFAULT);

    H5Tclose(type);
    H5Tclose(type2);
    H5Dclose(dataset);
    H5Sclose(space);
    H5Gclose(group);
    H5Pclose(create_plist);

    H5Fclose(fid);

}


/*

/ : g1  g2  attr1  attr2
g1 : g1.1  g1.2
g1.1 : dset1.1.1(attr1, attr2)   dset1.1.2
g1.2 : g1.2.1 extlink
g1.2.1 : slink
g2 : dset2.1  dset2.2 udlink

 */

static void gent_all(void)
{
    hid_t fid, group, attr, dataset, space;
    hsize_t dims[2];
    int data[2][2], dset1[10][10], dset2[20];
    char buf[60];
    int i, j;
    float dset2_1[10], dset2_2[3][5];

    fid = H5Fcreate(FILE7, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* create groups */
    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g1/g1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g1/g1.2/g1.2.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* root attributes */
    group = H5Gopen2(fid, "/", H5P_DEFAULT);

    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(group, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
    sprintf(buf, "abcdefghi");
    H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
    H5Sclose(space);
    H5Aclose(attr);

    dims[0] = 2; dims[1] = 2;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(group, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT);
    data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
    H5Awrite(attr, H5T_NATIVE_INT, data);
    H5Sclose(space);
    H5Aclose(attr);

    H5Gclose(group);

    group = H5Gopen2(fid, "/g1/g1.1", H5P_DEFAULT);

    /* dset1.1.1 */
    dims[0] = 10; dims[1] = 10;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(group, "dset1.1.1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 10; i++)
        for(j = 0; j < 10; j++)
            dset1[i][j] = j * i;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);

    /* attributes of dset1.1.1 */
    dims[0] = 27;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(dataset, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
    sprintf(buf, "1st attribute of dset1.1.1");
    H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
    H5Sclose(space);
    H5Aclose(attr);

    dims[0] = 27;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(dataset, "attr2", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT);
    sprintf(buf, "2nd attribute of dset1.1.1");
    H5Awrite(attr, H5T_NATIVE_SCHAR, buf);
    H5Sclose(space);
    H5Aclose(attr);

    H5Dclose(dataset);

    /* dset1.1.2 */
    dims[0] = 20;
    space = H5Screate_simple(1, dims, NULL);
    dataset = H5Dcreate2(group, "dset1.1.2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 20; i++)
        dset2[i] = i;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);
    H5Sclose(space);
    H5Dclose(dataset);

    H5Gclose(group);

    /* external link */
    H5Lcreate_external("somefile", "somepath", fid, "/g1/g1.2/extlink", H5P_DEFAULT, H5P_DEFAULT);

    /* soft link */
    group = H5Gopen2(fid, "/g1/g1.2/g1.2.1", H5P_DEFAULT);
    H5Lcreate_soft("somevalue", group, "slink", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gopen2(fid, "/g2", H5P_DEFAULT);

    /* dset2.1 */
    dims[0] = 10;
    space = H5Screate_simple(1, dims, NULL);
    dataset = H5Dcreate2(group, "dset2.1", H5T_IEEE_F32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 10; i++)
        dset2_1[i] = (float)((float)i * 0.1F + 1);
    H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1);
    H5Sclose(space);
    H5Dclose(dataset);

    /* dset2.2 */
    dims[0] = 3; dims[1] = 5;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(group, "dset2.2", H5T_IEEE_F32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 3; i++)
        for(j = 0; j < 5; j++)
            dset2_2[i][j] = (float)((float)(i + 1) * (float)j * 0.1F);
    H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2);
    H5Sclose(space);
    H5Dclose(dataset);

    H5Gclose(group);

    /* user-defined link */
    H5Lregister(UD_link_class);
    H5Lcreate_ud(fid, "/g2/udlink", (H5L_type_t)MY_LINKCLASS, NULL, 0, H5P_DEFAULT, H5P_DEFAULT);

    H5Fclose(fid);
}

/*
            o
          /___\
      g1 o/   \o g2
          \___/


o - group objects

 */

static void gent_loop(void) {
    hid_t fid, group;

    fid = H5Fcreate(FILE10, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    H5Lcreate_hard(fid, "/g2", H5L_SAME_LOC, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(fid, "/g1", H5L_SAME_LOC, "/g2/g2.1", H5P_DEFAULT, H5P_DEFAULT);

    H5Fclose(fid);
}

static void gent_loop2(void)
{
    hid_t fid, group;

    fid = H5Fcreate(FILE11, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* create group object g1 and implcit path from root object */
    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* create group object g2 and implcit path from root object */
    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* create path from object at /g1 to object at /g2 and name it g1.1 */
    H5Lcreate_hard(fid, "/g2", H5L_SAME_LOC, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT);

    /* create path from object at /g2 to object at /g1 and name it g2.1 */
    H5Lcreate_soft("/g1", fid, "/g2/g2.1", H5P_DEFAULT, H5P_DEFAULT);

    H5Fclose(fid);
}

/*
                  /
     |       |       |   \    \    \     \      \
     g1     g2      g3   g4   g5    g6    g7     g8
    / \      |       |    \     \    \      \      \
 g1.1 g1.2 slink2  link3 dset2 slink4 dset3 slink5   elink
  |    |    (g1)  (dset2)      (dset3)      (elink)  udlink
 dset1 link1                                slink6
      (dset1)                               (udlink)
 */

static void
gent_many(void)
{
    hid_t fid, group, attr, dataset, space, space2, type, create_plist, type2;
    hid_t array_dt;
    hsize_t dims[2];
    int data[2][2], dset2[10][10], dset3[10][10];
    double d[10];

    char buf[60];
    int i, j;
    int i0, i1, i2, i3;
    hsize_t sdim, maxdim;

    typedef struct { /* compound type has members with rank > 1 */
            int a[2][2][2][2]; /* arrays are 2x2x2x2    */
            double b[2][2][2][2];
            double c[2][2][2][2];
    } dset1_t;
    dset1_t dset1[6];

    hsize_t dim[4];
    herr_t ret;

    fid = H5Fcreate(FILE12, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    create_plist = H5Pcreate(H5P_DATASET_CREATE);

    sdim = 2;
    H5Pset_chunk(create_plist, 1, &sdim);

    group = H5Gcreate2(fid, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

    dim[0] = dim[1] = dim[2] = dim[3] = 2;
    array_dt = H5Tarray_create2(H5T_STD_I32BE, 4, dim);
    H5Tinsert(type, "a_array", HOFFSET(dset1_t, a), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_IEEE_F64BE, 4, dim);
    H5Tinsert(type, "b_array", HOFFSET(dset1_t, b), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_IEEE_F64BE, 4, dim);
    H5Tinsert(type, "c_array", HOFFSET(dset1_t, c), array_dt);
    H5Tclose(array_dt);

    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 4, dim);
    H5Tinsert(type2, "a_array", HOFFSET(dset1_t, a), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_DOUBLE, 4, dim);
    H5Tinsert(type2, "b_array", HOFFSET(dset1_t, b), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_DOUBLE, 4, dim);
    H5Tinsert(type2, "c_array", HOFFSET(dset1_t, c), array_dt);
    H5Tclose(array_dt);


    /* dset1 */
    sdim = 6;
    maxdim = H5S_UNLIMITED;
    space = H5Screate_simple(1, &sdim, &maxdim);
    dataset = H5Dcreate2(group, "dset1", type, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    /* add attributes to dset1 */
    dims[0] = 10;
    space2 = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(dataset, "attr1", H5T_STD_I8BE, space2, H5P_DEFAULT, H5P_DEFAULT);
    sprintf(buf, "abcdefghi");
    H5Awrite(attr, H5T_NATIVE_CHAR, buf);
    H5Sclose(space2);
    H5Aclose(attr);

    dims[0] = 2; dims[1] = 2;
    space2 = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(dataset, "attr2", H5T_STD_I32BE, space2, H5P_DEFAULT, H5P_DEFAULT);
    data[0][0] = 0; data[0][1] = 1; data[1][0] = 2; data[1][1] = 3;
    H5Awrite(attr, H5T_NATIVE_INT, data);
    H5Sclose(space2);
    H5Aclose(attr);

    dims[0] = 10;
    space2 = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(dataset, "attr3", H5T_IEEE_F64BE, space2, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 10; i++)
        d[i] = 0.1F * (float)i;
    H5Awrite(attr, H5T_NATIVE_DOUBLE, d);
    H5Sclose(space2);
    H5Aclose(attr);

    for(j=0; j<(int)sdim; j++) {
        for(i3 = 0; i3 < 2; i3++) {
            for(i2 = 0; i2 < 2; i2++) {
                for(i1 = 0; i1 < 2; i1++) {
                    for(i0 = 0; i0 < 2; i0++) {
                        dset1[j].a[i3][i2][i1][i0] = i0 + j;
                        dset1[j].b[i3][i2][i1][i0] = (double)(i0 + j);
                        dset1[j].c[i3][i2][i1][i0] = (double)((hsize_t)i0 + (hsize_t)j + sdim);
                    }
                }
            }
        }
    }

    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

    H5Dclose(dataset);
    H5Sclose(space);

    H5Tclose(type);
    H5Tclose(type2);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g1/g1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(group, "/g1/g1.1/dset1", H5L_SAME_LOC, "link1", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_soft("/g1", group, "slink2", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* dset2 */
    dims[0] = 10; dims[1] = 10;
    space = H5Screate_simple(2, dims, NULL);

    dataset = H5Dcreate2(group, "dset2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 10; i++)
        for(j = 0; j < 10; j++)
            dset2[i][j] = j;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Dclose(dataset);

    H5Sclose(space);
    H5Gclose(group);

    group = H5Gopen2(fid, "/g3", H5P_DEFAULT);
    H5Lcreate_hard(group, "/g4/dset2", H5L_SAME_LOC, "link3", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* dset3 */
    dims[0] = 10; dims[1] = 10;
    space = H5Screate_simple(2, dims, NULL);

    dataset = H5Dcreate2(group, "dset3", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 0; i < 10; i++)
        for(j = 0; j < 10; j++)
            dset3[i][j] = i;
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);

    H5Dclose(dataset);

    H5Sclose(space);
    H5Gclose(group);

    group = H5Gopen2(fid, "/g5", H5P_DEFAULT);
    H5Lcreate_soft("/g6/dset3", group, "slink4", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    H5Pclose(create_plist);

    group = H5Gcreate2(fid, "/g7", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g8", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(group);

    /* Create dangling external and UD links */
    H5Lcreate_external("somefile", "somepath", fid, "/g8/elink", H5P_DEFAULT, H5P_DEFAULT);
    H5Lregister(UD_link_class);
    H5Lcreate_ud(fid, "/g8/udlink", (H5L_type_t)MY_LINKCLASS, NULL, 0, H5P_DEFAULT, H5P_DEFAULT);

    /* Create links to external and UD links */
    ret = H5Lcreate_soft("/g8/elink", fid, "/g7/slink5", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);
    ret = H5Lcreate_soft("/g8/udlink", fid, "/g7/slink6", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    H5Fclose(fid);
}

static hid_t mkstr(int size, H5T_str_t pad) {
    hid_t type;

    if((type=H5Tcopy(H5T_C_S1)) < 0) return -1;
    if(H5Tset_size(type, (size_t)size) < 0) return -1;
    if(H5Tset_strpad(type, pad) < 0) return -1;

    return type;
}

static void gent_str(void) {
    hid_t fid, dataset, space, f_type, m_type, str_type, f_type2;
    hid_t array_dt;

    hsize_t dims1[] = { 3, 4};
    char string1[12][3] = {"s1","s2","s3","s4","s5","s6","s7","s8","s9",
            "s0","s1","s2"};

    hsize_t dims2[]={20};
    char string2[20][10] = {"ab cd ef1", "ab cd ef2", "ab cd ef3", "ab cd ef4",
            "ab cd ef5", "ab cd ef6", "ab cd ef7", "ab cd ef8",
            "ab cd ef9", "ab cd ef0", "ab cd ef1", "ab cd ef2",
            "ab cd ef3", "ab cd ef4", "ab cd ef5", "ab cd ef6",
            "ab cd ef7", "ab cd ef8", "ab cd ef9", "ab cd ef0"};

    hsize_t dims3[] = { 27};
    char string3[27][6] = {"abcd0", "abcd1", "abcd2", "abcd3",
            "abcd4", "abcd5", "abcd6", "abcd7",
            "abcd8", "abcd9", "abcd0", "abcd1",
            "abcd2", "abcd3", "abcd4", "abcd5",
            "abcd6", "abcd7", "abcd8", "abcd9",
            "abcd0", "abcd1", "abcd2", "abcd3",
            "abcd4", "abcd5", "abcd6"};

    int i, j, k, l;

    hsize_t dims4[] = { 3 };
    char string4[3][21] = { "s1234567890123456789", "s1234567890123456789",
            "s1234567890123456789"};

    hsize_t dims5[] = { 3, 6};
    typedef struct {
            int a[8][10];
            char s[12][33];
    } compound_t;
    compound_t comp1[3][6];
    hsize_t mdims[2];

    fid = H5Fcreate(FILE13, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* string 1 : nullterm string */
    space = H5Screate_simple(2, dims1, NULL);
    f_type = mkstr(5, H5T_STR_NULLTERM);
    m_type = mkstr(3, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid, "/string1", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string1);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 2 : space pad string */
    space = H5Screate_simple(1, dims2, NULL);
    f_type = mkstr(11, H5T_STR_SPACEPAD);
    m_type = mkstr(10, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid, "/string2", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string2);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 3 : null pad string */
    space = H5Screate_simple(1, dims3, NULL);
    f_type = mkstr(8, H5T_STR_NULLPAD);
    m_type = mkstr(6, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid, "/string3", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string3);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* string 4 : space pad long string */
    space = H5Screate_simple(1, dims4, NULL);
    f_type = mkstr(168, H5T_STR_SPACEPAD);
    m_type = mkstr(21, H5T_STR_NULLTERM);
    dataset = H5Dcreate2(fid, "/string4", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, m_type, H5S_ALL, H5S_ALL, H5P_DEFAULT, string4);
    H5Tclose(m_type);
    H5Tclose(f_type);
    H5Sclose(space);
    H5Dclose(dataset);

    /* compound data */
    space = H5Screate_simple(2, dims5, NULL);
    f_type = H5Tcreate (H5T_COMPOUND, sizeof(compound_t));
    f_type2 = H5Tcreate (H5T_COMPOUND, sizeof(compound_t));

    mdims[0] = 8; mdims[1] = 10;

    array_dt = H5Tarray_create2(H5T_STD_I32BE, 2, mdims);
    H5Tinsert(f_type, "int_array", HOFFSET(compound_t, a), array_dt);
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 2, mdims);
    H5Tinsert(f_type2, "int_array", HOFFSET(compound_t, a), array_dt);
    H5Tclose(array_dt);

    mdims[0] = 3; mdims[1] = 4;

    str_type = mkstr(32, H5T_STR_SPACEPAD);
    array_dt = H5Tarray_create2(str_type, 2, mdims);
    H5Tinsert(f_type, "string", HOFFSET(compound_t, s), array_dt);
    H5Tclose(array_dt);
    H5Tclose(str_type);

    str_type = mkstr(33, H5T_STR_NULLTERM);
    array_dt = H5Tarray_create2(str_type, 2, mdims);
    H5Tinsert(f_type2, "string", HOFFSET(compound_t, s), array_dt);
    H5Tclose(array_dt);
    H5Tclose(str_type);

    for(i = 0; i < 3; i++)
        for(j = 0; j < 6; j++) {
            for(k = 0 ; k < 8; k++)
                for(l = 0; l < 10; l++)
                    comp1[i][j].a[k][l] = (l + j + k) * (l + j + k);
            for(k = 0 ; k < 12; k++)
                HDstrcpy(comp1[i][j].s[k], "abcdefgh12345678abcdefgh12345678");
        }

    dataset = H5Dcreate2(fid, "/comp1", f_type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, f_type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, comp1);

    H5Tclose(f_type);
    H5Tclose(f_type2);
    H5Sclose(space);
    H5Dclose(dataset);

    H5Fclose(fid);
}

/*
                      /
       /     /     |    \    \     \
     g1     g2    g3    g4    g5    g6
     |       |     |     |     \     \
  string1       string3       string5
         string2       string4       string6
 */

static void gent_str2(void)
{
    hid_t fid, group, attr, dataset, space, space2, mem_space, hyper_space;
    hid_t fxdlenstr, fxdlenstr2, memtype;
    hsize_t dims[1], size[1], stride[1], count[1], block[1];
    hsize_t start[1];


    int i;
    char buf[LENSTR+10];
    char buf2[3*LENSTR2];
    hsize_t sdim;

    fid = H5Fcreate(FILE14, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    fxdlenstr = H5Tcopy(H5T_C_S1);
    H5Tset_size(fxdlenstr, LENSTR);
    H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
    H5Tset_strpad(fxdlenstr, H5T_STR_NULLTERM);

    memtype = H5Tcopy(H5T_C_S1);
    H5Tset_size(memtype, LENSTR);
    H5Tset_cset(memtype, H5T_CSET_ASCII);
    H5Tset_strpad(memtype, H5T_STR_NULLTERM);

    sdim = 10;
    size[0] = sdim;
    space = H5Screate_simple(1, size, NULL);
    size[0] = 1;
    mem_space = H5Screate_simple(1,size,NULL);
    hyper_space = H5Scopy(space);

    /* dset1 */

    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset1", fxdlenstr, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* add attributes to dset1 */

    fxdlenstr2 = H5Tcopy(H5T_C_S1);
    H5Tset_size(fxdlenstr2, LENSTR2);
    H5Tset_cset(fxdlenstr2, H5T_CSET_ASCII);
    H5Tset_strpad(fxdlenstr2, H5T_STR_NULLTERM);

    dims[0] = 3;
    space2 = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(dataset, "attr1", fxdlenstr2, space2, H5P_DEFAULT, H5P_DEFAULT);
    sprintf(&(buf2[0*LENSTR2]), "0123456789");
    sprintf(&(buf2[1*LENSTR2]), "abcdefghij");
    sprintf(&(buf2[2*LENSTR2]), "ABCDEFGHIJ");
    H5Awrite(attr, fxdlenstr2, buf2);
    H5Sclose(space2);
    H5Tclose(fxdlenstr2);
    H5Aclose(attr);

    stride[0]=1;
    count[0]=1;
    block[0]=1;

    for(i = 0; (hsize_t)i < sdim; i++) {
        start[0] = (hsize_t)i;
        sprintf(buf, "This is row %1d of type H5T_STR_NULLTERM of", i);
        H5Tset_size(memtype, HDstrlen(buf)+1);
        H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
        H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
    }
    H5Dclose(dataset);
    H5Gclose(group);

    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset2", fxdlenstr, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; (hsize_t)i < sdim; i++) {
        start[0] = (hsize_t)i;
        sprintf(buf, "This is row %1d of type H5T_STR_NULLTERM of string array", i);
        H5Tset_size(memtype, HDstrlen(buf)+1);
        H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
        H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
    }
    H5Dclose(dataset);
    H5Gclose(group);


    H5Tclose(fxdlenstr);
    fxdlenstr = H5Tcopy(H5T_C_S1);
    H5Tset_size(fxdlenstr, LENSTR);
    H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
    H5Tset_strpad(fxdlenstr, H5T_STR_NULLPAD);

    group = H5Gcreate2(fid, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset3", fxdlenstr, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0;(hsize_t) i < sdim; i++) {
        start[0] = (hsize_t)i;
        sprintf(buf, "This is row %1d of type H5T_STR_NULLPAD of", i);
        H5Tset_size(memtype, HDstrlen(buf)+1);
        H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
        H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
    }
    H5Dclose(dataset);
    H5Gclose(group);


    group = H5Gcreate2(fid, "/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset4", fxdlenstr, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; (hsize_t)i < sdim; i++) {
        start[0] = (hsize_t)i;
        sprintf(buf, "This is row %1d of type H5T_STR_NULLPAD of string array", i);
        H5Tset_size(memtype, HDstrlen(buf)+1);
        H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
        H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
    }
    H5Dclose(dataset);
    H5Gclose(group);

    H5Tclose(fxdlenstr);
    fxdlenstr = H5Tcopy(H5T_C_S1);
    H5Tset_size(fxdlenstr, LENSTR);
    H5Tset_cset(fxdlenstr, H5T_CSET_ASCII);
    H5Tset_strpad(fxdlenstr, H5T_STR_SPACEPAD);

    group = H5Gcreate2(fid, "/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset5", fxdlenstr, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; (hsize_t)i < sdim; i++) {
        start[0] = (hsize_t)i;
        sprintf(buf, "This is row %1d of type H5T_STR_SPACEPAD of", i);
        H5Tset_size(memtype, HDstrlen(buf) + 1);
        H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
        H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
    }
    H5Dclose(dataset);
    H5Gclose(group);


    group = H5Gcreate2(fid, "/g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    dataset = H5Dcreate2(group, "dset6", fxdlenstr, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; (hsize_t)i < sdim; i++) {
        start[0] = (hsize_t)i;
        sprintf(buf, "This is row %1d of type H5T_STR_SPACEPAD of string array", i);
        H5Tset_size(memtype, HDstrlen(buf) + 1);
        H5Sselect_hyperslab(hyper_space, H5S_SELECT_SET, start, stride, count, block);
        H5Dwrite(dataset, memtype, mem_space, hyper_space, H5P_DEFAULT, buf);
    }

    H5Dclose(dataset);
    H5Gclose(group);
    H5Tclose(fxdlenstr);
    H5Tclose(memtype);
    H5Sclose(mem_space);
    H5Sclose(hyper_space);
    H5Sclose(space);
    H5Fclose(fid);
}

static void gent_enum(void)
{
    /*some code is taken from enum.c in the test dir */
    hid_t file, type, space, dset;
    int val;
    enumtype data[] = {RED,   GREEN, BLUE,  GREEN, WHITE,
            WHITE, BLACK, GREEN, BLUE,  RED,
            RED,   BLUE,  GREEN, BLACK, WHITE,
            RED,   WHITE, GREEN, GREEN, BLUE};
    hsize_t size[1] = {NELMTS(data)};

    file = H5Fcreate(FILE15,H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Try to test names with special characters */
    type = H5Tcreate(H5T_ENUM, sizeof(enumtype));
    H5Tenum_insert(type, "RED",   (val = 0, &val));
    H5Tenum_insert(type, "GREEN\ngreen", (val = 1, &val));
    H5Tenum_insert(type, "BLUE blue",  (val = 2, &val));
    H5Tenum_insert(type, "WHITE \"white\"", (val = 3, &val));
    H5Tenum_insert(type, "BLACK \'black\'", (val = 4, &val));
    H5Tcommit2(file, "enum normal", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    space = H5Screate_simple(1,size,NULL);
    dset = H5Dcreate2(file,"table",type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dset, type, space, space, H5P_DEFAULT, data);

    H5Dclose(dset);
    H5Sclose(space);
    H5Fclose(file);
}

static void gent_objref(void)
{
    /*some code is taken from enum.c in the test dir */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */

    hid_t  group;      /* Group ID             */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Datatype ID   */
    hsize_t  dims1[] = {SPACE1_DIM1};
    hobj_ref_t *wbuf,      /* buffer to write to disk */
    *rbuf,       /* buffer read from disk */
    *tbuf;       /* temp. buffer read from disk */
    uint32_t   *tu32;      /* Temporary pointer to uint32 data */
    int        i;          /* counting variables */
    const char *write_comment = "Foo!"; /* Comments for group */

    /* Allocate write & read buffers */
    wbuf = (hobj_ref_t*) HDmalloc(sizeof(hobj_ref_t) * SPACE1_DIM1);
    rbuf = (hobj_ref_t*) HDmalloc(sizeof(hobj_ref_t) * SPACE1_DIM1);
    tbuf = (hobj_ref_t*) HDmalloc(sizeof(hobj_ref_t) * SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE16, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Set group's comment */
    H5Oset_comment(group, write_comment);

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_STD_U32BE, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(tu32 = (uint32_t *)((void*)wbuf), i = 0; i < SPACE1_DIM1; i++)
        *tu32++ = (uint32_t)(i * 3);

    /* Write selection to disk */
    H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_STD_U8BE, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    H5Tinsert(tid1, "a", HOFFSET(s1_t,a), H5T_STD_I32BE);

    H5Tinsert(tid1, "b", HOFFSET(s1_t,b), H5T_IEEE_F32BE);

    H5Tinsert(tid1, "c", HOFFSET(s1_t,c), H5T_IEEE_F32BE);

    /* Save datatype for later */
    H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Close datatype */
    H5Tclose(tid1);

    /* Close group */
    H5Gclose(group);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Create reference to dataset */
    H5Rcreate(&wbuf[0], fid1, "/Group1/Dataset1", H5R_OBJECT, (hid_t)-1);

    /* Create reference to dataset */
    H5Rcreate(&wbuf[1], fid1, "/Group1/Dataset2", H5R_OBJECT, (hid_t)-1);

    /* Create reference to group */
    H5Rcreate(&wbuf[2], fid1, "/Group1", H5R_OBJECT, (hid_t)-1);

    /* Create reference to named datatype */
    H5Rcreate(&wbuf[3], fid1, "/Group1/Datatype1", H5R_OBJECT, (hid_t)-1);

    /* Write selection to disk */
    H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);

    /* Close disk dataspace */
    H5Sclose(sid1);

    /* Close Dataset */
    H5Dclose(dataset);

    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(tbuf);
}

static void gent_datareg(void)
{
    /*some code is taken from enum.c in the test dir */

    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dset1, /* Dataset ID   */
    dset2;      /* Dereferenced dataset ID */
    hid_t  sid1,       /* Dataspace ID #1  */
    sid2;       /* Dataspace ID #2  */
    hsize_t  dims1[] = {SPACE1_DIM1},
            dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t  start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t  stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t  count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t  block[SPACE2_RANK];     /* Block size of hyperslab */
    hsize_t  coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hdset_reg_ref_t      *wbuf,      /* buffer to write to disk */
    *rbuf;       /* buffer read from disk */
    uint8_t    *dwbuf,      /* Buffer for writing numeric data to disk */
    *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    int        i;          /* counting variables */

    /* Allocate write & read buffers */
    wbuf = (hdset_reg_ref_t*) HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf = (hdset_reg_ref_t*) HDmalloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    dwbuf = (uint8_t*) HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    drbuf = (uint8_t*) HDcalloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE17, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a dataset */
    dset2 = H5Dcreate2(fid1, "Dataset2", H5T_STD_U8BE, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(tu8 = dwbuf, i = 0; i < SPACE2_DIM1 * SPACE2_DIM2; i++)
        *tu8++ = (uint8_t)(i * 3);

    /* Write selection to disk */
    H5Dwrite(dset2, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf);

    /* Close Dataset */
    H5Dclose(dset2);

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a dataset */
    dset1 = H5Dcreate2(fid1, "Dataset1", H5T_STD_REF_DSETREG, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0] = 2; start[1] = 2;
    stride[0] = 1; stride[1] = 1;
    count[0] = 6; count[1] = 6;
    block[0] = 1; block[1] = 1;
    H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);

    H5Sget_select_npoints(sid2);

    /* Store first dataset region */
    H5Rcreate(&wbuf[0], fid1, "/Dataset2", H5R_DATASET_REGION, sid2);

    /* Select sequence of ten points for second reference */
    coord1[0][0]=6; coord1[0][1]=9;
    coord1[1][0]=2; coord1[1][1]=2;
    coord1[2][0]=8; coord1[2][1]=4;
    coord1[3][0]=1; coord1[3][1]=6;
    coord1[4][0]=2; coord1[4][1]=8;
    coord1[5][0]=3; coord1[5][1]=2;
    coord1[6][0]=0; coord1[6][1]=4;
    coord1[7][0]=9; coord1[7][1]=0;
    coord1[8][0]=7; coord1[8][1]=1;
    coord1[9][0]=3; coord1[9][1]=3;
    H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(hsize_t *)coord1);

    H5Sget_select_npoints(sid2);

    /* Store second dataset region */
    H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close disk dataspace */
    H5Sclose(sid1);

    /* Close Dataset */
    H5Dclose(dset1);

    /* Close uint8 dataset dataspace */
    H5Sclose(sid2);

    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(dwbuf);
    HDfree(drbuf);
}

static void gent_attrreg(void)
{
    /*some code is taken from enum.c in the test dir */

    hid_t  fid1;        /* HDF5 File IDs  */
    hid_t  dset1;       /* Dataset ID   */
    hid_t  dset2;       /* Dereferenced dataset ID */
    hid_t  sid1;        /* Dataspace ID #1  */
    hid_t  sid2;        /* Dataspace ID #2  */
    hid_t  sid3;        /* Dataspace ID #3  */
    hid_t  attr1;       /* Attribute ID  */
    hsize_t  dims1[] = {SPACE1_DIM1};
    hsize_t  dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t  start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t  stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t  count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t  block[SPACE2_RANK];     /* Block size of hyperslab */
    hsize_t  coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hdset_reg_ref_t      *wbuf;      /* buffer to write to disk */
    hdset_reg_ref_t      *rbuf;      /* buffer read from disk */
    uint8_t    *dwbuf;      /* Buffer for writing numeric data to disk */
    uint8_t    *drbuf;      /* Buffer for reading numeric data from disk */
    uint8_t    *tu8;        /* Temporary pointer to uint8 data */
    int        i;           /* counting variables */

    /* Allocate write & read buffers */
    wbuf = (hdset_reg_ref_t*) HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf = (hdset_reg_ref_t*) HDmalloc(sizeof(hdset_reg_ref_t)*SPACE1_DIM1);
    dwbuf = (uint8_t*) HDmalloc(sizeof(uint8_t)*SPACE2_DIM1*SPACE2_DIM2);
    drbuf = (uint8_t*) HDcalloc(sizeof(uint8_t),SPACE2_DIM1*SPACE2_DIM2);

    /* Create file */
    fid1 = H5Fcreate(FILE64, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a dataset */
    dset2 = H5Dcreate2(fid1, "Dataset2", H5T_STD_U8BE, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(tu8 = dwbuf, i = 0; i < SPACE2_DIM1 * SPACE2_DIM2; i++)
        *tu8++ = (uint8_t)(i * 3);

    /* Write selection to disk */
    H5Dwrite(dset2, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf);

    /* Close Dataset */
    H5Dclose(dset2);

    /*
     * Create dataset with a null dataspace to serve as the parent for
     * the attribute.
     */
    sid1 = H5Screate (H5S_NULL);
    dset1 = H5Dcreate2 (fid1, "Dataset1", H5T_STD_I32LE, sid1, H5P_DEFAULT,
            H5P_DEFAULT, H5P_DEFAULT);
    H5Sclose (sid1);

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0] = 2; start[1] = 2;
    stride[0] = 1; stride[1] = 1;
    count[0] = 6; count[1] = 6;
    block[0] = 1; block[1] = 1;
    H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);

    H5Sget_select_npoints(sid2);

    /* Store first dataset region */
    H5Rcreate(&wbuf[0], fid1, "/Dataset2", H5R_DATASET_REGION, sid2);

    /* Select sequence of ten points for second reference */
    coord1[0][0]=6; coord1[0][1]=9;
    coord1[1][0]=2; coord1[1][1]=2;
    coord1[2][0]=8; coord1[2][1]=4;
    coord1[3][0]=1; coord1[3][1]=6;
    coord1[4][0]=2; coord1[4][1]=8;
    coord1[5][0]=3; coord1[5][1]=2;
    coord1[6][0]=0; coord1[6][1]=4;
    coord1[7][0]=9; coord1[7][1]=0;
    coord1[8][0]=7; coord1[8][1]=1;
    coord1[9][0]=3; coord1[9][1]=3;
    H5Sselect_elements(sid2,H5S_SELECT_SET,POINT1_NPOINTS,(hsize_t *)coord1);

    H5Sget_select_npoints(sid2);

    /* Store second dataset region */
    H5Rcreate(&wbuf[1],fid1,"/Dataset2",H5R_DATASET_REGION,sid2);

    /* Create dataspace for the attribute */
    sid3 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create the attribute and write the region references to it. */
    attr1 = H5Acreate2 (dset1, "Attribute1", H5T_STD_REF_DSETREG, sid3, H5P_DEFAULT,
            H5P_DEFAULT);
    H5Awrite (attr1, H5T_STD_REF_DSETREG, wbuf);

    /* Close attribute dataspace */
    H5Sclose(sid3);

    /* Close attribute */
    H5Aclose (attr1);

    /* Close Dataset */
    H5Dclose(dset1);

    /* Close uint8 dataset dataspace */
    H5Sclose(sid2);

    /* Close file */
    H5Fclose(fid1);

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(dwbuf);
    HDfree(drbuf);
}

/*taken from Elena's compound test file*/
static void gent_nestcomp(void)
{
    /* Compound memeber of the compound datatype*/
    typedef struct cmp_t {
            char   a;
            float  b[2];
    } cmp_t;

    /* First structure  and dataset*/
    typedef struct s1_t {
            int    a;
            float  b;
            double c;
            cmp_t  d;
    } s2_t;
    hid_t      cmp_tid;    /* Handle for the compound datatype */
    hid_t      char_id;    /* Handle for the string datatype */
    hid_t      array_dt;
    hsize_t     array_dims[] = {2};    /* Dataspace dimensions */
    unsigned    ndims = 1;    /* Number of dimensions in the array field */

    s2_t       s1[10];
    hid_t      s2_tid;     /* File datatype identifier */

    int        i;
    hid_t      file, dataset, space; /* Handles */
    herr_t     status;
    hsize_t    dim[] = {10};   /* Dataspace dimensions */

    char datasetname[] = "ArrayOfStructures";


    /*
     * Initialize the data
     */
    for(i = 0; i< 10; i++) {
        s1[i].a = i;
        s1[i].b = (float)(i*i);
        s1[i].c = 1.0F/(float)(i + 1);
        s1[i].d.a = (char)(65 + i);
        s1[i].d.b[0] = -100.0F;
        s1[i].d.b[1] =  100.0F;
    }

    /*
     * Create the data space.
     */
    space = H5Screate_simple(1, dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(FILE18, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory data type.
     */
    /*
     * Create a datatype for compound field first.
     */
    cmp_tid = H5Tcreate (H5T_COMPOUND, sizeof(cmp_t));

    /* We are using C string of length one to represent "real" character */
    char_id = H5Tcopy(H5T_C_S1);
    H5Tset_strpad(char_id, H5T_STR_NULLTERM);
    H5Tinsert(cmp_tid, "char_name", HOFFSET(cmp_t, a), char_id);

    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, ndims, array_dims);
    H5Tinsert(cmp_tid, "array_name", HOFFSET(cmp_t, b), array_dt);
    H5Tclose(array_dt);

    s2_tid = H5Tcreate (H5T_COMPOUND, sizeof(s2_t));
    H5Tinsert(s2_tid, "a_name", HOFFSET(s2_t, a), H5T_NATIVE_INT);
    H5Tinsert(s2_tid, "c_name", HOFFSET(s2_t, c), H5T_NATIVE_DOUBLE);
    H5Tinsert(s2_tid, "b_name", HOFFSET(s2_t, b), H5T_NATIVE_FLOAT);

    /* Insert compound memeber created above */
    H5Tinsert(s2_tid, "d_name", HOFFSET(s2_t, d), cmp_tid);

    /*
     * Create the dataset.
     */
    dataset = H5Dcreate2(file, datasetname, s2_tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Wtite data to the dataset;
     */
    status = H5Dwrite(dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);
    if(status < 0)
        HDfprintf(stderr, "gent_nestcomp H5Dwrite failed\n");

    /*
     * Release resources
     */
    H5Tclose(s2_tid);
    H5Tclose(cmp_tid);
    H5Tclose(char_id);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file);
}

static void gent_opaque(void)
{
    hid_t file, type, dataset, space;
    char test[100][2];
    int x;
    hsize_t dim = 2;

    for(x = 0; x < 100; x++){
        test[x][0] = (char)x;
        test[x][1] = (char)(99 - x);
    }

    /*
     * Create the data space.
     */
    space = H5Screate_simple(1, &dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(FILE19, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory datatype.
     */
    type = H5Tcreate(H5T_OPAQUE, sizeof(char) * 100 * 2);
    H5Tset_tag(type, "test opaque type");

    /*
     * Create the dataset.
     */
    dataset = H5Dcreate2(file, "opaque test", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset;
     */
    H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, test);

    H5Tclose(type);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file);
}

static void gent_bitfields(void)
{
    hid_t  file, grp=-1, type=-1, space=-1, dset=-1;
    size_t  i;
    hsize_t  nelmts;
    unsigned char buf[32];

    file = H5Fcreate(FILE20, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    if((grp = H5Gcreate2(file, "typetests", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* bitfield_1 */
    nelmts = sizeof(buf);
    if((type = H5Tcopy(H5T_STD_B8LE)) < 0 ||
            (space = H5Screate_simple(1, &nelmts, NULL)) < 0 ||
            (dset = H5Dcreate2(grp, "bitfield_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    for(i = 0; i < sizeof buf; i++)
        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* bitfield_2 */
    nelmts = sizeof(buf)/2;
    if((type = H5Tcopy(H5T_STD_B16LE)) < 0 ||
            (space = H5Screate_simple(1, &nelmts, NULL)) < 0 ||
            (dset = H5Dcreate2(grp, "bitfield_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for(i = 0; i < sizeof buf; i++)
        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;
    if(H5Gclose(grp) < 0) goto error;
    H5Fclose(file);

    error:
    H5E_BEGIN_TRY {
        H5Gclose(grp);
        H5Tclose(type);
        H5Sclose(space);
        H5Dclose(dset);
    } H5E_END_TRY;
}

static void gent_vldatatypes(void)
{
    hvl_t adata, wdata[SPACE1_DIM1];
    hid_t file, dset, space, type;
    hsize_t dims[] = { SPACE1_DIM1 };
    int i;
    herr_t ret=0;

    file = H5Fcreate(FILE21, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Allocate and initialize VL dataset to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
        int j;

        wdata[i].p = HDmalloc((size_t)(i + 1) * sizeof(int));
        wdata[i].len = (size_t)(i + 1);

        for(j = 0; j < i + 1; j++)
            ((int *)wdata[i].p)[j] = i * 10 + j;
    }

    /* write out the integers in little-endian format */
    space = H5Screate_simple(SPACE1_RANK, dims, NULL);
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate2(file, "Dataset1.0", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Tclose(type);
    HDassert(ret >= 0);
    ret = H5Sclose(space);
    HDassert(ret >= 0);

    /* Allocate and initialize VL dataset to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
        int j;

        wdata[i].p = HDmalloc((size_t)(i + 1) * sizeof(float));
        wdata[i].len = (size_t)(i + 1);

        for(j = 0; j < i + 1; j++)
            ((float *)wdata[i].p)[j] = (float)((float)(i * 10) + ((float)j) / 10.0F);
    } /* end for */

    /* write out the floats in little-endian format */
    space = H5Screate_simple(SPACE1_RANK, dims, NULL);
    type = H5Tvlen_create(H5T_NATIVE_FLOAT);
    dset = H5Dcreate2(file, "Dataset2.0", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Tclose(type);
    HDassert(ret >= 0);
    ret = H5Sclose(space);
    HDassert(ret >= 0);

    /* Allocate and initialize a scalar VL dataset to write */
    adata.p = HDmalloc(37 * sizeof(int));
    adata.len = 37;

    for(i = 0; i < 37; i++)
        ((int *)adata.p)[i] = i * 2;

    /* write out scalar VL dataset in little-endian format */
    space = H5Screate_simple(0, NULL, NULL);
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate2(file, "Dataset3.0", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    ret = H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, &adata);
    HDassert(ret >= 0);
    ret = H5Dvlen_reclaim(type, space, H5P_DEFAULT, &adata);
    HDassert(ret >= 0);

    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Tclose(type);
    HDassert(ret >= 0);
    ret = H5Sclose(space);
    HDassert(ret >= 0);
    ret = H5Fclose(file);
    HDassert(ret >= 0);
}

static void
gent_vldatatypes2(void)
{
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hvl_t *t1;              /* Temporary pointer to VL information */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1, tid2; /* Datatype IDs         */
    hsize_t  dims1[] = {SPACE1_DIM1};
    unsigned       i,j,k;      /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for(i = 0; i < SPACE1_DIM1; i++) {
        wdata[i].p = (hvl_t *)HDmalloc((i + 1) * sizeof(hvl_t));
        if(wdata[i].p == NULL) {
            printf("Cannot allocate memory for VL data! i=%u\n", i);
            return;
        } /* end if */
        wdata[i].len = i + 1;
        for(t1 = (hvl_t *)wdata[i].p, j = 0; j < (i + 1); j++, t1++) {
            t1->p = (unsigned *)HDmalloc((j + 1) * sizeof(unsigned));
            if(t1->p == NULL) {
                printf("Cannot allocate memory for VL data! i=%u, j=%u\n",i,j);
                return;
            } /* end if */
            t1->len=j+1;
            for(k=0; k<(j+1); k++)
                ((unsigned int *)t1->p)[k]=i*100+j*10+k;
        } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE22, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create(H5T_NATIVE_UINT);

    /* Create the base VL type */
    tid2 = H5Tvlen_create(tid1);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid2, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Dvlen_reclaim(tid2, sid1, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);

}

static void gent_vldatatypes3(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
        hvl_t v;
    } s1;
    s1 wdata[SPACE1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1, tid2; /* Datatype IDs         */
    hsize_t  dims1[] = {SPACE1_DIM1};
    unsigned       i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].i = (int)(i * 10);
        wdata[i].f = (float)((float)(i * 20)/3.0F);
        wdata[i].v.p = HDmalloc((size_t)(i + 1) * sizeof(unsigned int));
        wdata[i].v.len = (size_t)(i + 1);
        for(j = 0; j < (i + 1); j++)
            ((unsigned int *)wdata[i].v.p)[j] = i * 10 + j;
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE23, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL datatype to refer to */
    tid1 = H5Tvlen_create(H5T_NATIVE_UINT);

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));

    /* Insert fields */
    ret = H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    HDassert(ret >= 0);
    ret = H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    HDassert(ret >= 0);
    ret = H5Tinsert(tid2, "v", HOFFSET(s1, v), tid1);
    HDassert(ret >= 0);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid2, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Dvlen_reclaim(tid2, sid1, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

static void gent_vldatatypes4(void)
{
    typedef struct {             /* Struct that the VL sequences are composed of */
        int i;
        float f;
    } s1;
    hvl_t wdata[SPACE1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1, tid2; /* Datatype IDs         */
    hsize_t  dims1[] = {SPACE1_DIM1};
    unsigned       i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize VL data to write */
    for(i=0; i<SPACE1_DIM1; i++) {
        wdata[i].p = HDmalloc((i + 1) * sizeof(s1));
        wdata[i].len = i + 1;
        for(j = 0; j < (i + 1); j++) {
            ((s1 *)wdata[i].p)[j].i = (int)(i * 10 + j);
            ((s1 *)wdata[i].p)[j].f = (float)((float)(i * 20 + j) / 3.0F);
        } /* end for */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE24, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create the base compound type */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1));

    /* Insert fields */
    ret = H5Tinsert(tid2, "i", HOFFSET(s1, i), H5T_NATIVE_INT);
    HDassert(ret >= 0);
    ret = H5Tinsert(tid2, "f", HOFFSET(s1, f), H5T_NATIVE_FLOAT);
    HDassert(ret >= 0);

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create(tid2);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Dvlen_reclaim(tid1, sid1, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

/* Generate a variable-length dataset with NULL values in it */
static void gent_vldatatypes5(void)
{
    hvl_t wdata [SPACE1_DIM1];
    hid_t  fid1;
    hid_t  dataset;
    hid_t  sid1;
    hid_t  tid1;
    hsize_t  dims1[] = {SPACE1_DIM1};
    int                 i,j;          /* counting variable */
    herr_t  ret;  /* Generic return value  */

    /* initialize data for dataset */
    for(i=0; i<SPACE1_DIM1; i++) {
        if(i%2) {
            wdata[i].len=0;
            wdata[i].p=NULL;
        } /* end if */
        else {
            wdata[i].len = (size_t)(i + 5);
            wdata[i].p = HDmalloc(sizeof(unsigned) * (size_t)(i + 5));
            for(j = 0; j < i + 5; j++)
                ((unsigned *)wdata[i].p)[j] = (unsigned)(j * 2);
        } /* end else */
    } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE43, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid1>0);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    HDassert(sid1>0);

    /* Create a datatype to refer to */
    tid1 = H5Tvlen_create(H5T_NATIVE_UINT);
    HDassert(tid1>0);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, F43_DSETNAME, tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(dataset>0);

    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    ret = H5Dclose(dataset);
    HDassert(ret >= 0);

    ret = H5Dvlen_reclaim(tid1, sid1, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    ret = H5Tclose(tid1);
    HDassert(ret >= 0);

    ret = H5Sclose (sid1);
    HDassert(ret >= 0);

    ret = H5Fclose (fid1);
    HDassert(ret >= 0);
}

/* This is big enough to make h5dump to use hyperslap to read
   from file and display portion by portion. This also prints out array indices
   via region reference for testing refion reference output.
   Note: this was added originally prepared for bug2092. before the fix h5dump didn't
         display array indices every 262 x N (N > 0) based on 2000x1000 dims.
 */
#define SPACE_ARRAY1BIG_DIM 2000
#define ARRAY1BIG_DIM 1000

static void gent_array1_big(void)
{
    int *wdata;   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Datatype ID   */
    hsize_t  sdims1[] = {SPACE_ARRAY1BIG_DIM};
    hsize_t  tdims1[] = {ARRAY1BIG_DIM};
    int        i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */


    /* for region reference dataset */
    hid_t  dset2;
    hid_t  sid2;
    hsize_t  dims2[] = {SPACE1_DIM1};
    hsize_t  start[SPACE1_RANK];     /* Starting location of hyperslab */
    hsize_t  stride[SPACE1_RANK];    /* Stride of hyperslab */
    hsize_t  count[SPACE1_RANK];     /* Element count of hyperslab */
    hsize_t  block[SPACE1_RANK];     /* Block size of hyperslab */
    hdset_reg_ref_t   *wbuf;         /* buffer to write to disk */

    start[0] = 0;
    stride[0] = 1;
    count[0] = 999;
    block[0] = 1;

    /* Allocate write & read buffers */
    wbuf = (hdset_reg_ref_t*) HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    wdata = (int *)HDmalloc(sizeof(int) * (size_t)(SPACE_ARRAY1BIG_DIM * ARRAY1BIG_DIM));

    /* Allocate and initialize array data to write */
    for(i = 0; i < SPACE_ARRAY1BIG_DIM; i++)
        for(j = 0; j < ARRAY1BIG_DIM; j++)
            *(wdata + (i * ARRAY1BIG_DIM) + j) = i * 1;

    /* Create file */
    fid1 = H5Fcreate(FILE25_BIG, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------
     * Array type dataset
     */
    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid1 = H5Tarray_create2(H5T_NATIVE_INT, ARRAY1_RANK, tdims1);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /*---------------------------
     * Region Reference dataset
     */
    /* Create dataspace for the reference dataset */
    sid2 = H5Screate_simple(SPACE1_RANK, dims2, NULL);

    /* Create a dataset */
    dset2 = H5Dcreate2(fid1, "Dataset2", H5T_STD_REF_DSETREG, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Create references */
    H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, stride, count, block);

    H5Sget_select_npoints(sid1);

    /* Create Dataset1 region */
    H5Rcreate(&wbuf[0], fid1, "/Dataset1", H5R_DATASET_REGION, sid1);

    /* Write selection to disk */
    H5Dwrite(dset2,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);

    /* Release memory */
    HDfree(wbuf);
    HDfree(wdata);
}

static void gent_array1(void)
{
    int wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++)
            wdata[i][j]=i*10+j;

    /* Create file */
    fid1 = H5Fcreate(FILE25, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid1 = H5Tarray_create2(H5T_NATIVE_INT, ARRAY1_RANK, tdims1);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

static void gent_array2(void)
{
    int wdata[SPACE1_DIM1][ARRAY2_DIM1][ARRAY2_DIM2][ARRAY2_DIM3];   /* Information to write */
    hid_t  fid;        /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid;        /* Dataspace ID   */
    hid_t  tid;        /* Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims2[] = {ARRAY2_DIM1,ARRAY2_DIM2,ARRAY2_DIM3};
    int        i,j,k,l;    /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY2_DIM1; j++)
            for(k=0; k<ARRAY2_DIM2; k++)
                for(l=0; l<ARRAY2_DIM3; l++)
                    wdata[i][j][k][l]=i*1000+j*100+k*10+l;

    /* Create file */
    fid = H5Fcreate(FILE26, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a datatype to refer to */
    tid = H5Tarray_create2(H5T_NATIVE_INT, ARRAY2_RANK, tdims2);

    /* Create a dataset */
    dataset = H5Dcreate2(fid, "Dataset1", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);
    ret = H5Sclose(sid);
    HDassert(ret >= 0);
    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}

static void gent_array3(void)
{
    int wdata[SPACE1_DIM1][ARRAY1_DIM1][ARRAY3_DIM1][ARRAY3_DIM2];   /* Information to write */
    hid_t  fid;        /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid;        /* Dataspace ID   */
    hid_t  tid1;       /* 1-D array Datatype ID */
    hid_t  tid2;       /* 2-D array Datatype ID */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    hsize_t  tdims2[] = {ARRAY3_DIM1,ARRAY3_DIM2};
    int        i,j,k,l;    /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Allocate and initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++)
            for(k=0; k<ARRAY3_DIM1; k++)
                for(l=0; l<ARRAY3_DIM2; l++)
                    wdata[i][j][k][l]=i*1000+j*100+k*10+l;

    /* Create file */
    fid = H5Fcreate(FILE27, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a 2-D datatype to refer to */
    tid2 = H5Tarray_create2(H5T_NATIVE_INT, ARRAY3_RANK, tdims2);

    /* Create a 1-D datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);

    /* Create a dataset */
    dataset = H5Dcreate2(fid, "Dataset1", tid1, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);
    ret = H5Sclose(sid);
    HDassert(ret >= 0);
    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}

static void gent_array4(void)
{
    typedef struct {        /* Typedef for compound datatype */
            int i;
            float f;
    } s2_t;
    s2_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID   */
    hid_t  tid2;       /* Compound Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j;        /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].i = i * 10 + j;
            wdata[i][j].f = (float)((float)i * 2.5F + (float)j);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE28, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

    /* Insert integer field */
    ret = H5Tinsert(tid2, "i", HOFFSET(s2_t, i), H5T_NATIVE_INT);
    HDassert(ret >= 0);

    /* Insert float field */
    ret = H5Tinsert(tid2, "f", HOFFSET(s2_t, f), H5T_NATIVE_FLOAT);
    HDassert(ret >= 0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);

    /* Close compound datatype */
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

static void gent_array5(void)
{
    typedef struct {        /* Typedef for compound datatype */
            int i;
            float f[ARRAY1_DIM1];
    } s2_t;
    s2_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID */
    hid_t  tid2;       /* Compound Datatype ID */
    hid_t  tid3;       /* Nested Array Datatype ID */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j,k;      /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].i = i * 10 + j;
            for(k=0; k<ARRAY1_DIM1; k++)
                wdata[i][j].f[k] = (float)((float)i * 10 + (float)j * 2.5F + (float)k);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE29, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

    /* Insert integer field */
    ret = H5Tinsert (tid2, "i", HOFFSET(s2_t,i), H5T_NATIVE_INT);
    HDassert(ret >= 0);

    /* Create an array of floats datatype */
    tid3 = H5Tarray_create2(H5T_NATIVE_FLOAT, ARRAY1_RANK, tdims1);

    /* Insert float array field */
    ret = H5Tinsert (tid2, "f", HOFFSET(s2_t,f), tid3);
    HDassert(ret >= 0);

    /* Close array of floats field datatype */
    ret = H5Tclose(tid3);
    HDassert(ret >= 0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);

    /* Close compound datatype */
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

static void gent_array6(void)
{
    hvl_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID   */
    hid_t  tid2;       /* VL Datatype ID       */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j,k;      /* counting variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].p = HDmalloc((size_t)(i + j + 1) * sizeof(unsigned int));
            wdata[i][j].len = (size_t)(i + j + 1);
            for(k = 0; k < (i + j + 1); k++)
                ((unsigned int *)wdata[i][j].p)[k] = (unsigned)(i * 100 + j * 10 + k);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE30, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create a compound datatype to refer to */
    tid2 = H5Tvlen_create(H5T_NATIVE_UINT);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);

    /* Close VL datatype */
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Dvlen_reclaim(tid1, sid1, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

static void gent_array7(void)
{
    hvl_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   /* Information to write */
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset; /* Dataset ID   */
    hid_t  sid1;       /* Dataspace ID   */
    hid_t  tid1;       /* Array Datatype ID   */
    hid_t  tid2;       /* VL Datatype ID       */
    hid_t  tid3;       /* Nested Array Datatype ID   */
    hsize_t  sdims1[] = {SPACE1_DIM1};
    hsize_t  tdims1[] = {ARRAY1_DIM1};
    int        i,j,k,l;    /* Index variables */
    herr_t  ret;  /* Generic return value  */

    /* Initialize array data to write */
    for(i=0; i<SPACE1_DIM1; i++)
        for(j=0; j<ARRAY1_DIM1; j++) {
            wdata[i][j].p = HDmalloc((size_t)(i + j + 1) * (sizeof(unsigned int) * ARRAY1_DIM1));
            wdata[i][j].len = (size_t)(i + j + 1);
            for(k = 0; k < (i + j + 1); k++)
                for(l = 0; l<ARRAY1_DIM1; l++)
                    ((unsigned int *)wdata[i][j].p)[k * ARRAY1_DIM1 + l] = (unsigned)(i * 1000+j * 100+k * 10 + l);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILE31, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);

    /* Create the nested array datatype to refer to */
    tid3 = H5Tarray_create2(H5T_NATIVE_UINT, ARRAY1_RANK, tdims1);

    /* Create a VL datatype of 1-D arrays to refer to */
    tid2 = H5Tvlen_create(tid3);

    /* Close nested array datatype */
    ret = H5Tclose(tid3);
    HDassert(ret >= 0);

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);

    /* Close VL datatype */
    ret = H5Tclose(tid2);
    HDassert(ret >= 0);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Reclaim the write VL data */
    ret = H5Dvlen_reclaim(tid1, sid1, H5P_DEFAULT, wdata);
    HDassert(ret >= 0);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    HDassert(ret >= 0);
    ret = H5Tclose(tid1);
    HDassert(ret >= 0);
    ret = H5Sclose(sid1);
    HDassert(ret >= 0);
    ret = H5Fclose(fid1);
    HDassert(ret >= 0);
}

/* Test the boundary of the display output buffer at the reallocation event */
static void gent_array8(void)
{
    hid_t       file = -1; /* Handles */
    hid_t       filetype = -1; /* Handles */
    hid_t       space = -1; /* Handles */
    hid_t       dset = -1; /* Handles */
    herr_t      status = -1;
    hsize_t sdims[] = {F64_DIM0};
    hsize_t tdims[] = {F64_DIM1};
    int         *wdata;      /* Write buffer */
    unsigned int i;

    /* Allocate data buffer */
    wdata = HDmalloc(F64_DIM1 * sizeof(int));
    HDassert(wdata);

    /*
     * Initialize data.  i is the element in the dataspace, j and k the
     * elements within the array datatype.
     */
    for (i = 0; i < F64_DIM1; i++)
        wdata[i] = (int)i;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (F64_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create array datatypes for file and memory.
     */
    filetype = H5Tarray_create2 (H5T_NATIVE_INT, 1, tdims);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, sdims, NULL);

    /*
     * Create the dataset and write the array data to it.
     */
    if(file>=0 && filetype>=0 && space>=0) {
        dset = H5Dcreate2 (file, F64_DATASET, filetype, space, H5P_DEFAULT, H5P_DEFAULT,
                H5P_DEFAULT);
        if(dset>=0)
            status = H5Dwrite (dset, filetype, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
            HDassert(status >= 0);
    }

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
    HDassert(status >= 0);
    status = H5Sclose (space);
    HDassert(status >= 0);
    status = H5Tclose (filetype);
    HDassert(status >= 0);
    status = H5Fclose (file);
    HDassert(status >= 0);
    HDfree(wdata);
}

static void gent_empty(void)
{
    typedef struct {
            int a;
            float b;
            char c;
    } empty_struct;
    hid_t file, dset, space, type;
    hsize_t dims[] = { SPACE1_DIM1 };
    herr_t ret=0;

    file = H5Fcreate(FILE32, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    space = H5Screate_simple(SPACE1_RANK, dims, NULL);

    /* write out an empty vlen dataset */
    type = H5Tvlen_create(H5T_NATIVE_INT);
    dset = H5Dcreate2(file, "Dataset1.0", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Tclose(type);
    HDassert(ret >= 0);

    /* write out an empty native integer dataset dataset */
    dset = H5Dcreate2(file, "Dataset2.0", H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    HDassert(ret >= 0);

    /* write out an empty native floating-point dataset dataset */
    dset = H5Dcreate2(file, "Dataset3.0", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    HDassert(ret >= 0);

    /* write out an empty array dataset */
    type = H5Tarray_create2(H5T_NATIVE_INT, SPACE1_RANK, dims);
    dset = H5Dcreate2(file, "Dataset4.0", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Tclose(type);
    HDassert(ret >= 0);

    /* write out an empty compound dataset */
    type = H5Tcreate(H5T_COMPOUND,sizeof(empty_struct));
    H5Tinsert(type, "a", HOFFSET(empty_struct, a),H5T_NATIVE_INT);
    H5Tinsert(type, "b", HOFFSET(empty_struct, b),H5T_NATIVE_FLOAT);
    H5Tinsert(type, "c", HOFFSET(empty_struct, c),H5T_NATIVE_CHAR);
    dset = H5Dcreate2(file, "Dataset5.0", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* Don't write any data */
    ret = H5Dclose(dset);
    HDassert(ret >= 0);
    ret = H5Tclose(type);
    HDassert(ret >= 0);

    ret = H5Sclose(space);
    HDassert(ret >= 0);

    ret = H5Fclose(file);
    HDassert(ret >= 0);
}

static void
gent_group_comments(void)
{
    hid_t fid, group;

    fid = H5Fcreate(FILE33, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* / */
    group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g1", "Comment for group /g1", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g2", "Comment for group /g2", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g3", "Comment for group /g3", H5P_DEFAULT);
    H5Gclose(group);

    /* /g1 */
    group = H5Gcreate2(fid, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g1/g1.1", "Comment for group /g1/g1.1", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g1/g1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g1/g1.2", "Comment for group /g1/g1.2", H5P_DEFAULT);
    H5Gclose(group);

    /* /g2 */
    group = H5Gcreate2(fid, "/g2/g2.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g2/g2.1", "Comment for group /g2/g2.1", H5P_DEFAULT);
    H5Gclose(group);

    /* /g3 */
    group = H5Gcreate2(fid, "/g3/g3.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g3/g3.1", "Comment for group /g3/g3.1", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3/g3.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g3/g3.2", "Comment for group /g3/g3.2", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3/g3.3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g3/g3.3", "Comment for group /g3/g3.3", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g3/g3.4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g3/g3.4", "Comment for group /g3/g3.4", H5P_DEFAULT);
    H5Gclose(group);

    /* /g2/g2.1 */
    group = H5Gcreate2(fid, "/g2/g2.1/g2.1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g2/g2.1/g2.1.1", "Comment for group /g2/g2.1/g2.1.1", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2/g2.1/g2.1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g2/g2.1/g2.1.2", "Comment for group /g2/g2.1/g2.1.2", H5P_DEFAULT);
    H5Gclose(group);
    group = H5Gcreate2(fid, "/g2/g2.1/g2.1.3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/g2/g2.1/g2.1.3", "Comment for group /g2/g2.1/g2.1.3", H5P_DEFAULT);
    H5Gclose(group);

    /* /glongcomment */
    group = H5Gcreate2(fid, "/glongcomment", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Oset_comment_by_name(group, "/glongcomment", "Comment for group /glongcomment with a really, really, really long, long, long comment", H5P_DEFAULT);
    H5Gclose(group);

    H5Fclose(fid);
}

static
void gent_split_file(void)
{
    hid_t fapl, fid, root, attr, space, dataset, atype;
    char meta[] = "this is some metadata on this file";
    hsize_t dims[2];
    int i, j, dset[10][15];

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_split(fapl, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);
    fid = H5Fcreate(FILE34, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    atype = H5Tcopy(H5T_C_S1);
    H5Tset_size(atype, HDstrlen(meta) + 1);
    H5Tset_strpad(atype, H5T_STR_NULLTERM);

    dims[0] = 1;
    space = H5Screate_simple(1, dims, NULL);
    attr = H5Acreate2(root, "Metadata", atype, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, atype, meta);
    H5Tclose(atype);
    H5Sclose(space);
    H5Aclose(attr);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 10; i++)
        for(j = 0; j < 15; j++)
            dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Gclose(root);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static
void gent_family(void)
{
    hid_t fapl, fid, space, dataset;
    hsize_t dims[2];
    int i, j, dset[10][15];

#define FAMILY_SIZE     256

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT);

    fid = H5Fcreate(FILE35, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 10; i++)
        for(j = 0; j < 15; j++)
            dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static const char *multi_letters = "msbrglo";

static
void gent_multi(void)
{
    hid_t fapl, fid, space, dataset;
    hsize_t dims[2];
    int i, j, dset[10][15];

    /* Multi-file driver, general case of the split driver */
    H5FD_mem_t mt, memb_map[H5FD_MEM_NTYPES];
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    const char *memb_name[H5FD_MEM_NTYPES];
    char sv[H5FD_MEM_NTYPES][1024];
    haddr_t memb_addr[H5FD_MEM_NTYPES];

    fapl = H5Pcreate(H5P_FILE_ACCESS);

    HDmemset(memb_map, 0, sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);

    HDassert(HDstrlen(multi_letters) == H5FD_MEM_NTYPES);

    for(mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt] = mt;
        sprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
        memb_name[mt] = sv[mt];
        /*printf("memb_name[%d]=%s, memb_map[%d]=%d; ", mt, memb_name[mt], mt, memb_map[mt]);*/
        memb_addr[mt] = (haddr_t)MAX(mt - 1, 0) * (HADDR_MAX / 10);
    }
    memb_map[H5FD_MEM_DEFAULT] = H5FD_MEM_SUPER;

    H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
            memb_addr, FALSE);

    fid = H5Fcreate(FILE36, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* create dataset */
    dims[0] = 10;
    dims[1] = 15;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, "/dset1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 10; i++)
        for(j = 0; j < 15; j++)
            dset[i][j] = i + j;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
    H5Pclose(fapl);
}

static void gent_large_objname(void)
{
    hid_t fid, group, group2;
    char grp_name[128];
    register int i;

    fid = H5Fcreate(FILE37, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate2(fid, "this_is_a_large_group_name", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < 50; ++i) {
        sprintf(grp_name, "this_is_a_large_group_name%d", i);
        group2 = H5Gcreate2(group, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Gclose(group2);
    }

    H5Gclose(group);
    H5Fclose(fid);
}

static void gent_vlstr(void)
{
    const char *wdata[SPACE1_DIM1]= {
            "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
            "conceived in liberty and dedicated to the proposition that all men are created equal.",
            "",
            NULL
    };   /* Information to write */
    const char *string_att= "This is the string for the attribute";
    hid_t  fid1;  /* HDF5 File IDs  */
    hid_t  dataset, root; /* Dataset ID   */
    hid_t  sid1, dataspace;/* Dataspace ID   */
    hid_t  tid1, att;      /* Datatype ID   */
    hsize_t  dims1[] = {SPACE1_DIM1};

    /* Create file */
    fid1 = H5Fcreate(FILE38, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a VL string datatype to refer to */
    tid1 = H5Tcopy (H5T_C_S1);
    H5Tset_size (tid1, H5T_VARIABLE);

    /* Create a dataset and write VL string to it. */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    H5Dclose(dataset);

    /* Create a named VL string type.  Change padding of datatype */
    H5Tset_strpad(tid1, H5T_STR_NULLPAD);
    H5Tcommit2(fid1, "vl_string_type", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Create an group attribute of VL string type */
    root = H5Gopen2(fid1, "/", H5P_DEFAULT);
    dataspace = H5Screate(H5S_SCALAR);

    att = H5Acreate2(root, "test_scalar", tid1, dataspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(att, tid1, &string_att);

    /* Close */
    H5Tclose(tid1);
    H5Sclose(sid1);
    H5Sclose(dataspace);
    H5Aclose(att);
    H5Gclose(root);
    H5Fclose(fid1);
}

static void gent_char(void)
{
    const char *wdata =
            "Four score and seven years ago our forefathers brought "
            "forth on this continent a new nation, conceived in "
            "liberty and dedicated to the proposition that all "
            "men are created equal. Now we are engaged in a great "
            "civil war, testing whether that nation or any nation "
            "so conceived and so dedicated can long endure.";
    hid_t       fid1;               /* HDF5 File IDs    */
    hid_t       dataset;            /* Dataset ID       */
    hid_t       sid1;               /* Dataspace ID     */
    hsize_t     dims1[1];

    dims1[0] = HDstrlen(wdata);

    /* Create file */
    fid1 = H5Fcreate(FILE39, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    sid1 = H5Screate_simple(1, dims1, NULL);

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", H5T_NATIVE_CHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write some characters to it. */
    H5Dwrite(dataset, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

    /* Close */
    H5Dclose(dataset);
    H5Sclose(sid1);
    H5Fclose(fid1);
}



/*-------------------------------------------------------------------------
 * Function: write_attr_in
 *
 * Purpose: write attributes in LOC_ID (dataset, group, named datatype)
 *
 * Return: void
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2003
 *
 *-------------------------------------------------------------------------
 */

static void write_attr_in(hid_t loc_id,
        const char* dset_name, /* for saving reference to dataset*/
        hid_t fid)
{
    /* Compound datatype */
    typedef struct s_t
    {
            char   a;
            double b;
    } s_t;

    typedef enum
    {
        E_RED,
        E_GREEN
    } e_t;

    hid_t   aid;
    hid_t   sid;
    hid_t   tid;
    herr_t  status;
    int     val, i, j, k, n;
    float   f;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]={2};
    char       buf1[2][3]= {"ab","de"};        /* string */
    char       buf2[2]= {1,2};                 /* bitfield, opaque */
    s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
    hobj_ref_t buf4[2];                        /* reference */
    hvl_t      buf5[2];                        /* vlen */
    hsize_t    dimarray[1]={3};                /* array dimension */
    int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
    int        buf7[2]= {1,2};                 /* integer */
    float      buf8[2]= {1,2};                 /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t    dims2[2]={3,2};
    char       buf12[6][3]= {"ab","cd","ef","gh","ij","kl"};         /* string */
    char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
    s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
    hobj_ref_t buf42[3][2];                                          /* reference */
    hvl_t      buf52[3][2];                                          /* vlen */
    int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
    int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
    float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]={4,3,2};
    char       buf13[24][3]= {"ab","cd","ef","gh","ij","kl","mn","pq",
            "rs","tu","vw","xz","AB","CD","EF","GH",
            "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
    char       buf23[4][3][2];    /* bitfield, opaque */
    s_t        buf33[4][3][2];    /* compound */
    hobj_ref_t buf43[4][3][2];    /* reference */
    hvl_t      buf53[4][3][2];    /* vlen */
    int        buf63[24][3];      /* array */
    int        buf73[4][3][2];    /* integer */
    float      buf83[4][3][2];    /* float */


    /*-------------------------------------------------------------------------
     * 1D attributes
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(tid, 2);
    write_attr(loc_id,1,dims,"string",tid,buf1);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id,1,dims,"bitfield",tid,buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id,1,dims,"opaque",tid,buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id,1,dims,"compound",tid,buf3);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if(dset_name)
    {
        status=H5Rcreate(&buf4[0],fid,dset_name,H5R_OBJECT,(hid_t)-1);
        status=H5Rcreate(&buf4[1],fid,dset_name,H5R_OBJECT,(hid_t)-1);
        write_attr(loc_id,1,dims,"reference",H5T_STD_REF_OBJ,buf4);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED",   (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id,1,dims,"enum",tid,0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len = 1;
    buf5[0].p = HDmalloc(1 * sizeof(int));
    ((int *)buf5[0].p)[0] = 1;
    buf5[1].len = 2;
    buf5[1].p = HDmalloc(2 * sizeof(int));
    ((int *)buf5[1].p)[0] = 2;
    ((int *)buf5[1].p)[1] = 3;

    sid = H5Screate_simple(1, dims, NULL);
    tid = H5Tvlen_create(H5T_NATIVE_INT);
    aid = H5Acreate2(loc_id, "vlen", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf5);
    HDassert(status >= 0);
    status = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf5);
    HDassert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 1, dims, "array", tid, buf6);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
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
    tid = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(tid, 2);
    write_attr(loc_id,2,dims2,"string2D",tid,buf12);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id,2,dims2,"bitfield2D",tid,buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id,2,dims2,"opaque2D",tid,buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id,2,dims2,"compound2D",tid,buf32);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if(dset_name)
    {
        for(i = 0; i < 3; i++) {
            for(j = 0; j < 2; j++) {
                status=H5Rcreate(&buf42[i][j],fid,dset_name,H5R_OBJECT,(hid_t)-1);
            }
        }
        write_attr(loc_id,2,dims2,"reference2D",H5T_STD_REF_OBJ,buf42);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED",   (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id,2,dims2,"enum2D",tid,0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n=0;
    for(i = 0; i < 3; i++) {
        for(j = 0; j < 2; j++) {
            int l;

            buf52[i][j].p = HDmalloc((size_t)(i + 1) * sizeof(int));
            buf52[i][j].len = (size_t)(i + 1);
            for(l = 0; l < i + 1; l++)
                ((int *)buf52[i][j].p)[l] = n++;
        }
    }

    sid = H5Screate_simple(2, dims2, NULL);
    tid = H5Tvlen_create(H5T_NATIVE_INT);
    aid = H5Acreate2(loc_id, "vlen2D", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf52);
    HDassert(status >= 0);
    status = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf52);
    HDassert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 2, dims2, "array2D", tid, buf62);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
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
    tid = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(tid, 2);
    write_attr(loc_id,3,dims3,"string3D",tid,buf13);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    n=1;
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 3; j++) {
            for(k = 0; k < 2; k++) {
                buf23[i][j][k] = (char)n++;
            }
        }
    }
    tid = H5Tcopy(H5T_STD_B8LE);
    write_attr(loc_id,3,dims3,"bitfield3D",tid,buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_attr(loc_id,3,dims3,"opaque3D",tid,buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    n=1;
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 3; j++) {
            for(k = 0; k < 2; k++) {
                buf33[i][j][k].a = (char)n++;
                buf33[i][j][k].b=n++;
            }
        }
    }
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_attr(loc_id,3,dims3,"compound3D",tid,buf33);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if(dset_name)
    {
        for(i = 0; i < 4; i++) {
            for(j = 0; j < 3; j++) {
                for(k = 0; k < 2; k++)
                    status=H5Rcreate(&buf43[i][j][k],fid,dset_name,H5R_OBJECT,(hid_t)-1);
            }
        }
        write_attr(loc_id,3,dims3,"reference3D",H5T_STD_REF_OBJ,buf43);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED",   (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_attr(loc_id,3,dims3,"enum3D",tid,0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 3; j++) {
            for(k = 0; k < 2; k++) {
                int l;

                buf53[i][j][k].p = HDmalloc((size_t)(i + 1) * sizeof(int));
                buf53[i][j][k].len = (size_t)(i + 1);
                for(l = 0; l < i + 1; l++)
                    ((int *)buf53[i][j][k].p)[l] = n++;
            }
        }
    }

    sid = H5Screate_simple(3, dims3, NULL);
    tid = H5Tvlen_create(H5T_NATIVE_INT);
    aid = H5Acreate2(loc_id, "vlen3D", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Awrite(aid, tid, buf53);
    HDassert(status >= 0);
    status = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf53);
    HDassert(status >= 0);
    status = H5Aclose(aid);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    n = 1;
    for(i = 0; i < 24; i++)
        for(j = 0; j < (int)dimarray[0]; j++)
            buf63[i][j] = n++;

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_attr(loc_id, 3, dims3, "array3D", tid, buf63);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    n=1; f=1;
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 3; j++) {
            for(k = 0; k < 2; k++) {
                buf73[i][j][k]=n++;
                buf83[i][j][k]=f++;
            }
        }
    }
    write_attr(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
    write_attr(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}



/*-------------------------------------------------------------------------
 * Function: write_dset_in
 *
 * Purpose: write datasets in LOC_ID
 *
 * Return: void
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: May 28, 2003
 *
 *-------------------------------------------------------------------------
 */

static void write_dset_in(hid_t loc_id,
        const char* dset_name, /* for saving reference to dataset*/
        hid_t fid)
{
    /* Compound datatype */
    typedef struct s_t
    {
            char   a;
            double b;
    } s_t;

    typedef enum
    {
        E_RED,
        E_GREEN
    } e_t;

    hid_t   did;
    hid_t   sid;
    hid_t   tid;
    hid_t   plist_id;
    herr_t  status;
    int     val, i, j, k, n;
    float   f;
    int     fillvalue=2;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]={2};
    char       buf1[2][3]= {"ab","de"};        /* string */
    char       buf2[2]= {1,2};                 /* bitfield, opaque */
    s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
    hobj_ref_t buf4[2];                        /* reference */
    hvl_t      buf5[2];                        /* vlen */
    hsize_t    dimarray[1]={3};                /* array dimension */
    int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
    int        buf7[2]= {1,2};                 /* integer */
    float      buf8[2]= {1,2};                 /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t    dims2[2]={3,2};
    char       buf12[6][3]= {"ab","cd","ef","gh","ij","kl"};         /* string */
    char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
    s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
    hobj_ref_t buf42[3][2];                                          /* reference */
    hvl_t      buf52[3][2];                                          /* vlen */
    int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
    int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
    float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]={4,3,2};
    char       buf13[24][3]= {"ab","cd","ef","gh","ij","kl","mn","pq",
            "rs","tu","vw","xz","AB","CD","EF","GH",
            "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
    char       buf23[4][3][2];    /* bitfield, opaque */
    s_t        buf33[4][3][2];    /* compound */
    hobj_ref_t buf43[4][3][2];    /* reference */
    hvl_t      buf53[4][3][2];    /* vlen */
    int        buf63[24][3];      /* array */
    int        buf73[4][3][2];    /* integer */
    float      buf83[4][3][2];    /* float */


    /*-------------------------------------------------------------------------
     * 1D
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(tid, 2);
    write_dset(loc_id,1,dims,"string",tid,buf1);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id,1,dims,"bitfield",tid,buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_dset(loc_id,1,dims,"opaque",tid,buf2);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id,1,dims,"compound",tid,buf3);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if(dset_name)
    {
        status=H5Rcreate(&buf4[0],fid,dset_name,H5R_OBJECT,(hid_t)-1);
        status=H5Rcreate(&buf4[1],fid,dset_name,H5R_OBJECT,(hid_t)-1);
        write_dset(loc_id,1,dims,"reference",H5T_STD_REF_OBJ,buf4);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED",   (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_dset(loc_id,1,dims,"enum",tid,0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len = 1;
    buf5[0].p = HDmalloc( 1 * sizeof(int));
    ((int *)buf5[0].p)[0]=1;
    buf5[1].len = 2;
    buf5[1].p = HDmalloc( 2 * sizeof(int));
    ((int *)buf5[1].p)[0]=2;
    ((int *)buf5[1].p)[1]=3;

    sid = H5Screate_simple(1, dims, NULL);
    tid = H5Tvlen_create(H5T_NATIVE_INT);
    did = H5Dcreate2(loc_id, "vlen", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf5);
    HDassert(status >= 0);
    status = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf5);
    HDassert(status >= 0);
    status = H5Dclose(did);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(loc_id, 1, dims, "array", tid, buf6);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    write_dset(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
    write_dset(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


    /*-------------------------------------------------------------------------
     * 2D
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(tid, 2);
    write_dset(loc_id,2,dims2,"string2D",tid,buf12);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id,2,dims2,"bitfield2D",tid,buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_dset(loc_id,2,dims2,"opaque2D",tid,buf22);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id,2,dims2,"compound2D",tid,buf32);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if(dset_name)
    {
        for(i = 0; i < 3; i++) {
            for(j = 0; j < 2; j++) {
                status=H5Rcreate(&buf42[i][j],fid,dset_name,H5R_OBJECT,(hid_t)-1);
            }
        }
        write_dset(loc_id,2,dims2,"reference2D",H5T_STD_REF_OBJ,buf42);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED",   (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_dset(loc_id,2,dims2,"enum2D",tid,0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n=0;
    for(i = 0; i < 3; i++)
        for(j = 0; j < 2; j++) {
            int l;

            buf52[i][j].p = HDmalloc((size_t)(i + 1) * sizeof(int));
            buf52[i][j].len = (size_t)(i + 1);
            for(l = 0; l < i + 1; l++)
                ((int *)buf52[i][j].p)[l] = n++;
        }

    sid = H5Screate_simple(2, dims2, NULL);
    tid = H5Tvlen_create(H5T_NATIVE_INT);
    did = H5Dcreate2(loc_id, "vlen2D", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf52);
    HDassert(status >= 0);
    status = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf52);
    HDassert(status >= 0);
    status = H5Dclose(did);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(loc_id, 2, dims2, "array2D", tid, buf62);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER, write a fill value
     *-------------------------------------------------------------------------
     */
    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_fill_value(plist_id, H5T_NATIVE_INT, &fillvalue);
    sid = H5Screate_simple(2, dims2, NULL);
    did = H5Dcreate2(loc_id, "integer2D", H5T_NATIVE_INT, sid, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf72);
    status = H5Pclose(plist_id);
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
    tid = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(tid, 2);
    write_dset(loc_id,3,dims3,"string3D",tid,buf13);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */

    n=1;
    for(i = 0; i < 4; i++)
        for(j = 0; j < 3; j++)
            for(k = 0; k < 2; k++)
                buf23[i][j][k] = (char)n++;
    tid = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id,3,dims3,"bitfield3D",tid,buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(tid, "1-byte opaque type"); /* must set this */
    write_dset(loc_id,3,dims3,"opaque3D",tid,buf23);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_COMPOUND
     *-------------------------------------------------------------------------
     */

    n=1;
    for(i = 0; i < 4; i++)
        for(j = 0; j < 3; j++)
            for(k = 0; k < 2; k++) {
                buf33[i][j][k].a = (char)n++;
                buf33[i][j][k].b = n++;
            }
    tid = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(tid, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id,3,dims3,"compound3D",tid,buf33);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    /* Create references to dataset */
    if(dset_name)
    {
        for(i = 0; i < 4; i++) {
            for(j = 0; j < 3; j++) {
                for(k = 0; k < 2; k++)
                    status=H5Rcreate(&buf43[i][j][k],fid,dset_name,H5R_OBJECT,(hid_t)-1);
            }
        }
        write_dset(loc_id,3,dims3,"reference3D",H5T_STD_REF_OBJ,buf43);
    }

    /*-------------------------------------------------------------------------
     * H5T_ENUM
     *-------------------------------------------------------------------------
     */
    tid = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(tid, "RED",   (val = 0, &val));
    H5Tenum_insert(tid, "GREEN", (val = 1, &val));
    write_dset(loc_id,3,dims3,"enum3D",tid,0);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */

    /* Allocate and initialize VL dataset to write */
    n = 0;
    for(i = 0; i < 4; i++)
        for(j = 0; j < 3; j++)
            for(k = 0; k < 2; k++) {
                int l;

                buf53[i][j][k].p = HDmalloc(((size_t)i + 1) * sizeof(int));
                buf53[i][j][k].len = (size_t)(i + 1);
                for(l = 0; l < i + 1; l++)
                    ((int *)buf53[i][j][k].p)[l] = n++;
            }

    sid = H5Screate_simple(3, dims3, NULL);
    tid = H5Tvlen_create(H5T_NATIVE_INT);
    did = H5Dcreate2(loc_id, "vlen3D", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf53);
    HDassert(status >= 0);
    status = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf53);
    HDassert(status >= 0);
    status = H5Dclose(did);
    status = H5Tclose(tid);
    status = H5Sclose(sid);

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    n=1;
    for(i = 0; i < 24; i++) {
        for(j = 0; j < (int)dimarray[0]; j++) {
            buf63[i][j]=n++;
        }
    }

    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(loc_id, 3, dims3, "array3D", tid, buf63);
    status = H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * H5T_INTEGER and H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    n=1; f=1;
    for(i = 0; i < 4; i++) {
        for(j = 0; j < 3; j++) {
            for(k = 0; k < 2; k++) {
                buf73[i][j][k]=n++;
                buf83[i][j][k]=f++;
            }
        }
    }
    write_dset(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
    write_dset(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}




/*-------------------------------------------------------------------------
 * Function: gent_attr_all
 *
 * Purpose: generate all datatype attributes
 *
 * Return: void
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: May 19, 2003
 *
 *-------------------------------------------------------------------------
 */

static void gent_attr_all(void)
{
    hid_t   fid;
    hid_t   did;
    hid_t   group_id;
    hid_t   group2_id;
    hid_t   root_id;
    hid_t   sid;
    hsize_t dims[1] = {2};
    herr_t  status;

    /* Create a file and a dataset */
    fid = H5Fcreate(FILE40, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create a 1D dataset */
    sid = H5Screate_simple(1,dims,NULL);
    did = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Sclose(sid);
    HDassert(status >= 0);

    /* Create groups */
    group_id  = H5Gcreate2(fid, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    group2_id = H5Gcreate2(fid, "g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    root_id   = H5Gopen2(fid, "/", H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * write a series of attributes on the dataset, group
     *-------------------------------------------------------------------------
     */

    write_attr_in(did,"dset",fid);
    write_attr_in(group_id,NULL,(hid_t)0);
    write_attr_in(root_id,NULL,(hid_t)0);

    /*-------------------------------------------------------------------------
     * write a series of datasets on group 2
     *-------------------------------------------------------------------------
     */

    write_dset_in(group2_id,"/dset",fid);

    /* Close */
    status = H5Dclose(did);
    HDassert(status >= 0);
    status = H5Gclose(group_id);
    HDassert(status >= 0);
    status = H5Gclose(group2_id);
    HDassert(status >= 0);
    status = H5Gclose(root_id);
    HDassert(status >= 0);

    /* Close file */
    status = H5Fclose(fid);
    HDassert(status >= 0);
}


/*-------------------------------------------------------------------------
 * Function: write_attr
 *
 * Purpose: utility function to write an attribute
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: May 19, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int write_attr(hid_t loc_id, int rank, hsize_t *dims, const char *attr_name,
        hid_t tid, void *buf)
{
    hid_t   aid;
    hid_t   sid;
    herr_t  status;

    /* Create a buf space  */
    sid = H5Screate_simple(rank, dims, NULL);

    /* Create the attribute */
    aid = H5Acreate2(loc_id, attr_name, tid, sid, H5P_DEFAULT, H5P_DEFAULT);

    /* Write the buf */
    if(buf)
        status = H5Awrite(aid, tid, buf);

    /* Close */
    status = H5Aclose(aid);
    status = H5Sclose(sid);

    return status;
}

/*-------------------------------------------------------------------------
 * Function: write_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 * Return:
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: May 27, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int write_dset( hid_t loc_id, int rank, hsize_t *dims, const char *dset_name,
        hid_t tid, void *buf )
{
    hid_t   did;
    hid_t   sid;
    herr_t  status;

    /* Create a buf space  */
    sid = H5Screate_simple(rank, dims, NULL);

    /* Create a dataset */
    did = H5Dcreate2(loc_id, dset_name, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write the buf */
    if(buf)
        status = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    /* Close */
    status = H5Dclose(did);
    status = H5Sclose(sid);

    return status;
}


static void gent_compound_complex(void)
{
    /* Structure and array for compound types                             */
    typedef struct Array1Struct {
            int                a;
            const char         *b[F41_DIMb];
            char               c[F41_ARRAY_DIMc];
            short              d[F41_ARRAY_DIMd1][F41_ARRAY_DIMd2];
            float              e;
            double             f[F41_ARRAY_DIMf];
            char               g;
    } Array1Struct;
    Array1Struct       Array1[F41_LENGTH];

    /* Define the value of the string array                           */
    const char *quote [F41_DIMb] = {
            "A fight is a contract that takes two people to honor.",
            "A combative stance means that you've accepted the contract.",
            "In which case, you deserve what you get.",
            "  --  Professor Cheng Man-ch'ing"
    };

    /* Define the value of the character array                        */
    char chararray [F41_ARRAY_DIMc] = {'H', 'e', 'l', 'l', 'o', '!'};


    hid_t      Array1Structid;            /* File datatype identifier */
    hid_t      array_tid;                 /* Array datatype handle    */
    hid_t      array1_tid;                /* Array datatype handle    */
    hid_t      array2_tid;                /* Array datatype handle    */
    hid_t      array4_tid;                /* Array datatype handle    */
    hid_t      datafile, dataset;         /* Datafile/dataset handles */
    hid_t      dataspace;                 /* Dataspace handle         */
    herr_t     status;                    /* Error checking variable */
    hsize_t    dim[] = {F41_LENGTH};          /* Dataspace dimensions     */
    hsize_t    array_dimb[] = {F41_DIMb};     /* Array dimensions         */
    hsize_t    array_dimd[]={F41_ARRAY_DIMd1,F41_ARRAY_DIMd2}; /* Array dimensions         */
    hsize_t    array_dimf[]={F41_ARRAY_DIMf}; /* Array dimensions         */
    hid_t      str_array_id;

    int        m, n, o;                   /* Array init loop vars     */

    /* Initialize the data in the arrays/datastructure                */
    for(m = 0; m< F41_LENGTH; m++) {
        Array1[m].a = m;

        for(n = 0; n < F41_DIMb; n++)
            Array1[m].b[n] = quote[n];

        for(n = 0; n < F41_ARRAY_DIMc; n++)
            Array1[m].c[n] = chararray[n];

        for(n = 0; n < F41_ARRAY_DIMd1; n++)
            for(o = 0; o < F41_ARRAY_DIMd2; o++)
                Array1[m].d[n][o] = (short)(m + n + o);

        Array1[m].e = (float)((float)m * 0.96F );

        for(n = 0; n < F41_ARRAY_DIMf; n++)
            Array1[m].f[n] = ((float)m * 1024.9637F );

        Array1[m].g = 'm';
    }

    /* Create the dataspace                                           */
    dataspace = H5Screate_simple(F41_RANK, dim, NULL);
    HDassert(dataspace >= 0);

    /* Create the file                                                */
    datafile = H5Fcreate(FILE41, H5F_ACC_TRUNC, H5P_DEFAULT,
            H5P_DEFAULT);
    HDassert(datafile >= 0);

    /* Copy the array data type for the string array                  */
    array_tid = H5Tcopy (H5T_C_S1);
    HDassert(array_tid >= 0);

    /* Set the string array size to Variable                          */
    status = H5Tset_size (array_tid,H5T_VARIABLE);
    HDassert(status >= 0);

    /* Create the array data type for the string array                */
    str_array_id = H5Tarray_create2(array_tid, F41_ARRAY_RANK, array_dimb);
    HDassert(str_array_id >= 0);

    /* Copy the array data type for the character array               */
    array1_tid = H5Tcopy (H5T_C_S1);
    HDassert(array1_tid >= 0);

    /* Set the character array size                                   */
    status = H5Tset_size (array1_tid, F41_ARRAY_DIMc);
    HDassert(status >= 0);

    /* Create the array data type for the character array             */
    array2_tid = H5Tarray_create2(H5T_NATIVE_SHORT, F41_ARRAY_RANKd, array_dimd);
    HDassert(array2_tid >= 0);

    /* Create the array data type for the character array             */
    array4_tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F41_ARRAY_RANK, array_dimf);
    HDassert(array4_tid >= 0);

    /* Create the memory data type                                    */
    Array1Structid = H5Tcreate (H5T_COMPOUND, sizeof(Array1Struct));
    HDassert(Array1Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Array1Structid, "a_name",
            HOFFSET(Array1Struct, a), H5T_NATIVE_INT);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, "b_name",
            HOFFSET(Array1Struct, b), str_array_id);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, "c_name",
            HOFFSET(Array1Struct, c), array1_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, "d_name",
            HOFFSET(Array1Struct, d), array2_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, "e_name",
            HOFFSET(Array1Struct, e), H5T_NATIVE_FLOAT);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, "f_name",
            HOFFSET(Array1Struct, f), array4_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, "g_name",
            HOFFSET(Array1Struct, g), H5T_NATIVE_CHAR);
    HDassert(status >= 0);

    /* Create the dataset                                             */
    dataset = H5Dcreate2(datafile, F41_DATASETNAME, Array1Structid,
            dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the dataset                                      */
    status = H5Dwrite(dataset, Array1Structid, H5S_ALL, H5S_ALL,
            H5P_DEFAULT, Array1);
    HDassert(status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Array1Structid);
    HDassert(status >= 0);

    status = H5Tclose(array_tid);
    HDassert(status >= 0);

    status = H5Tclose(array1_tid);
    HDassert(status >= 0);

    status = H5Tclose(array2_tid);
    HDassert(status >= 0);

    status = H5Tclose(array4_tid);
    HDassert(status >= 0);

    status = H5Tclose(str_array_id);
    HDassert(status >= 0);

    status = H5Sclose(dataspace);
    HDassert(status >= 0);

    status = H5Dclose(dataset);
    HDassert(status >= 0);

    status = H5Fclose(datafile);
    HDassert(status >= 0);
}


static void gent_named_dtype_attr(void)
{
    hid_t fid;
    hid_t did;
    hid_t sid;
    hid_t tid;
    hid_t aid;
    hid_t gid;
    int data=8;
    herr_t ret;

    /* Create a file */
    fid=H5Fcreate(FILE42, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>0);

    /*-------------------------------------------------------------------------
     * datatype
     *-------------------------------------------------------------------------
     */

    /* Create a datatype to commit and use */
    tid=H5Tcopy(H5T_NATIVE_INT);
    HDassert(tid>0);

    /* Commit datatype to file */
    ret = H5Tcommit2(fid, F42_TYPENAME, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    /* Create a hard link to the datatype */
    ret = H5Lcreate_hard(fid, F42_TYPENAME, fid, F42_LINKNAME, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    /* Create a scalar dataspace used for all objects */
    sid = H5Screate(H5S_SCALAR);
    HDassert(sid > 0);

    /* Create attribute on commited datatype */
    aid = H5Acreate2(tid, F42_ATTRNAME, H5T_STD_I32LE, sid, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(aid > 0);

    /* Write data into the attribute */
    ret = H5Awrite(aid, H5T_NATIVE_INT, &data);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * dataset
     *-------------------------------------------------------------------------
     */

    /* Create dataset */
    did = H5Dcreate2(fid, F42_DSETNAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(did > 0);

    /* Create attribute on dataset */
    aid = H5Acreate2(did, F42_ATTRNAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(aid > 0);

    /* Write data into the attribute */
    ret = H5Awrite(aid, H5T_NATIVE_INT, &data);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * group
     *-------------------------------------------------------------------------
     */

    /* Create a group */
    gid = H5Gcreate2(fid, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(gid > 0);

    /* Create attribute on group */
    aid = H5Acreate2(gid, F42_ATTRNAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(aid > 0);

    /* Write data into the attribute */
    ret = H5Awrite(aid, H5T_NATIVE_INT, &data);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */

    /* Close attribute */
    ret = H5Aclose(aid);
    HDassert(ret >= 0);

    /* Close dataset */
    ret = H5Dclose(did);
    HDassert(ret >= 0);

    /* Close dataspace */
    ret = H5Sclose(sid);
    HDassert(ret >= 0);

    /* Close datatype */
    ret = H5Tclose(tid);
    HDassert(ret >= 0);

    /* Close file */
    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}


/*-------------------------------------------------------------------------
 * Function: gent_null_space
 *
 * Purpose: generates dataset and attribute of null dataspace
 *-------------------------------------------------------------------------
 */
static void gent_null_space(void)
{
    hid_t fid, root, dataset, space, attr;
    int dset_buf = 10;
    int point = 4;

    fid = H5Fcreate(FILE45, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* null space */
    space = H5Screate(H5S_NULL);

    /* dataset */
    dataset = H5Dcreate2(fid, "dset", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    /* nothing should be written */
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &dset_buf);

    /* attribute */
    attr = H5Acreate2(root, "attr", H5T_NATIVE_UINT, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT, &point); /* Nothing can be written */

    H5Dclose(dataset);
    H5Aclose(attr);
    H5Gclose(root);
    H5Sclose(space);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function: gent_zero_dim_size
 *
 * Purpose: generates dataset and attribute with dataspace of 0 dimension size
 *-------------------------------------------------------------------------
 */
static void gent_zero_dim_size(void)
{
    hid_t fid, root, dataset, space, attr;
    hsize_t  dims1[] = {SPACE3_DIM1, SPACE3_DIM2};
    int dset_buf = 10;
    int point = 4;

    fid = H5Fcreate(FILE67, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* dataspace of 0 dimension size */
    space = H5Screate_simple(SPACE3_RANK, dims1, NULL);

    /* dataset */
    dataset = H5Dcreate2(fid, "dset of 0 dimension size", H5T_STD_I32BE, space, H5P_DEFAULT,
            H5P_DEFAULT, H5P_DEFAULT);
    /* nothing should be written */
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &dset_buf);

    /* attribute */
    attr = H5Acreate2(root, "attr of 0 dimension size", H5T_NATIVE_UINT, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT, &point); /* Nothing can be written */

    H5Dclose(dataset);
    H5Aclose(attr);
    H5Gclose(root);
    H5Sclose(space);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function: make_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 *-------------------------------------------------------------------------
 */
static
int make_dset(hid_t loc_id,
        const char *name,
        hid_t sid,
        hid_t tid,
        hid_t dcpl,
        void *buf)
{
    hid_t   dsid;

    /* create the dataset */
    if((dsid = H5Dcreate2(loc_id, name, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        return -1;

    /* write */
    if(H5Dwrite(dsid, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto out;

    /* close */
    if(H5Dclose(dsid) < 0)
        return -1;

    return 0;

    out:
    H5E_BEGIN_TRY {
        H5Dclose(dsid);
    } H5E_END_TRY;
    return -1;
}



/*-------------------------------------------------------------------------
 * Function: make_external
 *
 * Purpose: make a dataset with external storage
 *
 *-------------------------------------------------------------------------
 */
static void
make_external(hid_t fid)
{
    hid_t   dcpl;         /*dataset creation property list */
    hid_t   sid;          /*dataspace ID */
    hid_t   dsid;         /*dataset ID   */
    hsize_t cur_size[1];  /*data space current size */
    hsize_t max_size[1];  /*data space maximum size */
    hsize_t size;         /*bytes reserved for data in the external file*/
    int     ret;

    cur_size[0] = max_size[0] = 100;
    size = (max_size[0]*sizeof(int)/2);

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    ret = H5Pset_external(dcpl, "ext1.bin", (off_t)0, size);
    HDassert(ret >= 0);

    ret = H5Pset_external(dcpl, "ext2.bin", (off_t)0, size);
    HDassert(ret >= 0);

    sid = H5Screate_simple(1, cur_size, max_size);
    HDassert(ret >= 0);

    dsid = H5Dcreate2(fid, "external", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    HDassert(ret >= 0);

    H5Dclose(dsid);
    HDassert(ret >= 0);

    H5Sclose(sid);
    HDassert(ret >= 0);

    H5Pclose(dcpl);
    HDassert(ret >= 0);
}

/*-------------------------------------------------------------------------
 * Function: gent_filters
 *
 * Purpose: make several datasets with filters, external dataset
 *
 *-------------------------------------------------------------------------
 */
static void gent_filters(void)
{
    hid_t    fid;  /* file id */
    hid_t    dcpl; /* dataset creation property list */
    hid_t    sid;  /* dataspace ID */
    hid_t    tid;  /* datatype ID */
#ifdef H5_HAVE_FILTER_SZIP
    unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block=4;
#endif /* H5_HAVE_FILTER_SZIP */
    hsize_t  dims1[RANK]      = {DIM1,DIM2};
    hsize_t  chunk_dims[RANK] = {CDIM1,CDIM2};
    int      buf1[DIM1][DIM2];
    int      i, j, n, ret;

    for(i=n=0; i<DIM1; i++){
        for(j=0; j<DIM2; j++){
            buf1[i][j]=n++;
        }
    }

    /* create a file */
    fid  = H5Fcreate(FILE44, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);

    /* create a space */
    sid = H5Screate_simple(SPACE2_RANK, dims1, NULL);

    /* create a dataset creation property list; the same DCPL is used for all dsets */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /*-------------------------------------------------------------------------
     * create a compact and contiguous storage layout dataset
     * add a comment to the datasets
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_layout(dcpl, H5D_COMPACT);
    HDassert(ret >= 0);

    ret=make_dset(fid,"compact",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    ret = H5Oset_comment_by_name(fid, "compact", "This is a dataset with compact storage", H5P_DEFAULT);
    HDassert(ret >= 0);

    ret = H5Pset_layout(dcpl, H5D_CONTIGUOUS);
    HDassert(ret >= 0);

    ret=make_dset(fid,"contiguous",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    ret = H5Oset_comment_by_name(fid, "contiguous", "This is a dataset with contiguous storage", H5P_DEFAULT);
    HDassert(ret >= 0);

    ret = H5Pset_layout(dcpl, H5D_CHUNKED);
    HDassert(ret >= 0);

    ret = H5Pset_chunk(dcpl, SPACE2_RANK, chunk_dims);
    HDassert(ret >= 0);

    ret=make_dset(fid,"chunked",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    ret = H5Oset_comment_by_name(fid, "chunked", "This is a dataset with chunked storage", H5P_DEFAULT);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * make several dataset with filters
     *-------------------------------------------------------------------------
     */

    /* set up chunk */
    ret = H5Pset_chunk(dcpl, SPACE2_RANK, chunk_dims);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * SZIP
     *-------------------------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SZIP
    if(h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        /* remove the filters from the dcpl */
        ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
        HDassert(ret >= 0);

        /* set szip data */
        ret = H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block);
        HDassert(ret >= 0);

        ret=make_dset(fid,"szip",sid,H5T_NATIVE_INT,dcpl,buf1);
        HDassert(ret >= 0);
    }
#endif /* H5_HAVE_FILTER_SZIP */

    /*-------------------------------------------------------------------------
     * GZIP
     *-------------------------------------------------------------------------
     */
#if defined (H5_HAVE_FILTER_DEFLATE)
    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /* set deflate data */
    ret = H5Pset_deflate(dcpl, 9);
    HDassert(ret >= 0);

    ret=make_dset(fid,"deflate",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);
#endif


    /*-------------------------------------------------------------------------
     * shuffle
     *-------------------------------------------------------------------------
     */

    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /* set the shuffle filter */
    ret = H5Pset_shuffle(dcpl);
    HDassert(ret >= 0);

    ret=make_dset(fid,"shuffle",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);


    /*-------------------------------------------------------------------------
     * checksum
     *-------------------------------------------------------------------------
     */

    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /* set the checksum filter */
    ret = H5Pset_fletcher32(dcpl);
    HDassert(ret >= 0);

    ret=make_dset(fid,"fletcher32",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * nbit
     *-------------------------------------------------------------------------
     */

    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /* set the checksum filter */
    ret = H5Pset_nbit(dcpl);
    HDassert(ret >= 0);

    tid=H5Tcopy(H5T_NATIVE_INT);
    H5Tset_precision(tid,H5Tget_size(tid)-1);
    ret=make_dset(fid,"nbit",sid,tid,dcpl,buf1);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * scaleoffset
     *-------------------------------------------------------------------------
     */

    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /* set the scaleoffset filter */
    ret = H5Pset_scaleoffset(dcpl,H5Z_SO_INT,(int)H5Tget_size(H5T_NATIVE_INT));
    HDassert(ret >= 0);

    ret=make_dset(fid,"scaleoffset",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * all filters
     *-------------------------------------------------------------------------
     */
    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /* set the shuffle filter */
    ret = H5Pset_shuffle(dcpl);
    HDassert(ret >= 0);

#ifdef H5_HAVE_FILTER_SZIP
    if(h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        szip_options_mask=H5_SZIP_CHIP_OPTION_MASK | H5_SZIP_EC_OPTION_MASK;
        /* set szip data */
        ret = H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block);
        HDassert(ret >= 0);
    }
#endif /* H5_HAVE_FILTER_SZIP */

#if defined (H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    ret = H5Pset_deflate(dcpl, 5);
    HDassert(ret >= 0);
#endif

    /* set the checksum filter */
    ret = H5Pset_fletcher32(dcpl);
    HDassert(ret >= 0);

    /* set the nbit filter */
    ret = H5Pset_nbit(dcpl);
    HDassert(ret >= 0);

    ret=make_dset(fid,"all",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);


    /*-------------------------------------------------------------------------
     * user defined filter
     *-------------------------------------------------------------------------
     */
    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    ret = H5Zregister (H5Z_MYFILTER);
    HDassert(ret >= 0);

    ret = H5Pset_filter (dcpl, MYFILTER_ID, 0, 0, NULL);
    HDassert(ret >= 0);

    ret=make_dset(fid,"myfilter",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl,H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * make an external dataset
     *-------------------------------------------------------------------------
     */
    make_external(fid);

    /*-------------------------------------------------------------------------
     * H5D_ALLOC_TIME_EARLY
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY);
    HDassert(ret >= 0);
    ret=make_dset(fid,"alloc_time_early",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * H5D_ALLOC_TIME_INCR
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR);
    HDassert(ret >= 0);
    ret=make_dset(fid,"alloc_time_incr",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * H5D_ALLOC_TIME_LATE
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    HDassert(ret >= 0);
    ret=make_dset(fid,"alloc_time_late",sid,H5T_NATIVE_INT,dcpl,buf1);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * commit a H5G_TYPE type with a comment
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_STD_B8LE);
    ret = H5Tcommit2(fid, "mytype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    ret = H5Oset_comment_by_name(fid, "mytype", "This is a commited datatype", H5P_DEFAULT);
    HDassert(ret >= 0);

    ret = H5Tclose(tid);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    ret = H5Sclose(sid);
    HDassert(ret >= 0);

    ret = H5Pclose(dcpl);
    HDassert(ret >= 0);

    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}


/*-------------------------------------------------------------------------
 * Function: myfilter
 *
 * Purpose: filter operation callback function; the filter does nothing
 *
 *-------------------------------------------------------------------------
 */
static size_t
myfilter(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
        const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
        size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function: set_local_myfilter
 *
 * Purpose: filter operation "set local" callback
 *
 *-------------------------------------------------------------------------
 */

static herr_t
set_local_myfilter(hid_t dcpl_id, hid_t H5_ATTR_UNUSED tid, hid_t H5_ATTR_UNUSED sid)
{
    unsigned flags;                        /* Filter flags */
    size_t   cd_nelmts = 0;                /* Number of filter parameters */
    unsigned cd_values[2] = {5, 6};        /* Filter parameters */

    /* Get the filter's current parameters */
    if(H5Pget_filter_by_id2(dcpl_id, MYFILTER_ID, &flags, &cd_nelmts, cd_values, 0, NULL, NULL) < 0)
        return(FAIL);

    cd_nelmts = 2;

    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, MYFILTER_ID, flags, cd_nelmts, cd_values) < 0)
        return(FAIL);

    return(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function: gent_fcontents
 *
 * Purpose: generate several files to list its contents
 *
 *-------------------------------------------------------------------------
 */
static void gent_fcontents(void)
{
    hid_t    fid;   /* file id */
    hid_t    gid1;  /* group ID */
    hid_t    tid;   /* datatype ID */
    hsize_t  dims[1]={4};
    int      buf[4]={1,2,3,4};
    int      ret;


    /* create a file */
    fid  = H5Fcreate(FILE46, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);


    write_dset(fid,1,dims,"dset",H5T_NATIVE_INT,buf);


    /*-------------------------------------------------------------------------
     * links
     *-------------------------------------------------------------------------
     */


    /* hard link to "dset" */
    gid1 = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(gid1, "/dset", H5L_SAME_LOC, "dset1", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(gid1);


    /* hard link to "dset" */
    gid1 = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(gid1, "/dset", H5L_SAME_LOC, "dset2", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(gid1);


    /* hard link to "g2" */
    gid1 = H5Gopen2(fid, "/g1", H5P_DEFAULT);
    H5Lcreate_hard(gid1, "/g2", H5L_SAME_LOC, "g1.1", H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(gid1);


    /* hard link to "dset" */
    ret = H5Lcreate_hard(fid, "/dset", H5L_SAME_LOC, "dset3", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);


    /* hard link to "dset" */
    ret = H5Lcreate_hard(fid, "/dset", H5L_SAME_LOC, "dset4", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);


    /* soft link to itself */
    ret = H5Lcreate_soft("mylink", fid, "mylink", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);


    /* soft link to "dset" */
    ret = H5Lcreate_soft("/dset", fid, "softlink", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    /* dangling external link */
    ret = H5Lcreate_external("fname", "oname", fid, "extlink", H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    /* dangling udlink */
    ret = H5Lcreate_ud(fid, "udlink", (H5L_type_t)MY_LINKCLASS, NULL, 0, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * datatypes
     *-------------------------------------------------------------------------
     */
    tid = H5Tcopy(H5T_NATIVE_INT);
    ret = H5Tcommit2(fid, "mytype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);


    /* no name datatype */
    tid = H5Tcopy(H5T_NATIVE_INT);
    ret = H5Tcommit2(fid, "mytype2", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(ret >= 0);
    write_dset(fid, 1, dims, "dsetmytype2", tid, buf);
    ret = H5Ldelete(fid, "mytype2", H5P_DEFAULT);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);


    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */


    ret = H5Fclose(fid);
    HDassert(ret >= 0);


    /* create a file for the bootblock test */
    fid  = H5Fcreate(FILE47, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);


    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}

/*-------------------------------------------------------------------------
 * Function: gent_fvalues
 *
 * Purpose: generate a file for the fill values test
 *
 *-------------------------------------------------------------------------
 */
static void gent_fvalues(void)
{
    /* compound datatype */
    typedef struct c_t
    {
            char   a;
            double b;
    } c_t;


    hid_t      fid;  /* file id */
    hid_t      dcpl; /* dataset creation property list */
    hid_t      sid;  /* dataspace ID */
    hid_t      tid;  /* datatype ID */
    hid_t      did;  /* datasetID */
    hsize_t    dims[1]={2};
    int        buf[2]={1,2};                   /* integer */
    int        fillval1=-99;                   /* integer fill value */
    c_t        buf2[2]={{1,2},{3,4}};          /* compound */
    c_t        fillval2[1]={{1,2}};            /* compound fill value */
    hvl_t      buf3[2];                        /* vlen */
    hvl_t      fillval3;                       /* vlen fill value */
    hsize_t    dimarray[1]={3};                /* array dimension */
    int        buf4[2][3]= {{1,2,3},{4,5,6}};  /* array */
    int        ret;

    /* create a file */
    fid  = H5Fcreate(FILE48, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);

    /* create a space */
    sid = H5Screate_simple(1, dims, NULL);

    /* create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /*-------------------------------------------------------------------------
     * make datasets with fill value combinations
     * H5D_FILL_TIME_IFSET
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET);
    HDassert(ret >= 0);

    ret = H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval1);
    HDassert(ret >= 0);

    ret=make_dset(fid,"fill_time_ifset",sid,H5T_NATIVE_INT,dcpl,buf);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * H5D_FILL_TIME_NEVER
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER);
    HDassert(ret >= 0);

    ret = H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval1);
    HDassert(ret >= 0);

    ret=make_dset(fid,"fill_time_never",sid,H5T_NATIVE_INT,dcpl,buf);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * H5D_FILL_TIME_ALLOC
     *-------------------------------------------------------------------------
     */
    ret = H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC);
    HDassert(ret >= 0);

    ret = H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval1);
    HDassert(ret >= 0);

    ret=make_dset(fid,"fill_time_alloc",sid,H5T_NATIVE_INT,dcpl,buf);
    HDassert(ret >= 0);

    ret = H5Pclose(dcpl);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * dataset with no fill value
     *-------------------------------------------------------------------------
     */
    write_dset(fid,1,dims,"no_fill",H5T_NATIVE_INT,buf);

    /*-------------------------------------------------------------------------
     * dataset with a H5T_COMPOUND fill value
     *-------------------------------------------------------------------------
     */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    tid = H5Tcreate (H5T_COMPOUND, sizeof(c_t));
    H5Tinsert(tid, "a", HOFFSET(c_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(tid, "b", HOFFSET(c_t, b), H5T_NATIVE_DOUBLE);
    ret = H5Pset_fill_value(dcpl, tid, &fillval2);
    HDassert(ret >= 0);
    ret=make_dset(fid,"fill_compound",sid,tid,dcpl,buf2);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);
    ret = H5Pclose(dcpl);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * dataset with a H5T_VLEN fill value
     *-------------------------------------------------------------------------
     */
    buf3[0].len = 1;
    buf3[0].p = HDmalloc( 1 * sizeof(int));
    ((int *)buf3[0].p)[0]=1;
    buf3[1].len = 2;
    buf3[1].p = HDmalloc(2 * sizeof(int));
    ((int *)buf3[1].p)[0] = 2;
    ((int *)buf3[1].p)[1] = 3;

    tid = H5Tvlen_create(H5T_NATIVE_INT);
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    fillval3.p=NULL; fillval3.len=0;
    ret = H5Pset_fill_value(dcpl, tid, &fillval3);
    HDassert(ret >= 0);

    did = H5Dcreate2(fid, "fill_vlen", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    ret = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf3);
    HDassert(ret >= 0);
    ret = H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf3);
    HDassert(ret >= 0);
    ret = H5Dclose(did);
    ret = H5Tclose(tid);
    ret = H5Pclose(dcpl);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * dataset with a H5T_ARRAY fill value
     *-------------------------------------------------------------------------
     */
    tid = H5Tarray_create2(H5T_NATIVE_INT, 1, dimarray);
    write_dset(fid, 1, dims, "fill_array", tid, buf4);
    ret = H5Tclose(tid);


    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    ret = H5Sclose(sid);
    HDassert(ret >= 0);
    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}


/*-------------------------------------------------------------------------
 * Function: gent_string
 *
 * Purpose: make several datasets for the string with escape/not escape test
 *
 *-------------------------------------------------------------------------
 */
static void gent_string(void)
{
    /* compound datatype */
    typedef struct c_t
    {
            int    a;
            char   str[255];
    } c_t;

    hid_t    fid;      /* file id */
    hid_t    sid;      /* dataspace ID */
    hid_t    tid;      /* datatype ID */
    hid_t    str_tid;  /* datatype ID */
    hid_t    did;      /* dataset ID */
    char     buf1[]={"quote \"  backspace\b form feed\f new line\n tab\t new line\n carriage return\r"};
    const char *buf2[SPACE1_DIM1]= {
            "Four score and seven\n years ago our forefathers brought forth on this continent a new nation,",
            "conceived in liberty\n and dedicated to the proposition that all men are created equal.",
            "Now we are engaged\n in a great civil war,",
            "testing whether that\n nation or any nation so conceived and so dedicated can long endure."
    };
    c_t      buf3 = {24, "Four score and seven\n years ago our forefathers brought forth on this continent a new nation"};
    char     buf4[] = {"Four score and seven\n years ago our forefathers brought forth on this continent a new nation"};
    hsize_t  dims1[]  = {1};
    hsize_t        dims2[]  = {SPACE1_DIM1};
    hsize_t        dims4[1];
    int      ret;

    dims4[0] = sizeof(buf4);

    /* create a file */
    fid  = H5Fcreate(FILE49, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);

    /*-------------------------------------------------------------------------
     * str1
     *-------------------------------------------------------------------------
     */

    tid=H5Tcopy(H5T_C_S1);
    ret = H5Tset_size(tid, sizeof(buf1));
    HDassert(ret >= 0);
    write_dset(fid,1,dims1,"str1",tid,buf1);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * str2
     *-------------------------------------------------------------------------
     */
    sid = H5Screate_simple(SPACE1_RANK, dims2, NULL);
    tid = H5Tcopy(H5T_C_S1);
    ret = H5Tset_size(tid, H5T_VARIABLE);
    HDassert(ret >= 0);
    did = H5Dcreate2(fid, "str2", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    ret = H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);
    ret = H5Dclose(did);
    HDassert(ret >= 0);
    ret = H5Sclose(sid);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * str3
     *-------------------------------------------------------------------------
     */
    sid = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    tid = H5Tcreate (H5T_COMPOUND, sizeof(c_t));
    str_tid = H5Tcopy( H5T_C_S1 );
    H5Tset_size( str_tid, 255 );
    H5Tinsert(tid, "a", HOFFSET(c_t, a), H5T_NATIVE_INT);
    H5Tinsert(tid, "str", HOFFSET(c_t, str), str_tid );
    ret=make_dset(fid,"str3",sid,tid,H5P_DEFAULT,&buf3);
    HDassert(ret >= 0);
    ret = H5Tclose(tid);
    HDassert(ret >= 0);
    ret = H5Tclose(str_tid);
    HDassert(ret >= 0);
    ret = H5Sclose(sid);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * str4
     *-------------------------------------------------------------------------
     */
    sid = H5Screate_simple(SPACE1_RANK, dims4, NULL);
    ret=make_dset(fid,"str4",sid,H5T_NATIVE_CHAR,H5P_DEFAULT,buf4);
    ret = H5Sclose(sid);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}


/*-------------------------------------------------------------------------
 * Function: gent_aindices
 *
 * Purpose: make several datasets for the array indices and subsetting tests
 *
 *-------------------------------------------------------------------------
 */
static void gent_aindices(void)
{
    hid_t    fid;     /* file id */
    hid_t    gid[6];  /* group ids */
    hsize_t  dims1[1]  = {100};
    hsize_t  dims2[2]  = {10,10};
    hsize_t  dims3[3]  = {2,10,10};
    hsize_t  dims4[4]  = {2,2,10,10};
    int      buf1[100];
    int      buf2[10][10];
    int      buf3[2][10][10];
    int      buf4[2][2][10][10];
    int      i, j, k, l, n, ret;

    for(i = n = 0; i < 100; i++)
        buf1[i] = n++;

    for(i = n = 0; i < 10; i++)
        for(j = 0; j < 10; j++)
            buf2[i][j] = n++;
    for(i = n = 0; i < 2; i++)
        for(j = 0; j < 10; j++)
            for(k = 0; k < 10; k++)
                buf3[i][j][k] = n++;
    for(i = n = 0; i < 2; i++)
        for(j = 0; j < 2; j++)
            for(k = 0; k < 10; k++)
                for(l = 0; l < 10; l++)
                    buf4[i][j][k][l] = n++;

    /* create a file */
    fid  = H5Fcreate(FILE50, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);

    /*-------------------------------------------------------------------------
     * root datasets
     *-------------------------------------------------------------------------
     */
    write_dset(fid,1,dims1,"1d",H5T_NATIVE_INT,buf1);
    write_dset(fid,2,dims2,"2d",H5T_NATIVE_INT,buf2);
    write_dset(fid,3,dims3,"3d",H5T_NATIVE_INT,buf3);
    write_dset(fid,4,dims4,"4d",H5T_NATIVE_INT,buf4);

    /*-------------------------------------------------------------------------
     * test with group indentation
     *-------------------------------------------------------------------------
     */
    gid[0] = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid[1] = H5Gcreate2(fid, "g1/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid[2] = H5Gcreate2(fid, "g1/g2/g3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid[3] = H5Gcreate2(fid, "g1/g2/g3/g4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid[4] = H5Gcreate2(fid, "g1/g2/g3/g4/g5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid[5] = H5Gcreate2(fid, "g1/g2/g3/g4/g5/g6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    write_dset(gid[5],1,dims1,"1d",H5T_NATIVE_INT,buf1);
    write_dset(gid[5],2,dims2,"2d",H5T_NATIVE_INT,buf2);
    write_dset(gid[5],3,dims3,"3d",H5T_NATIVE_INT,buf3);
    write_dset(gid[5],4,dims4,"4d",H5T_NATIVE_INT,buf4);
    for(i = 0; i < 6; i++)
        H5Gclose(gid[i]);

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    ret = H5Fclose(fid);
    HDassert(ret >= 0);

}


/*-------------------------------------------------------------------------
 * Function: gent_longlinks
 *
 * Purpose: make file with very long names for objects
 *
 *-------------------------------------------------------------------------
 */
static void gent_longlinks(void)
{
    hid_t       fid = (-1);     /* File ID */
    hid_t       gid = (-1);     /* Group ID */
    hid_t       gid2 = (-1);    /* Datatype ID */
    char               *objname = NULL; /* Name of object [Long] */
    size_t              u;              /* Local index variable */

    /* Create files */
    fid = H5Fcreate(FILE51, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid >= 0);

    /* Create group with short name in file (used as target for hard links) */
    gid = H5Gcreate2(fid, "grp1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(gid >= 0);

    /* Construct very long file name */
    objname = (char*) HDmalloc((size_t)(F51_MAX_NAME_LEN + 1));
    HDassert(objname);
    for(u = 0; u < F51_MAX_NAME_LEN; u++)
        objname[u] = 'a';
    objname[F51_MAX_NAME_LEN] = '\0';

    /* Create hard link to existing object */
    HDassert(H5Lcreate_hard(fid, "grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) >= 0);

    /* Create soft link to existing object */
    objname[0] = 'b';
    HDassert(H5Lcreate_soft("grp1", fid, objname, H5P_DEFAULT, H5P_DEFAULT) >= 0);

    /* Create group with long name in existing group */
    gid2 = H5Gcreate2(gid, objname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(gid2 >= 0);

    /* Close objects */
    HDassert(H5Gclose(gid2) >= 0);
    HDassert(H5Gclose(gid) >= 0);
    HDassert(H5Fclose(fid) >= 0);

    /* Release memory */
    HDfree(objname);
}

/*-------------------------------------------------------------------------
 * Function: gent_ldouble
 *
 * Purpose: make file with a long double dataset
 *
 *-------------------------------------------------------------------------
 */
static int gent_ldouble(void)
{
    hid_t       fid;
    hid_t       did;
    hid_t       tid;
    hid_t       sid;
    hsize_t     dims[1] = {3};
    long double buf[3] = {1,2,3};

    if((fid = H5Fcreate(FILE52, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    if((sid = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;

    if((tid = H5Tcopy(H5T_NATIVE_LDOUBLE)) < 0)
        goto error;

    if(H5Tget_size(tid) == 0)
        goto error;

    if((did = H5Dcreate2(fid, "dset", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    if(H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Tclose(tid) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    return 0;

    error:
    printf("error !\n");
    return -1;

}


/*-------------------------------------------------------------------------
 * Function:    gent_binary
 *
 * Purpose:     Generate a file to be used in the binary output test
 *              Contains:
 *              1) an integer dataset
 *              2) a float dataset
 *              3) a double dataset
 *
 *-------------------------------------------------------------------------
 */
static void
gent_binary(void)
{
    hid_t    fid, sid, did, aid;
    hsize_t  dims[1]  = {6};
    int      ibuf[6]  = {1,2,3,4,5,6};
    float    fbuf[6]  = {1,2,3,4,5,6};
    double   dbuf[6]  = {1,2,3,4,5,6};

    fid = H5Fcreate(FILE55, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    sid = H5Screate_simple(1, dims, NULL);


    /*-------------------------------------------------------------------------
     * integer
     *-------------------------------------------------------------------------
     */

    did = H5Dcreate2(fid, "integer", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf);
    H5Dclose(did);

    /*-------------------------------------------------------------------------
     * float
     *-------------------------------------------------------------------------
     */
    did = H5Dcreate2(fid, "float", H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, fbuf);
    H5Dclose(did);

    /*-------------------------------------------------------------------------
     * double
     *-------------------------------------------------------------------------
     */
    did = H5Dcreate2(fid, "double", H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dbuf);
    /* create an attribute */
    aid = H5Acreate2(did, "attr", H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT);
    H5Aclose(aid);
    H5Dclose(did);


    /* close */
    H5Sclose(sid);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function: gen_bigdims
 *
 * Purpose: generate a dataset with dimensions greater than 4GB
 *          and write one hyperslab on the boundary
 *
 *-------------------------------------------------------------------------
 */
#define GB4LL    ((unsigned long long) 4*1024*1024*1024)
#define DIM_4GB  (GB4LL + 10)

static void
gent_bigdims(void)
{
    hid_t   fid = -1;
    hid_t   did = -1;
    hid_t   f_sid = -1;
    hid_t   m_sid = -1;
    hid_t   tid = -1;
    hid_t   dcpl = -1;
    hsize_t dims[1]={DIM_4GB};                 /* dataset dimensions */
    hsize_t chunk_dims[1]={1024};              /* chunk dimensions */
    hsize_t hs_start[1];
    hsize_t hs_size[1];                        /* hyperslab dimensions */
    size_t  size;
    char    fillvalue=0;
    char    *buf=NULL;
    hsize_t i;
    char    c;
    size_t  nelmts;
    int     ret;

    /* create a file */
    fid  = H5Fcreate(FILE56, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid >= 0);

    /* create dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto out;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_SCHAR, &fillvalue) < 0)
        goto out;
    if(H5Pset_chunk(dcpl, 1, chunk_dims) < 0)
        goto out;
    if((f_sid = H5Screate_simple(1, dims, NULL)) < 0)
        goto out;
    if((did = H5Dcreate2(fid, "dset4gb", H5T_NATIVE_SCHAR, f_sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto out;
    if((tid = H5Dget_type(did)) < 0)
        goto out;
    if((size = H5Tget_size(tid)) <= 0)
        goto out;

    /* select an hyperslab */
    nelmts = 20;
    hs_start[0] = GB4LL - 10;
    hs_size[0]  = nelmts;

    if((m_sid = H5Screate_simple(1, hs_size, hs_size)) < 0)
        goto out;

    buf=(char *) HDmalloc((unsigned)(nelmts*size));

    for(i=0, c=0; i<nelmts; i++, c++)
    {
        buf[i] = c;
    }

    if(H5Sselect_hyperslab (f_sid,H5S_SELECT_SET,hs_start,NULL,hs_size,NULL) < 0)
        goto out;
    if(H5Dwrite (did,H5T_NATIVE_SCHAR,m_sid,f_sid,H5P_DEFAULT,buf) < 0)
        goto out;


    HDfree(buf);
    buf=NULL;

    /* close */
    if(H5Tclose(tid) < 0)
        goto out;
    tid = -1;
    if(H5Sclose(f_sid) < 0)
        goto out;
    f_sid = -1;
    if(H5Sclose(m_sid) < 0)
        goto out;
    m_sid = -1;
    if(H5Pclose(dcpl) < 0)
        goto out;
    dcpl = -1;
    if(H5Dclose(did) < 0)
        goto out;
    did = -1;

    ret = H5Fclose(fid);
    HDassert(ret >= 0);

    return;

    out:
    printf("Error.....\n");
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(f_sid);
        H5Sclose(m_sid);
        H5Tclose(tid);
        H5Dclose(did);
        H5Fclose(fid);
    } H5E_END_TRY;
    return;
}



/*-------------------------------------------------------------------------
 * Function: gent_hyperslab
 *
 * Purpose: make a dataset for hyperslab read
 *
 *-------------------------------------------------------------------------
 */
static void
gent_hyperslab(void)
{
    hid_t    fid;     /* file id */
    hsize_t  dims[2]  = {32,4097}; /* big enough data size to force a second stripmine read */
    double   *buf;
    int      i, ret;

    buf = (double*) HDmalloc(32 * 4097 * sizeof(double) );
    for(i = 0; i < 32 * 4097; i++)
        buf[i] = 1;

    /* create a file */
    fid  = H5Fcreate(FILE57, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);

    write_dset(fid,2,dims,"stripmine",H5T_NATIVE_DOUBLE,buf);

    ret = H5Fclose(fid);
    HDassert(ret >= 0);

    HDfree(buf);
}

/*-------------------------------------------------------------------------
 * Function: gent_group_creation_order
 *
 * Purpose: generate a file with several groups with creation order set and not
 *  set tru its hierarchy
 *
 *-------------------------------------------------------------------------
 */
static void
gent_group_creation_order(void)
{
    hid_t    fid = -1;      /* file ID */
    hid_t    gid = -1;      /* group ID */
    hid_t    gcpl_id = -1;  /* group creation property list ID */
    hid_t    fcpl_id = -1;  /* file creation property list ID (to set root group order) */

    if((fcpl_id = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto out;

    if(H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED ) < 0)
        goto out;

    if((fid = H5Fcreate(FILE58, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT)) < 0)
        goto out;


    /* create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
     * create a group "2"
     *-------------------------------------------------------------------------
     */


    if((gid = H5Gcreate2(fid, "2", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    if((gid = H5Gcreate2(fid, "2/c", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "2/b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "2/a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    if((gid = H5Gcreate2(fid, "2/a/a2", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "2/a/a1", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    if((gid = H5Gcreate2(fid, "2/a/a2/a22", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "2/a/a2/a21", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;


    /*-------------------------------------------------------------------------
     * create a group "1" with H5P_CRT_ORDER_TRACKED set
     *-------------------------------------------------------------------------
     */
    if(H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0)
        goto out;


    if((gid = H5Gcreate2(fid, "1", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    if((gid = H5Gcreate2(fid, "1/c", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "1/b", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "1/a", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    if((gid = H5Gcreate2(fid, "1/a/a2", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "1/a/a1", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    if((gid = H5Gcreate2(fid, "1/a/a2/a22", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;
    if((gid = H5Gcreate2(fid, "1/a/a2/a21", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;


    if(H5Pclose(gcpl_id) < 0)
        goto out;
    gcpl_id = -1;
    if(H5Pclose(fcpl_id) < 0)
        goto out;
    fcpl_id = -1;
    if(H5Fclose(fid) < 0)
        goto out;
    fid = -1;

    return;

    out:
    printf("Error.....\n");
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Pclose(gcpl_id);
        H5Pclose(fcpl_id);
        H5Fclose(fid);

    } H5E_END_TRY;
    return;

}

/*-------------------------------------------------------------------------
 * Function: gent_attr_creation_order
 *
 * Purpose: generate a file with several objects with attributes with creation
 *  order set and not set
 *
 *-------------------------------------------------------------------------
 */
static void
gent_attr_creation_order(void)
{
    hid_t    fid = -1;      /* file id */
    hid_t    gid = -1;      /* group id */
    hid_t    did = -1;      /* dataset id */
    hid_t    sid = -1;      /* space id */
    hid_t    aid = -1;      /* attribute id */
    hid_t    tid = -1;      /* datatype id */
    hid_t    gcpl_id = -1;  /* group creation property list ID */
    hid_t    dcpl_id = -1;  /* dataset creation property list ID */
    hid_t    tcpl_id = -1;  /* datatype creation property list ID */
    int      i;
    const char *attr_name[3] = {"c", "b", "a" };

    if((fid = H5Fcreate(FILE59, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* create group creation property list */
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        goto out;

    /* create dataset creation property list */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto out;

    /* create dataset creation property list */
    if((tcpl_id = H5Pcreate(H5P_DATATYPE_CREATE)) < 0)
        goto out;

    /* enable attribute creation order tracking on dataset property list */
    if(H5Pset_attr_creation_order(dcpl_id, H5P_CRT_ORDER_TRACKED) < 0)
        goto out;

    /* enable attribute creation order tracking on group property list */
    if(H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0)
        goto out;

    /* enable attribute creation order tracking on datatype property list */
    if(H5Pset_attr_creation_order(tcpl_id, H5P_CRT_ORDER_TRACKED) < 0)
        goto out;

    /* create a dataspace */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        goto out;

    /*-------------------------------------------------------------------------
     * create a dataset with creation order tracked for attributes and atributes in it
     *-------------------------------------------------------------------------
     */

    /* create a dataset */
    if((did = H5Dcreate2(fid, "dt", H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(did, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Dclose(did) < 0)
        goto out;
    did = -1;


    /*-------------------------------------------------------------------------
     * create a dataset without creation order tracked for attributes and atributes in it
     *-------------------------------------------------------------------------
     */

    /* create a dataset */
    if((did = H5Dcreate2(fid, "d", H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(did, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Dclose(did) < 0)
        goto out;
    did = -1;



    /*-------------------------------------------------------------------------
     * create a group with creation order tracked for attributes and atributes in it
     *-------------------------------------------------------------------------
     */

    if((gid = H5Gcreate2(fid, "gt", H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(gid, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    /*-------------------------------------------------------------------------
     * create a group without creation order tracked for attributes and atributes in it
     *-------------------------------------------------------------------------
     */

    if((gid = H5Gcreate2(fid, "g", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(gid, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    /*-------------------------------------------------------------------------
     * create a named datatype with creation order tracked for attributes and atributes in it
     *-------------------------------------------------------------------------
     */

    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto out;

    if((H5Tcommit2(fid, "tt", tid, H5P_DEFAULT, tcpl_id, H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(tid, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Tclose(tid) < 0)
        goto out;
    tid = -1;

    /*-------------------------------------------------------------------------
     * create a named datatype without creation order tracked for attributes and atributes in it
     *-------------------------------------------------------------------------
     */

    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto out;

    if((H5Tcommit2(fid, "t", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(tid, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Tclose(tid) < 0)
        goto out;
    tid = -1;

    /*-------------------------------------------------------------------------
     * add some attributes to the root group
     *-------------------------------------------------------------------------
     */
    if((gid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0)
        goto out;

    /* add attributes */
    for(i = 0; i < 3; i++)
    {
        if((aid = H5Acreate2(gid, attr_name[i], H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto out;

        /* close attribute */
        if(H5Aclose(aid) < 0)
            goto out;
        aid = -1;
    } /* end for */

    if(H5Gclose(gid) < 0)
        goto out;
    gid = -1;

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    if(H5Sclose(sid) < 0)
        goto out;
    sid = -1;
    if(H5Pclose(dcpl_id) < 0)
        goto out;
    dcpl_id = -1;
    if(H5Pclose(gcpl_id) < 0)
        goto out;
    gcpl_id = -1;
    if(H5Pclose(tcpl_id) < 0)
        goto out;
    tcpl_id = -1;
    if(H5Fclose(fid) < 0)
        goto out;
    fid = -1;



    return;

    out:
    printf("Error.....\n");
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(gcpl_id);
        H5Pclose(dcpl_id);
        H5Pclose(tcpl_id);
        H5Fclose(fid);

    } H5E_END_TRY;
    return;

}

/*-------------------------------------------------------------------------
 * Function:    gent_fpformat
 *
 * Purpose:     Generate a file to be used in the floating point format test
 *              Contains:
 *              1) a float dataset
 *              2) a double dataset
 *
 *-------------------------------------------------------------------------
 */
static void
gent_fpformat(void)
{
    hid_t    fid, sid, did;
    hsize_t  dims[1]  = {6};
    double   dbuf[6]  = {-0.1234567f, 0.1234567f, 0, 0, 0, 0};
    float    fbuf[6]  = {-0.1234567f, 0.1234567f, 0, 0, 0, 0};

    fid = H5Fcreate(FILE60, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    sid = H5Screate_simple(1, dims, NULL);

    /*-------------------------------------------------------------------------
     * double
     *-------------------------------------------------------------------------
     */
    did = H5Dcreate2(fid, "double", H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dbuf);
    H5Dclose(did);


    /*-------------------------------------------------------------------------
     * float
     *-------------------------------------------------------------------------
     */
    did = H5Dcreate2(fid, "float", H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, fbuf);
    H5Dclose(did);


    /* close */
    H5Sclose(sid);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_extlinks
 *
 * Purpose:     Generate 3 files to be used in the external links test
 *   External links point from one HDF5 file to an object (Group, Dataset,
 *    or committed Datatype) in another file.  Try to create cycles.
 *
 *-------------------------------------------------------------------------
 */
static void
gent_extlinks(void)
{
    hid_t    source_fid, target_fid, far_fid, sid, did, gid, gid2, tid;
    hsize_t  dims[1]  = {6};
    int      buf[6]  = {1, 2, 3, 4, 5, 6};

    /* create two files, a source and a target */
    source_fid = H5Fcreate(FILE61, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    target_fid = H5Fcreate(FILE62, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    far_fid = H5Fcreate(FILE63, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);


    /*-------------------------------------------------------------------------
     * create Groups, a Dataset, a committed Datatype, external links, and a
     * cycle in the target
     *-------------------------------------------------------------------------
     */

    gid = H5Gcreate2(target_fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Gclose(H5Gcreate2(target_fid, "empty_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT));
    sid = H5Screate_simple(1, dims, NULL);
    did = H5Dcreate2(gid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    H5Lcreate_external(FILE61, "/", gid, "elink_t1", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external(FILE61, "/ext_link4", gid, "elink_t2", H5P_DEFAULT, H5P_DEFAULT);

    gid2 = H5Gcreate2(gid, "subgroup", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_hard(target_fid, "/group", gid2, "link_to_group", H5P_DEFAULT, H5P_DEFAULT);

    H5Dclose(did);
    H5Sclose(sid);
    H5Gclose(gid2);
    H5Gclose(gid);


    sid = H5Screate_simple(1, dims, NULL);
    did = H5Dcreate2(target_fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    H5Dclose(did);
    H5Sclose(sid);

    tid = H5Tcopy(H5T_NATIVE_INT);
    H5Tcommit2(target_fid, "type", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    H5Tclose(tid);

    /*-------------------------------------------------------------------------
     * create external links in the source file pointing to the target objects
     *-------------------------------------------------------------------------
     */

    H5Lcreate_external(FILE62, "group", source_fid, "ext_link1", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external(FILE62, "dset", source_fid, "ext_link2", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external(FILE62, "type", source_fid, "ext_link3", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external(FILE62, "group/elink_t2", source_fid, "ext_link4", H5P_DEFAULT, H5P_DEFAULT);
    H5Lcreate_external(FILE62, "empty_group", source_fid, "ext_link5", H5P_DEFAULT, H5P_DEFAULT);
    /* external link to soft link which linked to a dataset */
    H5Lcreate_external(FILE4_1, "/soft_dset1", source_fid, "ext2soft_link1", H5P_DEFAULT, H5P_DEFAULT);

    /* external link to dangle soft link  */
    H5Lcreate_external(FILE4_1, "/soft_dangle", source_fid, "ext2softdangle_link1", H5P_DEFAULT, H5P_DEFAULT);

    /*-------------------------------------------------------------------------
     * create external link in the "far" file pointing to the source file
     *-------------------------------------------------------------------------
     */
    H5Lcreate_external(FILE61, "/", far_fid, "src_file", H5P_DEFAULT, H5P_DEFAULT);

    /* close */
    H5Fclose(source_fid);
    H5Fclose(target_fid);
    H5Fclose(far_fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_fs_strategy_threshold
 *
 * Purpose:     Generate a file with non-default file space strategy and
 *    non-default free-space section threshold.
 *-------------------------------------------------------------------------
 */
static void
gent_fs_strategy_threshold(void)
{
    hid_t    fid;  /* File id */
    hid_t    fcpl;  /* File creation property */

    /* Create file-creation template */
    fcpl = H5Pcreate(H5P_FILE_CREATE);

    /* Set file space information */
    H5Pset_file_space(fcpl, STRATEGY, (hsize_t)THRESHOLD10);

    /* Create the file with the specified strategy and threshold */
    fid = H5Fcreate(FILE65, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);

    /* close */
    H5Fclose(fid);
    H5Pclose(fcpl);
}

/*
 * Create a file with new format:
 * Create one dataset with (set_chunk, fixed dims, null max. dims)
 *    so that Fixed Array indexing will be used.
 * Create one dataset with (set_chunk, fixed dims, null max. dims, filter)
 *    so that Fixed Array indexing will be used.
 * Create one dataset with (set_chunk, fixed dims, fixed max. dims)
 *    so that Fixed Array indexing will be used.
 *
 * Modifications:
 *    Fixed Array indexing will be used for chunked dataset
 *    with fixed max. dims setting.
 *
 */
static void
gent_dataset_idx(void)
{
    hid_t fid, space, dcpl, fapl;
    hsize_t dims[2];
    hsize_t maxdims[2];
    int buf[20][10];
    int i, j, ret;

    /* Get a copy of the file aaccess property */
    fapl = H5Pcreate(H5P_FILE_ACCESS);

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    assert(ret >= 0);

    fid = H5Fcreate(FILE68a, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    dims[0] = F68a_CHUNK;
    dims[1] = F68a_CHUNK;

    /* set chunk */
    ret = H5Pset_chunk(dcpl, RANK, dims);
    assert(ret >= 0);

    /* dataset with fixed dimensions */
    dims[0] = F68a_DIM20;
    dims[1] = F68a_DIM10;
    space = H5Screate_simple(RANK, dims, NULL);

    for(i = 0; i < F68a_DIM20; i++)
         for(j = 0; j < F68a_DIM10; j++)
              buf[i][j] = j;

    ret = make_dset(fid, F68a_DSET_FIXED, space, H5T_NATIVE_INT, dcpl, buf);
    assert(ret >= 0);
    H5Sclose(space);

    /* dataset with non-fixed dimensions */
    maxdims[0] = F68a_DIM200;
    maxdims[1] = F68a_DIM100;
    space = H5Screate_simple(RANK, dims, maxdims);

    ret = make_dset(fid, F68a_DSET_BTREE, space, H5T_NATIVE_INT, dcpl, buf);
    assert(ret >= 0);
    H5Sclose(space);

#if defined (H5_HAVE_FILTER_DEFLATE)

    /* dataset with fixed dimensions and filters */
    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl, H5Z_FILTER_ALL);
    assert(ret >= 0);

    /* set deflate data */
    ret = H5Pset_deflate(dcpl, 9);
    assert(ret >= 0);

    space = H5Screate_simple(RANK, dims, NULL);
    ret = make_dset(fid, F68a_DSET_FIXED_FILTER, space, H5T_NATIVE_INT, dcpl, buf);
    assert(ret >= 0);

    H5Sclose(space);
#endif

    H5Pclose(dcpl);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_packedbits
 *
 * Purpose:     Generate a file to be used in the h5dump packed bits tests.
 *   Four datasets of 1, 2, 4 and 8 bytes of unsigned int types are created.
 *   Four more datasets of 1, 2, 4 and 8 bytes of signed int types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *   A dummy dataset of double type is created for failure test.
 * Created:  Albert Cheng, 2010/5/10.
 * Modified: Allen Byrne, 2011/1/5 Use file to test Signed/Unsigned datatypes
 *-------------------------------------------------------------------------
 */
static void
gent_packedbits(void)
{
    hid_t fid, dataset, space;
    hsize_t dims[2];
    uint8_t  dsetu8[F66_XDIM][F66_YDIM8],   valu8bits;
    uint16_t dsetu16[F66_XDIM][F66_YDIM16], valu16bits;
    uint32_t dsetu32[F66_XDIM][F66_YDIM32], valu32bits;
    uint64_t dsetu64[F66_XDIM][F66_YDIM64], valu64bits;
    int8_t  dset8[F66_XDIM][F66_YDIM8],   val8bits;
    int16_t dset16[F66_XDIM][F66_YDIM16], val16bits;
    int32_t dset32[F66_XDIM][F66_YDIM32], val32bits;
    int64_t dset64[F66_XDIM][F66_YDIM64], val64bits;
    double  dsetdbl[F66_XDIM][F66_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE66, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Dataset of 8 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU08, H5T_STD_U8LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu8bits = (uint8_t) ~0u;  /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu8[i][0] = valu8bits;
        for(j = 1; j < dims[1]; j++)
            dsetu8[i][j] = (uint8_t)(dsetu8[i][j - 1] << 1);
        valu8bits = (uint8_t)(valu8bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu8);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM16;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU16, H5T_STD_U16LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu16bits = (uint16_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu16[i][0] = valu16bits;
        for(j = 1; j < dims[1]; j++)
            dsetu16[i][j] = (uint16_t)(dsetu16[i][j-1] << 1);
        valu16bits = (uint16_t)(valu16bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu16);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM32;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU32, H5T_STD_U32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu32bits = (uint32_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu32[i][0] = valu32bits;
        for(j = 1; j < dims[1]; j++)
            dsetu32[i][j] = dsetu32[i][j-1] << 1;
        valu32bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu32);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM64;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU64, H5T_STD_U64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu64bits = (uint64_t) ~0Lu;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu64[i][0] = valu64bits;
        for(j = 1; j < dims[1]; j++)
            dsetu64[i][j] = dsetu64[i][j-1] << 1;
        valu64bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu64);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 8 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS08, H5T_STD_I8LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val8bits = (int8_t) ~0;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset8[i][0] = val8bits;
        for(j = 1; j < dims[1]; j++)
            dset8[i][j] = (int8_t)(dset8[i][j-1] << 1);
        val8bits = (int8_t)(val8bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_INT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset8);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM16;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS16, H5T_STD_I16LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val16bits = (int16_t) ~0;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset16[i][0] = val16bits;
        for(j = 1; j < dims[1]; j++)
            dset16[i][j] = (int16_t)(dset16[i][j-1] << 1);
        val16bits = (int16_t)(val16bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_INT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset16);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM32;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS32, H5T_STD_I32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val32bits = (int32_t) ~0;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset32[i][0] = val32bits;
        for(j = 1; j < dims[1]; j++)
            dset32[i][j] = dset32[i][j-1] << 1;
        val32bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset32);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM64;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS64, H5T_STD_I64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val64bits = (int64_t) ~0L;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset64[i][0] = val64bits;
        for(j = 1; j < dims[1]; j++)
            dset64[i][j] = dset64[i][j-1] << 1;
        val64bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_INT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset64);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Double Dummy set for failure tests */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DUMMYDBL, H5T_IEEE_F64BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++)
            dsetdbl[i][j] = 0.0001F * (float)j + (float)i;

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetdbl);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_attr_packedbits
 *
 * Purpose:     Generate a file to be used in the h5dump packed bits tests.
 *   Four attributes of 1, 2, 4 and 8 bytes of unsigned int types are created.
 *   Four more datasets of 1, 2, 4 and 8 bytes of signed int types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *   A dummy dataset of double type is created for failure test.
 *   Use file to test Signed/Unsigned datatypes and keep in sync with gent_packedbits()
 *-------------------------------------------------------------------------
 */
static void
gent_attr_intsize(void)
{
    hid_t fid, attr, space, root;
    hsize_t dims[2];
    uint8_t  dsetu8[F66_XDIM][F66_YDIM8],   valu8bits;
    uint16_t dsetu16[F66_XDIM][F66_YDIM16], valu16bits;
    uint32_t dsetu32[F66_XDIM][F66_YDIM32], valu32bits;
    uint64_t dsetu64[F66_XDIM][F66_YDIM64], valu64bits;
    int8_t  dset8[F66_XDIM][F66_YDIM8],   val8bits;
    int16_t dset16[F66_XDIM][F66_YDIM16], val16bits;
    int32_t dset32[F66_XDIM][F66_YDIM32], val32bits;
    int64_t dset64[F66_XDIM][F66_YDIM64], val64bits;
    double  dsetdbl[F66_XDIM][F66_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE69, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* Attribute of 8 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETU08, H5T_STD_U8LE, space, H5P_DEFAULT, H5P_DEFAULT);

    valu8bits = (uint8_t) ~0u;  /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu8[i][0] = valu8bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu8[i][j] = (uint8_t)(dsetu8[i][j-1] << 1);
        }
        valu8bits = (uint8_t)(valu8bits << 1);
    }

    H5Awrite(attr, H5T_NATIVE_UINT8, dsetu8);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 16 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM16;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETU16, H5T_STD_U16LE, space, H5P_DEFAULT, H5P_DEFAULT);

    valu16bits = (uint16_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu16[i][0] = valu16bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu16[i][j] = (uint16_t)(dsetu16[i][j-1] << 1);
        }
        valu16bits = (uint16_t)(valu16bits << 1);
    }

    H5Awrite(attr, H5T_NATIVE_UINT16, dsetu16);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 32 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM32;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETU32, H5T_STD_U32LE, space, H5P_DEFAULT, H5P_DEFAULT);

    valu32bits = (uint32_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu32[i][0] = valu32bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu32[i][j] = dsetu32[i][j-1] << 1;
        }
        valu32bits <<= 1;
    }

    H5Awrite(attr, H5T_NATIVE_UINT32, dsetu32);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 64 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM64;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETU64, H5T_STD_U64LE, space, H5P_DEFAULT, H5P_DEFAULT);

    valu64bits = (uint64_t) ~0Lu;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu64[i][0] = valu64bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu64[i][j] = dsetu64[i][j-1] << 1;
        }
        valu64bits <<= 1;
    }

    H5Awrite(attr, H5T_NATIVE_UINT64, dsetu64);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 8 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETS08, H5T_STD_I8LE, space, H5P_DEFAULT, H5P_DEFAULT);

    val8bits = (int8_t) ~0; /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset8[i][0] = val8bits;
        for(j = 1; j < dims[1]; j++) {
            dset8[i][j] = (int8_t)(dset8[i][j-1] << 1);
        }
        val8bits = (int8_t)(val8bits << 1);
    }

    H5Awrite(attr, H5T_NATIVE_INT8, dset8);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 16 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM16;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETS16, H5T_STD_I16LE, space, H5P_DEFAULT, H5P_DEFAULT);

    val16bits = (int16_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset16[i][0] = val16bits;
        for(j = 1; j < dims[1]; j++) {
            dset16[i][j] = (int16_t)(dset16[i][j-1] << 1);
        }
        val16bits = (int16_t)(val16bits << 1);
    }

    H5Awrite(attr, H5T_NATIVE_INT16, dset16);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 32 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM32;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETS32, H5T_STD_I32LE, space, H5P_DEFAULT, H5P_DEFAULT);

    val32bits = (int32_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset32[i][0] = val32bits;
        for(j = 1; j < dims[1]; j++) {
            dset32[i][j] = dset32[i][j-1] << 1;
        }
        val32bits <<= 1;
    }

    H5Awrite(attr, H5T_NATIVE_INT32, dset32);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 64 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM64;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DATASETS64, H5T_STD_I64LE, space, H5P_DEFAULT, H5P_DEFAULT);

    val64bits = (int64_t) ~0L;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset64[i][0] = val64bits;
        for(j = 1; j < dims[1]; j++) {
            dset64[i][j] = dset64[i][j-1] << 1;
        }
        val64bits <<= 1;
    }

    H5Awrite(attr, H5T_NATIVE_INT64, dset64);
    H5Sclose(space);
    H5Aclose(attr);

    /* Double Dummy set for failure tests */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    attr = H5Acreate2(root, F66_DUMMYDBL, H5T_IEEE_F64BE, space, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++)
            dsetdbl[i][j] = 0.0001F * (float)j + (float)i;

    H5Awrite(attr, H5T_NATIVE_DOUBLE, dsetdbl);

    H5Sclose(space);
    H5Aclose(attr);

    H5Gclose(root);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_charsets
 *
 * Purpose:     Generate a file to be used in the character set test
 *              Contains:
 *              1) a ascii datatype
 *              2) a utf8 datatype
 *
 *-------------------------------------------------------------------------
 */
static void
gent_charsets(void)
{
    hid_t fid, did, sid;
    herr_t status;
    hsize_t dim[] = {1}; /* Dataspace dimensions */
    typedef struct CharSetInfo {
            const char *ascii_p_;
            const char *utf8_p_;
    } CharSetInfo;

    hid_t charset_dtid = H5Tcreate( H5T_COMPOUND, sizeof( CharSetInfo ) );
    hid_t ascii_dtid = H5Tcreate( H5T_STRING, H5T_VARIABLE );
    hid_t utf8_dtid = H5Tcreate( H5T_STRING, H5T_VARIABLE );
    const char * writeData[] = { "ascii", "utf8", };

    sid = H5Screate_simple( 1, dim, NULL );
    fid = H5Fcreate( FILE68, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
    status = H5Tset_cset( ascii_dtid, H5T_CSET_ASCII );
    HDassert(status >= 0);
    H5Tinsert( charset_dtid, "ascii", HOFFSET(CharSetInfo, ascii_p_ ), ascii_dtid );

    status = H5Tset_cset( utf8_dtid, H5T_CSET_UTF8 );
    HDassert(status >= 0);
    H5Tinsert( charset_dtid, "utf8", HOFFSET( CharSetInfo, utf8_p_ ), utf8_dtid );

    did = H5Dcreate2( fid, "CharSets", charset_dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );

    status = H5Dwrite( did, charset_dtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, writeData );
    HDassert(status >= 0);

    H5Tclose( charset_dtid );
    H5Tclose( ascii_dtid );
    H5Tclose( utf8_dtid );
    H5Sclose( sid );
    H5Dclose( did );
    H5Fclose( fid );
}

static void gent_compound_intsizes(void) {
    hid_t fid, dataset, space;
    hsize_t dims[2];
    hsize_t    array_dim8[]={F70_XDIM,F70_YDIM8}; /* Array dimensions         */
    hsize_t    array_dim16[]={F70_XDIM,F70_YDIM16}; /* Array dimensions         */
    hsize_t    array_dim32[]={F70_XDIM,F70_YDIM32}; /* Array dimensions         */
    hsize_t    array_dim64[]={F70_XDIM,F70_YDIM64}; /* Array dimensions         */
    hid_t      arrayu8_tid;                 /* Array datatype handle    */
    hid_t      arrayu16_tid;                /* Array datatype handle    */
    hid_t      arrayu32_tid;                /* Array datatype handle    */
    hid_t      arrayu64_tid;                /* Array datatype handle    */
    hid_t      array8_tid;                 /* Array datatype handle    */
    hid_t      array16_tid;                /* Array datatype handle    */
    hid_t      array32_tid;                /* Array datatype handle    */
    hid_t      array64_tid;                /* Array datatype handle    */
    hid_t      arraydbl_tid;                /* Array datatype handle    */
    uint8_t  valu8bits;
    uint16_t valu16bits;
    uint32_t valu32bits;
    uint64_t valu64bits;
    int8_t  val8bits;
    int16_t val16bits;
    int32_t val32bits;
    int64_t val64bits;
    /* Structure and array for compound types                             */
    typedef struct Array1Struct {
            uint8_t  dsetu8[F70_XDIM][F70_YDIM8];
            uint16_t dsetu16[F70_XDIM][F70_YDIM16];
            uint32_t dsetu32[F70_XDIM][F70_YDIM32];
            uint64_t dsetu64[F70_XDIM][F70_YDIM64];
            int8_t  dset8[F70_XDIM][F70_YDIM8];
            int16_t dset16[F70_XDIM][F70_YDIM16];
            int32_t dset32[F70_XDIM][F70_YDIM32];
            int64_t dset64[F70_XDIM][F70_YDIM64];
            double  dsetdbl[F70_XDIM][F70_YDIM8];
    } Array1Struct;
    Array1Struct *Array1;

    hid_t Array1Structid; /* File datatype identifier */
    herr_t status; /* Error checking variable */
    hsize_t dim[] = { F70_LENGTH }; /* Dataspace dimensions     */

    int m, n, o; /* Array init loop vars     */

    /* Allocate buffer */
    Array1 = (Array1Struct *)HDmalloc(sizeof(Array1Struct) * F70_LENGTH);
    HDassert(Array1);

    /* Initialize the data in the arrays/datastructure                */
    for (m = 0; m < F70_LENGTH; m++) {

        /* Array of 8 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM8;

        valu8bits = (uint8_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu8[n][0] = valu8bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dsetu8[n][o] = (uint8_t)(Array1[m].dsetu8[n][o-1] << 1);
            valu8bits = (uint8_t)(valu8bits << 1);
        }

        /* Array of 16 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM16;

        valu16bits = (uint16_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu16[n][0] = valu16bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dsetu16[n][o] = (uint16_t)(Array1[m].dsetu16[n][o-1] << 1);
            valu16bits = (uint16_t)(valu16bits << 1);
        }

        /* Array of 32 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM32;

        valu32bits = (uint32_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu32[n][0] = valu32bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dsetu32[n][o] = Array1[m].dsetu32[n][o-1] << 1;
            valu32bits <<= 1;
        }

        /* Array of 64 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM64;

        valu64bits = (uint64_t) ~0Lu;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu64[n][0] = valu64bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dsetu64[n][o] = Array1[m].dsetu64[n][o-1] << 1;
            valu64bits <<= 1;
        }

        /* Array of 8 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM8;

        val8bits = (int8_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset8[n][0] = val8bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dset8[n][o] = (int8_t)(Array1[m].dset8[n][o-1] << 1);
            val8bits = (int8_t)(val8bits << 1);
        }

        /* Array of 16 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM16;

        val16bits = (int16_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset16[n][0] = val16bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dset16[n][o] = (int16_t)(Array1[m].dset16[n][o-1] << 1);
            val16bits = (int16_t)(val16bits << 1);
        }

        /* Array of 32 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM32;

        val32bits = (int32_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset32[n][0] = val32bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dset32[n][o] = Array1[m].dset32[n][o-1] << 1;
            val32bits <<= 1;
        }

        /* Array of 64 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM64;

        val64bits = (int64_t) ~0L;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset64[n][0] = val64bits;
            for(o = 1; o < (int)dims[1]; o++)
                Array1[m].dset64[n][o] = Array1[m].dset64[n][o-1] << 1;
            val64bits <<= 1;
        }

        /* Double Dummy set for failure tests */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM8;

        for(n = 0; n < (int)dims[0]; n++)
            for(o = 0; o < (int)dims[1]; o++)
                Array1[m].dsetdbl[n][o] = 0.0001F * (float)o + (float)n;
    }

    /* Create the array data type for the 8 bits signed int array             */
    array8_tid = H5Tarray_create2(H5T_NATIVE_SCHAR, F70_ARRAY_RANK, array_dim8);
    HDassert(array8_tid >= 0);

    /* Create the array data type for the 16 bits signed int array             */
    array16_tid = H5Tarray_create2(H5T_NATIVE_SHORT, F70_ARRAY_RANK, array_dim16);
    HDassert(array16_tid >= 0);

    /* Create the array data type for the 32 bits signed int array             */
    array32_tid = H5Tarray_create2(H5T_NATIVE_INT, F70_ARRAY_RANK, array_dim32);
    HDassert(array32_tid >= 0);

    /* Create the array data type for the 64 bits signed int array             */
    array64_tid = H5Tarray_create2(H5T_NATIVE_LONG, F70_ARRAY_RANK, array_dim64);
    HDassert(array64_tid >= 0);

    /* Create the array data type for the 8 bits signed int array             */
    arrayu8_tid = H5Tarray_create2(H5T_NATIVE_UCHAR, F70_ARRAY_RANK, array_dim8);
    HDassert(arrayu8_tid >= 0);

    /* Create the array data type for the 16 bits signed int array             */
    arrayu16_tid = H5Tarray_create2(H5T_NATIVE_USHORT, F70_ARRAY_RANK, array_dim16);
    HDassert(arrayu16_tid >= 0);

    /* Create the array data type for the 32 bits signed int array             */
    arrayu32_tid = H5Tarray_create2(H5T_NATIVE_UINT, F70_ARRAY_RANK, array_dim32);
    HDassert(arrayu32_tid >= 0);

    /* Create the array data type for the 64 bits signed int array             */
    arrayu64_tid = H5Tarray_create2(H5T_NATIVE_ULONG, F70_ARRAY_RANK, array_dim64);
    HDassert(arrayu64_tid >= 0);

    /* Create the array data type for the 32 bits double array             */
    arraydbl_tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F70_ARRAY_RANK, array_dim8);
    HDassert(arraydbl_tid >= 0);

    /* Create the dataspace                                           */
    space = H5Screate_simple(F70_RANK, dim, NULL);
    HDassert(space >= 0);

    /* Create the file                                                */
    fid = H5Fcreate(FILE70, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid >= 0);

    /* Create the memory data type                                    */
    Array1Structid = H5Tcreate(H5T_COMPOUND, sizeof(Array1Struct));
    HDassert(Array1Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Array1Structid, F70_DATASETU08, HOFFSET(Array1Struct, dsetu8), arrayu8_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETU16, HOFFSET(Array1Struct, dsetu16), arrayu16_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETU32, HOFFSET(Array1Struct, dsetu32), arrayu32_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETU64, HOFFSET(Array1Struct, dsetu64), arrayu64_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS08, HOFFSET(Array1Struct, dset8), array8_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS16, HOFFSET(Array1Struct, dset16), array16_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS32, HOFFSET(Array1Struct, dset32), array32_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS64, HOFFSET(Array1Struct, dset64), array64_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DUMMYDBL, HOFFSET(Array1Struct, dsetdbl), arraydbl_tid);
    HDassert(status >= 0);

    /* Create the dataset                                             */
    dataset = H5Dcreate2(fid, F70_DATASETNAME, Array1Structid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the dataset                                      */
    status = H5Dwrite(dataset, Array1Structid, H5S_ALL, H5S_ALL, H5P_DEFAULT, Array1);
    HDassert(status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Array1Structid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu8_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu16_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu32_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu64_tid);
    HDassert(status >= 0);

    status = H5Tclose(array8_tid);
    HDassert(status >= 0);

    status = H5Tclose(array16_tid);
    HDassert(status >= 0);

    status = H5Tclose(array32_tid);
    HDassert(status >= 0);

    status = H5Tclose(array64_tid);
    HDassert(status >= 0);

    status = H5Tclose(arraydbl_tid);
    HDassert(status >= 0);

    status = H5Sclose(space);
    HDassert(status >= 0);

    status = H5Dclose(dataset);
    HDassert(status >= 0);

    status = H5Fclose(fid);
    HDassert(status >= 0);

    HDfree(Array1);
}

static void gent_compound_attr_intsizes(void) {
    hid_t fid, attr, space, root;
    hsize_t dims[2];
    hsize_t    array_dim8[]={F70_XDIM,F70_YDIM8}; /* Array dimensions         */
    hsize_t    array_dim16[]={F70_XDIM,F70_YDIM16}; /* Array dimensions         */
    hsize_t    array_dim32[]={F70_XDIM,F70_YDIM32}; /* Array dimensions         */
    hsize_t    array_dim64[]={F70_XDIM,F70_YDIM64}; /* Array dimensions         */
    hid_t      arrayu8_tid;                 /* Array datatype handle    */
    hid_t      arrayu16_tid;                /* Array datatype handle    */
    hid_t      arrayu32_tid;                /* Array datatype handle    */
    hid_t      arrayu64_tid;                /* Array datatype handle    */
    hid_t      array8_tid;                 /* Array datatype handle    */
    hid_t      array16_tid;                /* Array datatype handle    */
    hid_t      array32_tid;                /* Array datatype handle    */
    hid_t      array64_tid;                /* Array datatype handle    */
    hid_t      arraydbl_tid;                /* Array datatype handle    */
    uint8_t  valu8bits;
    uint16_t valu16bits;
    uint32_t valu32bits;
    uint64_t valu64bits;
    int8_t  val8bits;
    int16_t val16bits;
    int32_t val32bits;
    int64_t val64bits;
    /* Structure and array for compound types                             */
    typedef struct Array1Struct {
            uint8_t  dsetu8[F70_XDIM][F70_YDIM8];
            uint16_t dsetu16[F70_XDIM][F70_YDIM16];
            uint32_t dsetu32[F70_XDIM][F70_YDIM32];
            uint64_t dsetu64[F70_XDIM][F70_YDIM64];
            int8_t  dset8[F70_XDIM][F70_YDIM8];
            int16_t dset16[F70_XDIM][F70_YDIM16];
            int32_t dset32[F70_XDIM][F70_YDIM32];
            int64_t dset64[F70_XDIM][F70_YDIM64];
            double  dsetdbl[F70_XDIM][F70_YDIM8];
    } Array1Struct;
    Array1Struct Array1[F70_LENGTH];

    hid_t Array1Structid; /* File datatype identifier */
    herr_t status; /* Error checking variable */
    hsize_t dim[] = { F70_LENGTH }; /* Dataspace dimensions     */

    int m, n, o; /* Array init loop vars     */

    /* Initialize the data in the arrays/datastructure                */
    for (m = 0; m < F70_LENGTH; m++) {

        /* Array of 8 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM8;

        valu8bits = (uint8_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu8[n][0] = valu8bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dsetu8[n][o] = (uint8_t)(Array1[m].dsetu8[n][o-1] << 1);
            }
            valu8bits = (uint8_t)(valu8bits << 1);
        }

        /* Array of 16 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM16;

        valu16bits = (uint16_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu16[n][0] = valu16bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dsetu16[n][o] = (uint16_t)(Array1[m].dsetu16[n][o-1] << 1);
            }
            valu16bits = (uint16_t)(valu16bits << 1);
        }

        /* Array of 32 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM32;

        valu32bits = (uint32_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu32[n][0] = valu32bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dsetu32[n][o] = Array1[m].dsetu32[n][o-1] << 1;
            }
            valu32bits <<= 1;
        }

        /* Array of 64 bits unsigned int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM64;

        valu64bits = (uint64_t) ~0Lu;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dsetu64[n][0] = valu64bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dsetu64[n][o] = Array1[m].dsetu64[n][o-1] << 1;
            }
            valu64bits <<= 1;
        }

        /* Array of 8 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM8;

        val8bits = (int8_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset8[n][0] = val8bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dset8[n][o] = (int8_t)(Array1[m].dset8[n][o-1] << 1);
            }
            val8bits = (int8_t)(val8bits << 1);
        }

        /* Array of 16 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM16;

        val16bits = (int16_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset16[n][0] = val16bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dset16[n][o] = (int16_t)(Array1[m].dset16[n][o-1] << 1);
            }
            val16bits = (int16_t)(val16bits << 1);
        }

        /* Array of 32 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM32;

        val32bits = (int32_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset32[n][0] = val32bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dset32[n][o] = Array1[m].dset32[n][o-1] << 1;
            }
            val32bits <<= 1;
        }

        /* Array of 64 bits signed int */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM64;

        val64bits = (int64_t) ~0L;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Array1[m].dset64[n][0] = val64bits;
            for(o = 1; o < (int)dims[1]; o++) {
                Array1[m].dset64[n][o] = Array1[m].dset64[n][o-1] << 1;
            }
            val64bits <<= 1;
        }

        /* Double Dummy set for failure tests */
        dims[0] = F70_XDIM; dims[1] = F70_YDIM8;

        for(n = 0; n < (int)dims[0]; n++)
            for(o = 0; o < (int)dims[1]; o++)
                Array1[m].dsetdbl[n][o] = 0.0001F * (float)o + (float)n;
    }

    /* Create the array data type for the 8 bits signed int array             */
    array8_tid = H5Tarray_create2(H5T_NATIVE_SCHAR, F70_ARRAY_RANK, array_dim8);
    HDassert(array8_tid >= 0);

    /* Create the array data type for the 16 bits signed int array             */
    array16_tid = H5Tarray_create2(H5T_NATIVE_SHORT, F70_ARRAY_RANK, array_dim16);
    HDassert(array16_tid >= 0);

    /* Create the array data type for the 32 bits signed int array             */
    array32_tid = H5Tarray_create2(H5T_NATIVE_INT, F70_ARRAY_RANK, array_dim32);
    HDassert(array32_tid >= 0);

    /* Create the array data type for the 64 bits signed int array             */
    array64_tid = H5Tarray_create2(H5T_NATIVE_LONG, F70_ARRAY_RANK, array_dim64);
    HDassert(array64_tid >= 0);

    /* Create the array data type for the 8 bits signed int array             */
    arrayu8_tid = H5Tarray_create2(H5T_NATIVE_UCHAR, F70_ARRAY_RANK, array_dim8);
    HDassert(arrayu8_tid >= 0);

    /* Create the array data type for the 16 bits signed int array             */
    arrayu16_tid = H5Tarray_create2(H5T_NATIVE_USHORT, F70_ARRAY_RANK, array_dim16);
    HDassert(arrayu16_tid >= 0);

    /* Create the array data type for the 32 bits signed int array             */
    arrayu32_tid = H5Tarray_create2(H5T_NATIVE_UINT, F70_ARRAY_RANK, array_dim32);
    HDassert(arrayu32_tid >= 0);

    /* Create the array data type for the 64 bits signed int array             */
    arrayu64_tid = H5Tarray_create2(H5T_NATIVE_ULONG, F70_ARRAY_RANK, array_dim64);
    HDassert(arrayu64_tid >= 0);

    /* Create the array data type for the 32 bits double array             */
    arraydbl_tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F70_ARRAY_RANK, array_dim8);
    HDassert(arraydbl_tid >= 0);

    /* Create the dataspace                                           */
    space = H5Screate_simple(F70_RANK, dim, NULL);
    HDassert(space >= 0);

    /* Create the file                                                */
    fid = H5Fcreate(FILE71, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid >= 0);

    /* Create the memory data type                                    */
    Array1Structid = H5Tcreate(H5T_COMPOUND, sizeof(Array1Struct));
    HDassert(Array1Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Array1Structid, F70_DATASETU08, HOFFSET(Array1Struct, dsetu8), arrayu8_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETU16, HOFFSET(Array1Struct, dsetu16), arrayu16_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETU32, HOFFSET(Array1Struct, dsetu32), arrayu32_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETU64, HOFFSET(Array1Struct, dsetu64), arrayu64_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS08, HOFFSET(Array1Struct, dset8), array8_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS16, HOFFSET(Array1Struct, dset16), array16_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS32, HOFFSET(Array1Struct, dset32), array32_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DATASETS64, HOFFSET(Array1Struct, dset64), array64_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Array1Structid, F70_DUMMYDBL, HOFFSET(Array1Struct, dsetdbl), arraydbl_tid);
    HDassert(status >= 0);

    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* Create the Attribute  */
    attr = H5Acreate2(root, F71_DATASETNAME, Array1Structid, space, H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the attribute                                      */
    status = H5Awrite(attr, Array1Structid, Array1);
    HDassert(status >= 0);

    status = H5Tclose(arrayu8_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu16_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu32_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu64_tid);
    HDassert(status >= 0);

    status = H5Tclose(array8_tid);
    HDassert(status >= 0);

    status = H5Tclose(array16_tid);
    HDassert(status >= 0);

    status = H5Tclose(array32_tid);
    HDassert(status >= 0);

    status = H5Tclose(array64_tid);
    HDassert(status >= 0);

    status = H5Tclose(arraydbl_tid);
    HDassert(status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Array1Structid);
    HDassert(status >= 0);

    status = H5Sclose(space);
    HDassert(status >= 0);

    status = H5Aclose(attr);
    HDassert(status >= 0);

    status = H5Fclose(fid);
    HDassert(status >= 0);
}

static void gent_nested_compound_dt(void) {       /* test nested data type */
    hid_t fid, group, dataset, space, type, create_plist, type1, type2;
    hid_t array_dt, enum_dt;
    enumtype val;

    typedef struct {
            int a;
            float b;
    } dset1_t;
    dset1_t dset1[10];

    typedef struct {
            int a;
            float b;
            enumtype c;
    } dset2_t;
    dset2_t dset2[10];

    typedef struct {
            int a[5];
            float b[5][6];
            dset1_t c;
    } dset3_t;
    dset3_t dset3[10];

    enumtype dset4[] = {RED, GREEN, BLUE, GREEN, WHITE, BLUE};

    int i, j, k;
    unsigned ndims;
    hsize_t dim[2];

    hsize_t sdim, maxdim;

    sdim = 10;
    for(i = 0; i < (int)sdim; i++) {
        dset1[i].a = i;
        dset1[i].b = (float)(i*i);

        dset2[i].a = i;
        dset2[i].b = (float)((float)i + (float)i * 0.1F);
        dset2[i].c = GREEN;

        for(j = 0; j < 5; j++) {
            dset3[i].a[j] = i * j;
            for(k = 0; k < 6; k++) {
                dset3[i].b[j][k] = (float)((float)i * (float)j * (float)k * 1.0F);
            }
        }
        dset3[i].c.a = i;
        dset3[i].c.b = (float)((float)i * 1.0F);
    }

    fid = H5Fcreate(FILE72, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    create_plist = H5Pcreate(H5P_DATASET_CREATE);

    sdim = 2;
    H5Pset_chunk(create_plist, 1, &sdim);


    sdim = 6;
    maxdim = H5S_UNLIMITED;

    space = H5Screate_simple(1, &sdim, &maxdim);

    type = H5Tcreate (H5T_COMPOUND, sizeof(dset1[0]));
    H5Tinsert(type, "a_name", HOFFSET(dset1_t, a), H5T_STD_I32LE);
    H5Tinsert(type, "b_name", HOFFSET(dset1_t, b), H5T_IEEE_F32LE);

    dataset = H5Dcreate2(fid, "/dset1", type, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

    H5Tclose(type);
    H5Dclose(dataset);

    /* Create the shared enumerated datatype. */
    enum_dt = H5Tenum_create (H5T_NATIVE_INT);
    val = (enumtype) RED;
    H5Tenum_insert (enum_dt, "Red", &val);
    val = (enumtype) GREEN;
    H5Tenum_insert (enum_dt, "Green", &val);
    val = (enumtype) BLUE;
    H5Tenum_insert (enum_dt, "Blue", &val);
    val = (enumtype) WHITE;
    H5Tenum_insert (enum_dt, "White", &val);
    val = (enumtype) BLACK;
    H5Tenum_insert (enum_dt, "Black", &val);
    H5Tcommit2(fid, "enumtype", enum_dt,  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset2[0]));

    H5Tinsert(type2, "a_name", HOFFSET(dset2_t, a), H5T_NATIVE_INT);
    H5Tinsert(type2, "b_name", HOFFSET(dset2_t, b), H5T_NATIVE_FLOAT);
    H5Tinsert(type2, "c_name", HOFFSET(dset2_t, c), enum_dt);

    dataset = H5Dcreate2(fid, "/dset2", type2, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2);

    H5Tclose(type2);

    dataset = H5Dcreate2(fid, "/dset4", enum_dt, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);
    H5Dwrite(dataset, enum_dt, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset4);

    H5Tclose(enum_dt);
    H5Dclose(dataset);

    /* shared data type 1 */
    type1 = H5Tcreate(H5T_COMPOUND, sizeof(dset1_t));
    H5Tinsert(type1, "int_name", HOFFSET(dset1_t, a), H5T_STD_I32LE);
    H5Tinsert(type1, "float_name", HOFFSET(dset1_t, b), H5T_IEEE_F32LE);
    H5Tcommit2(fid, "type1", type1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    group = H5Gcreate2(fid, "/group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    type2 = H5Tcreate (H5T_COMPOUND, sizeof(dset3_t));

    ndims = 1; dim[0] = 5;
    array_dt = H5Tarray_create2(H5T_STD_I32LE, ndims, dim);
    H5Tinsert(type2, "int_name", HOFFSET(dset3_t, a), array_dt);
    H5Tclose(array_dt);

    ndims = 2; dim[0] = 5; dim[1] = 6;
    array_dt = H5Tarray_create2(H5T_IEEE_F32LE, ndims, dim);
    H5Tinsert(type2, "float_name", HOFFSET(dset3_t, b), array_dt);
    H5Tclose(array_dt);

    H5Tinsert (type2, "cmpd_name", HOFFSET (dset3_t, c), type1);

    dataset = H5Dcreate2(group, "dset3", type2, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);

    H5Dwrite(dataset, type2, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset3);

    dataset = H5Dcreate2(fid, "/dset5", type1, space, H5P_DEFAULT, create_plist, H5P_DEFAULT);
    H5Dwrite(dataset, type1, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);

    H5Tclose(type1);
    H5Tclose(type2);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Gclose(group);

    H5Pclose(create_plist);

    H5Fclose(fid);

}

/*-------------------------------------------------------------------------
 * Function:    gent_intscalars
 *
 * Purpose:     Generate a file to be used in the h5dump scalar tests.
 *   Four datasets of 1, 2, 4 and 8 bytes of unsigned int types are created.
 *   Four more datasets of 1, 2, 4 and 8 bytes of signed int types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *   A dummy dataset of double type is created for failure test.
 *-------------------------------------------------------------------------
 */
static void
gent_intscalars(void)
{
    hid_t fid, dataset, space, tid;
    hsize_t dims[2];
    uint8_t  dsetu8[F73_XDIM][F73_YDIM8],   valu8bits;
    uint16_t dsetu16[F73_XDIM][F73_YDIM16], valu16bits;
    uint32_t dsetu32[F73_XDIM][F73_YDIM32], valu32bits;
    uint64_t dsetu64[F73_XDIM][F73_YDIM64], valu64bits;
    int8_t  dset8[F73_XDIM][F73_YDIM8],   val8bits;
    int16_t dset16[F73_XDIM][F73_YDIM16], val16bits;
    int32_t dset32[F73_XDIM][F73_YDIM32], val32bits;
    int64_t dset64[F73_XDIM][F73_YDIM64], val64bits;
    double  dsetdbl[F73_XDIM][F73_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE73, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Dataset of 8 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U8LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU08, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu8bits = (uint8_t) ~0u;  /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu8[i][0] = valu8bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu8[i][j] = (uint8_t)(dsetu8[i][j-1] << 1);
        }
        valu8bits = (uint8_t)(valu8bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu8);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM16;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U16LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU16, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu16bits = (uint16_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu16[i][0] = valu16bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu16[i][j] = (uint16_t)(dsetu16[i][j-1] << 1);
        }
        valu16bits = (uint16_t)(valu16bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu16);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM32;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U32LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU32, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu32bits = (uint32_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu32[i][0] = valu32bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu32[i][j] = dsetu32[i][j-1] << 1;
        }
        valu32bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu32);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM64;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U64LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU64, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu64bits = (uint64_t) ~0Lu;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu64[i][0] = valu64bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu64[i][j] = dsetu64[i][j-1] << 1;
        }
        valu64bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu64);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 8 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I8LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS08, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val8bits = (int8_t) ~0; /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset8[i][0] = val8bits;
        for(j = 1; j < dims[1]; j++) {
            dset8[i][j] = (int8_t)(dset8[i][j-1] << 1);
        }
        val8bits = (int8_t)(val8bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset8);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM16;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I16LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS16, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val16bits = (int16_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset16[i][0] = val16bits;
        for(j = 1; j < dims[1]; j++) {
            dset16[i][j] = (int16_t)(dset16[i][j-1] << 1);
        }
        val16bits = (int16_t)(val16bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset16);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM32;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I32LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS32, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val32bits = (int32_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset32[i][0] = val32bits;
        for(j = 1; j < dims[1]; j++) {
            dset32[i][j] = dset32[i][j-1] << 1;
        }
        val32bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset32);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM64;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I64LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS64, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val64bits = (int64_t) ~0L;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset64[i][0] = val64bits;
        for(j = 1; j < dims[1]; j++) {
            dset64[i][j] = dset64[i][j-1] << 1;
        }
        val64bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset64);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Double Dummy set for failure tests */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DUMMYDBL, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++)
            dsetdbl[i][j] = 0.0001F * (float)j + (float)i;

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetdbl);

    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_attr_intscalars
 *
 * Purpose:     Generate a file to be used in the h5dump attribute scalar tests.
 *   Four attributes of 1, 2, 4 and 8 bytes of unsigned int types are created.
 *   Four more datasets of 1, 2, 4 and 8 bytes of signed int types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *   A dummy dataset of double type is created for failure test.
 *   Use file to test Signed/Unsigned datatypes and keep in sync with gent_packedbits()
 *-------------------------------------------------------------------------
 */
static void
gent_attr_intscalars(void)
{
    hid_t fid, attr, space, root, tid;
    hsize_t dims[2];
    uint8_t  dsetu8[F73_XDIM][F73_YDIM8],   valu8bits;
    uint16_t dsetu16[F73_XDIM][F73_YDIM16], valu16bits;
    uint32_t dsetu32[F73_XDIM][F73_YDIM32], valu32bits;
    uint64_t dsetu64[F73_XDIM][F73_YDIM64], valu64bits;
    int8_t  dset8[F73_XDIM][F73_YDIM8],   val8bits;
    int16_t dset16[F73_XDIM][F73_YDIM16], val16bits;
    int32_t dset32[F73_XDIM][F73_YDIM32], val32bits;
    int64_t dset64[F73_XDIM][F73_YDIM64], val64bits;
    double  dsetdbl[F73_XDIM][F73_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE74, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* Attribute of 8 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U8LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETU08, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    valu8bits = (uint8_t) ~0u;  /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu8[i][0] = valu8bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu8[i][j] = (uint8_t)(dsetu8[i][j-1] << 1);
        }
        valu8bits = (uint8_t)(valu8bits << 1);
    }

    H5Awrite(attr, tid, dsetu8);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 16 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM16;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U16LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETU16, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    valu16bits = (uint16_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu16[i][0] = valu16bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu16[i][j] = (uint16_t)(dsetu16[i][j-1] << 1);
        }
        valu16bits = (uint16_t)(valu16bits << 1);
    }

    H5Awrite(attr, tid, dsetu16);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 32 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM32;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U32LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETU32, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    valu32bits = (uint32_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu32[i][0] = valu32bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu32[i][j] = dsetu32[i][j-1] << 1;
        }
        valu32bits <<= 1;
    }

    H5Awrite(attr, tid, dsetu32);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 64 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM64;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U64LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETU64, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    valu64bits = (uint64_t) ~0Lu;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu64[i][0] = valu64bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu64[i][j] = dsetu64[i][j-1] << 1;
        }
        valu64bits <<= 1;
    }

    H5Awrite(attr, tid, dsetu64);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 8 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I8LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETS08, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    val8bits = (int8_t) ~0; /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset8[i][0] = val8bits;
        for(j = 1; j < dims[1]; j++) {
            dset8[i][j] = (int8_t)(dset8[i][j-1] << 1);
        }
        val8bits = (int8_t)(val8bits << 1);
    }

    H5Awrite(attr, tid, dset8);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 16 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM16;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I16LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETS16, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    val16bits = (int16_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset16[i][0] = val16bits;
        for(j = 1; j < dims[1]; j++) {
            dset16[i][j] = (int16_t)(dset16[i][j-1] << 1);
        }
        val16bits = (int16_t)(val16bits << 1);
    }

    H5Awrite(attr, tid, dset16);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 32 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM32;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I32LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETS32, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    val32bits = (int32_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset32[i][0] = val32bits;
        for(j = 1; j < dims[1]; j++) {
            dset32[i][j] = dset32[i][j-1] << 1;
        }
        val32bits <<= 1;
    }

    H5Awrite(attr, tid, dset32);
    H5Sclose(space);
    H5Aclose(attr);

    /* Attribute of 64 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM64;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I64LE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DATASETS64, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    val64bits = (int64_t) ~0L;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset64[i][0] = val64bits;
        for(j = 1; j < dims[1]; j++) {
            dset64[i][j] = dset64[i][j-1] << 1;
        }
        val64bits <<= 1;
    }

    H5Awrite(attr, tid, dset64);
    H5Sclose(space);
    H5Aclose(attr);

    /* Double Dummy set for failure tests */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F73_ARRAY_RANK, dims);
    attr = H5Acreate2(root, F73_DUMMYDBL, tid, space, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++)
            dsetdbl[i][j] = 0.0001F * (float)j + (float)i;

    H5Awrite(attr, tid, dsetdbl);

    H5Sclose(space);
    H5Aclose(attr);

    H5Gclose(root);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_string_scalars
 *
 * Purpose:     Generate a file to be used in the h5dump string scalar tests.
 *   A dataset of string types are created.
 *   An attribute of string types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *-------------------------------------------------------------------------
 */
static void
gent_string_scalars(void)
{
    hid_t fid, attr, dataset, space, tid, root;
    hsize_t dims[2];
    char  string[F73_XDIM][F73_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE75, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    root = H5Gopen2(fid, "/", H5P_DEFAULT);

    /* string scalar */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tcopy(H5T_C_S1);
    H5Tset_size(tid, F73_XDIM * F73_YDIM8);

    memset(string, ' ', F73_XDIM * F73_YDIM8);
    for(i = 0; i < dims[0]; i++) {
        string[i][0] = (char)('A' + i);
        for(j = 1; j < dims[1]; j++) {
            string[i][j] = (char)(string[i][j-1] + 1);
        }
    }
    string[dims[0]-1][dims[1]-1] = 0;

    /* Dataset of string scalar */
    dataset = H5Dcreate2(fid, "the_str", tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, string);
    H5Dclose(dataset);

    /* attribute of string scalar */
    attr = H5Acreate2(root, "attr_str", tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, string);

    H5Sclose(space);
    H5Aclose(attr);

    H5Gclose(root);
    H5Fclose(fid);
}

static void gent_compound_int_array(void) {
    hid_t fid, dataset, space;
    hsize_t dims[1];
    uint8_t  valu8bits;
    uint16_t valu16bits;
    uint32_t valu32bits;
    uint64_t valu64bits;
    int8_t  val8bits;
    int16_t val16bits;
    int32_t val32bits;
    int64_t val64bits;
    hsize_t    array_dim8[]={F76_DIM8}; /* Array dimensions         */
    hsize_t    array_dim16[]={F76_DIM16}; /* Array dimensions         */
    hsize_t    array_dim32[]={F76_DIM32}; /* Array dimensions         */
    hsize_t    array_dim64[]={F76_DIM64}; /* Array dimensions         */
    hid_t      arrayu8_tid;                 /* Array datatype handle    */
    hid_t      arrayu16_tid;                /* Array datatype handle    */
    hid_t      arrayu32_tid;                /* Array datatype handle    */
    hid_t      arrayu64_tid;                /* Array datatype handle    */
    hid_t      array8_tid;                 /* Array datatype handle    */
    hid_t      array16_tid;                /* Array datatype handle    */
    hid_t      array32_tid;                /* Array datatype handle    */
    hid_t      array64_tid;                /* Array datatype handle    */
    hid_t      arraydbl_tid;                /* Array datatype handle    */
    /* Structure and array for compound types                             */
    typedef struct Cmpd1Struct {
            uint8_t  dsetu8[F76_DIM8];
            uint16_t dsetu16[F76_DIM16];
            uint32_t dsetu32[F76_DIM32];
            uint64_t dsetu64[F76_DIM64];
            int8_t  dset8[F76_DIM8];
            int16_t dset16[F76_DIM16];
            int32_t dset32[F76_DIM32];
            int64_t dset64[F76_DIM64];
            double  dsetdbl[F76_DIM8];
    } Cmpd1Struct;
    Cmpd1Struct *Cmpd1;

    hid_t Cmpd1Structid; /* File datatype identifier */
    herr_t status; /* Error checking variable */
    hsize_t dim[] = { F76_LENGTH }; /* Dataspace dimensions     */

    int m, n; /* Array init loop vars     */

    /* Allocate buffer */
    Cmpd1 = (Cmpd1Struct *)HDmalloc(sizeof(Cmpd1Struct) * F76_LENGTH);
    HDassert(Cmpd1);

    /* Initialize the data in the arrays/datastructure                */
    for (m = 0; m < F76_LENGTH; m++) {

        /* Array of 8 bits unsigned int */
        dims[0] = F76_DIM8;

        valu8bits = (uint8_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dsetu8[n] = valu8bits;
            valu8bits = (uint8_t)(valu8bits << 1);
        }

        /* Array of 16 bits unsigned int */
        dims[0] = F76_DIM16;

        valu16bits = (uint16_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dsetu16[n] = valu16bits;
            valu16bits = (uint16_t)(valu16bits << 1);
        }

        /* Array of 32 bits unsigned int */
        dims[0] = F76_DIM32;

        valu32bits = (uint32_t) ~0u;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dsetu32[n] = valu32bits;
            valu32bits <<= 1;
        }

        /* Array of 64 bits unsigned int */
        dims[0] = F76_DIM64;

        valu64bits = (uint64_t) ~0Lu;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dsetu64[n] = valu64bits;
            valu64bits <<= 1;
        }

        /* Array of 8 bits signed int */
        dims[0] = F76_DIM8;

        val8bits = (int8_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dset8[n] = val8bits;
            val8bits = (int8_t)(val8bits << 1);
        }

        /* Array of 16 bits signed int */
        dims[0] = F76_DIM16;

        val16bits = (int16_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dset16[n] = val16bits;
            val16bits = (int16_t)(val16bits << 1);
        }

        /* Array of 32 bits signed int */
        dims[0] = F76_DIM32;

        val32bits = (int32_t) ~0;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dset32[n] = val32bits;
            val32bits <<= 1;
        }

        /* Array of 64 bits signed int */
        dims[0] = F76_DIM64;

        val64bits = (int64_t) ~0L;  /* all 1s */
        for(n = 0; n < (int)dims[0]; n++){
            Cmpd1[m].dset64[n] = val64bits;
            val64bits <<= 1;
        }

        /* Double Dummy set for failure tests */
        dims[0] = F76_DIM8;

        for(n = 0; n < (int)dims[0]; n++)
            Cmpd1[m].dsetdbl[n] = 0.0001F + (float)n;
    }

    /* Create the array data type for the 8 bits signed int array             */
    array8_tid = H5Tarray_create2(H5T_NATIVE_SCHAR, F76_ARRAY_RANK, array_dim8);
    HDassert(array8_tid >= 0);

    /* Create the array data type for the 16 bits signed int array             */
    array16_tid = H5Tarray_create2(H5T_NATIVE_SHORT, F76_ARRAY_RANK, array_dim16);
    HDassert(array16_tid >= 0);

    /* Create the array data type for the 32 bits signed int array             */
    array32_tid = H5Tarray_create2(H5T_NATIVE_INT, F76_ARRAY_RANK, array_dim32);
    HDassert(array32_tid >= 0);

    /* Create the array data type for the 64 bits signed int array             */
    array64_tid = H5Tarray_create2(H5T_NATIVE_LONG, F76_ARRAY_RANK, array_dim64);
    HDassert(array64_tid >= 0);

    /* Create the array data type for the 8 bits signed int array             */
    arrayu8_tid = H5Tarray_create2(H5T_NATIVE_UCHAR, F76_ARRAY_RANK, array_dim8);
    HDassert(arrayu8_tid >= 0);

    /* Create the array data type for the 16 bits signed int array             */
    arrayu16_tid = H5Tarray_create2(H5T_NATIVE_USHORT, F76_ARRAY_RANK, array_dim16);
    HDassert(arrayu16_tid >= 0);

    /* Create the array data type for the 32 bits signed int array             */
    arrayu32_tid = H5Tarray_create2(H5T_NATIVE_UINT, F76_ARRAY_RANK, array_dim32);
    HDassert(arrayu32_tid >= 0);

    /* Create the array data type for the 64 bits signed int array             */
    arrayu64_tid = H5Tarray_create2(H5T_NATIVE_ULONG, F76_ARRAY_RANK, array_dim64);
    HDassert(arrayu64_tid >= 0);

    /* Create the array data type for the 32 bits double array             */
    arraydbl_tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F76_ARRAY_RANK, array_dim8);
    HDassert(arraydbl_tid >= 0);

    /* Create the dataspace                                           */
    space = H5Screate_simple(F76_RANK, dim, NULL);
    HDassert(space >= 0);

    /* Create the file                                                */
    fid = H5Fcreate(FILE76, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid >= 0);

    /* Create the memory data type                                    */
    Cmpd1Structid = H5Tcreate(H5T_COMPOUND, sizeof(Cmpd1Struct));
    HDassert(Cmpd1Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Cmpd1Structid, F76_DATASETU08, HOFFSET(Cmpd1Struct, dsetu8), arrayu8_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETU16, HOFFSET(Cmpd1Struct, dsetu16), arrayu16_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETU32, HOFFSET(Cmpd1Struct, dsetu32), arrayu32_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETU64, HOFFSET(Cmpd1Struct, dsetu64), arrayu64_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS08, HOFFSET(Cmpd1Struct, dset8), array8_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS16, HOFFSET(Cmpd1Struct, dset16), array16_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS32, HOFFSET(Cmpd1Struct, dset32), array32_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS64, HOFFSET(Cmpd1Struct, dset64), array64_tid);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DUMMYDBL, HOFFSET(Cmpd1Struct, dsetdbl), arraydbl_tid);
    HDassert(status >= 0);

    /* Create the dataset                                             */
    dataset = H5Dcreate2(fid, F76_DATASETNAME, Cmpd1Structid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the dataset                                      */
    status = H5Dwrite(dataset, Cmpd1Structid, H5S_ALL, H5S_ALL, H5P_DEFAULT, Cmpd1);
    HDassert(status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Cmpd1Structid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu8_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu16_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu32_tid);
    HDassert(status >= 0);

    status = H5Tclose(arrayu64_tid);
    HDassert(status >= 0);

    status = H5Tclose(array8_tid);
    HDassert(status >= 0);

    status = H5Tclose(array16_tid);
    HDassert(status >= 0);

    status = H5Tclose(array32_tid);
    HDassert(status >= 0);

    status = H5Tclose(array64_tid);
    HDassert(status >= 0);

    status = H5Tclose(arraydbl_tid);
    HDassert(status >= 0);

    status = H5Sclose(space);
    HDassert(status >= 0);

    status = H5Dclose(dataset);
    HDassert(status >= 0);

    status = H5Fclose(fid);
    HDassert(status >= 0);

    HDfree(Cmpd1);
}

static void gent_compound_ints(void) {
    hid_t fid, dataset, space;
    uint8_t  valu8bits = (uint8_t) ~0u;  /* all 1s */
    uint16_t valu16bits = (uint16_t) ~0u;  /* all 1s */
    uint32_t valu32bits = (uint32_t) ~0u;  /* all 1s */
    uint64_t valu64bits = (uint64_t) ~0Lu;  /* all 1s */
    int8_t  val8bits = (int8_t) ~0;  /* all 1s */
    int16_t val16bits = (int16_t) ~0;  /* all 1s */
    int32_t val32bits = (int32_t) ~0;  /* all 1s */
    int64_t val64bits = (int64_t) ~0L;  /* all 1s */
    /* Structure and array for compound types                             */
    typedef struct Cmpd1Struct {
            uint8_t  dsetu8;
            uint16_t dsetu16;
            uint32_t dsetu32;
            uint64_t dsetu64;
            int8_t  dset8;
            int16_t dset16;
            int32_t dset32;
            int64_t dset64;
            double  dsetdbl;
    } Cmpd1Struct;
    Cmpd1Struct *Cmpd1;

    typedef struct Cmpd2Struct {
            uint64_t dsetu64;
            uint32_t dsetu32;
            uint16_t dsetu16;
            uint8_t  dsetu8;
            int64_t dset64;
            int32_t dset32;
            int16_t dset16;
            int8_t  dset8;
            double  dsetdbl;
    } Cmpd2Struct;
    Cmpd2Struct *Cmpd2;

    hid_t Cmpd1Structid; /* File datatype identifier */
    hid_t Cmpd2Structid; /* File datatype identifier */
    herr_t status; /* Error checking variable */
    hsize_t dim[] = { F77_LENGTH }; /* Dataspace dimensions     */

    int m; /* Array init loop vars     */

    /* Allocate buffers */
    Cmpd1 = (Cmpd1Struct *)HDmalloc(sizeof(Cmpd1Struct) * F77_LENGTH);
    HDassert(Cmpd1);
    Cmpd2 = (Cmpd2Struct *)HDmalloc(sizeof(Cmpd2Struct) * F77_LENGTH);
    HDassert(Cmpd2);

    /* Initialize the data in the arrays/datastructure                */
    for (m = 0; m < F77_LENGTH; m++) {

        /* Array of 8 bits unsigned int */
        if((m % F76_DIM8) == 0)
            valu8bits = (uint8_t) ~0u;  /* all 1s */
        Cmpd1[m].dsetu8 = valu8bits;
        Cmpd2[m].dsetu8 = valu8bits;
        valu8bits = (uint8_t)(valu8bits << 1);

        /* Array of 16 bits unsigned int */
        if((m % F76_DIM16) == 0)
            valu16bits = (uint16_t) ~0u;  /* all 1s */
        Cmpd1[m].dsetu16 = valu16bits;
        Cmpd2[m].dsetu16 = valu16bits;
        valu16bits = (uint16_t)(valu16bits << 1);

        /* Array of 32 bits unsigned int */
        if((m % F76_DIM32) == 0)
            valu32bits = (uint32_t) ~0u;  /* all 1s */
        Cmpd1[m].dsetu32 = valu32bits;
        Cmpd2[m].dsetu32 = valu32bits;
        valu32bits <<= 1;

        /* Array of 64 bits unsigned int */
        if((m % F76_DIM64) == 0)
            valu64bits = (uint64_t) ~0Lu;  /* all 1s */
        Cmpd1[m].dsetu64 = valu64bits;
        Cmpd2[m].dsetu64 = valu64bits;
        valu64bits <<= 1;

        /* Array of 8 bits signed int */
        if((m % F76_DIM8) == 0)
            val8bits = (int8_t) ~0;  /* all 1s */
        Cmpd1[m].dset8 = val8bits;
        Cmpd2[m].dset8 = val8bits;
        val8bits = (int8_t)(val8bits << 1);

        /* Array of 16 bits signed int */
        if((m % F76_DIM16) == 0)
            val16bits = (int16_t) ~0;  /* all 1s */
        Cmpd1[m].dset16 = val16bits;
        Cmpd2[m].dset16 = val16bits;
        val16bits = (int16_t)(val16bits << 1);

        /* Array of 32 bits signed int */
        if((m % F76_DIM32) == 0)
            val32bits = (int32_t) ~0;  /* all 1s */
        Cmpd1[m].dset32 = val32bits;
        Cmpd2[m].dset32 = val32bits;
        val32bits <<= 1;

        /* Array of 64 bits signed int */
        if((m % F76_DIM64) == 0)
            val64bits = (int64_t) ~0L;  /* all 1s */
        Cmpd1[m].dset64 = val64bits;
        Cmpd2[m].dset64 = val64bits;
        val64bits <<= 1;

        /* Double Dummy set for failure tests */
        Cmpd1[m].dsetdbl = 0.0001F + (float)m;
        Cmpd2[m].dsetdbl = 0.0001F + (float)m;
    }

    /* Create the dataspace                                           */
    space = H5Screate_simple(F76_RANK, dim, NULL);
    HDassert(space >= 0);

    /* Create the file                                                */
    fid = H5Fcreate(FILE77, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid >= 0);

    /* Create the memory data type                                    */
    Cmpd1Structid = H5Tcreate(H5T_COMPOUND, sizeof(Cmpd1Struct));
    HDassert(Cmpd1Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Cmpd1Structid, F76_DATASETU08, HOFFSET(Cmpd1Struct, dsetu8), H5T_NATIVE_UCHAR);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETU16, HOFFSET(Cmpd1Struct, dsetu16), H5T_NATIVE_USHORT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETU32, HOFFSET(Cmpd1Struct, dsetu32), H5T_NATIVE_UINT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETU64, HOFFSET(Cmpd1Struct, dsetu64), H5T_NATIVE_ULONG);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS08, HOFFSET(Cmpd1Struct, dset8), H5T_NATIVE_SCHAR);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS16, HOFFSET(Cmpd1Struct, dset16), H5T_NATIVE_SHORT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS32, HOFFSET(Cmpd1Struct, dset32), H5T_NATIVE_INT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DATASETS64, HOFFSET(Cmpd1Struct, dset64), H5T_NATIVE_LONG);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd1Structid, F76_DUMMYDBL, HOFFSET(Cmpd1Struct, dsetdbl), H5T_NATIVE_DOUBLE);
    HDassert(status >= 0);

    /* Create the dataset                                             */
    dataset = H5Dcreate2(fid, F77_DATASETNAME1, Cmpd1Structid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the dataset                                      */
    status = H5Dwrite(dataset, Cmpd1Structid, H5S_ALL, H5S_ALL, H5P_DEFAULT, Cmpd1);
    HDassert(status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Cmpd1Structid);
    HDassert(status >= 0);

    status = H5Sclose(space);
    HDassert(status >= 0);

    status = H5Dclose(dataset);
    HDassert(status >= 0);

    /* Create the dataspace                                           */
    space = H5Screate_simple(F76_RANK, dim, NULL);
    HDassert(space >= 0);

    /* Create the memory data type                                    */
    Cmpd2Structid = H5Tcreate(H5T_COMPOUND, sizeof(Cmpd2Struct));
    HDassert(Cmpd2Structid >= 0);

    /* Insert the arrays and variables into the structure             */
    status = H5Tinsert(Cmpd2Structid, F76_DATASETU64, HOFFSET(Cmpd2Struct, dsetu64), H5T_NATIVE_ULONG);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETU32, HOFFSET(Cmpd2Struct, dsetu32), H5T_NATIVE_UINT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETU16, HOFFSET(Cmpd2Struct, dsetu16), H5T_NATIVE_USHORT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETU08, HOFFSET(Cmpd2Struct, dsetu8), H5T_NATIVE_UCHAR);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETS64, HOFFSET(Cmpd2Struct, dset64), H5T_NATIVE_LONG);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETS32, HOFFSET(Cmpd2Struct, dset32), H5T_NATIVE_INT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETS16, HOFFSET(Cmpd2Struct, dset16), H5T_NATIVE_SHORT);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DATASETS08, HOFFSET(Cmpd2Struct, dset8), H5T_NATIVE_SCHAR);
    HDassert(status >= 0);

    status = H5Tinsert(Cmpd2Structid, F76_DUMMYDBL, HOFFSET(Cmpd2Struct, dsetdbl), H5T_NATIVE_DOUBLE);
    HDassert(status >= 0);

    /* Create the dataset                                             */
    dataset = H5Dcreate2(fid, F77_DATASETNAME2, Cmpd2Structid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* Write data to the dataset                                      */
    status = H5Dwrite(dataset, Cmpd2Structid, H5S_ALL, H5S_ALL, H5P_DEFAULT, Cmpd2);
    HDassert(status >= 0);

    /* Release resources                                              */
    status = H5Tclose(Cmpd2Structid);
    HDassert(status >= 0);

    status = H5Sclose(space);
    HDassert(status >= 0);

    status = H5Dclose(dataset);
    HDassert(status >= 0);

    status = H5Fclose(fid);
    HDassert(status >= 0);

    HDfree(Cmpd1);
    HDfree(Cmpd2);
}

/*-------------------------------------------------------------------------
 * Function:    gent_intscalars
 *
 * Purpose:     Generate a file to be used in the h5dump scalar with attribute tests.
 *   Four datasets of 1, 2, 4 and 8 bytes of unsigned int types are created.
 *   Four more datasets of 1, 2, 4 and 8 bytes of signed int types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *   A dummy dataset of double type is created for failure test.
 *-------------------------------------------------------------------------
 */
static void
gent_intattrscalars(void)
{
    hid_t fid, attr, dataset, space, tid;
    hsize_t dims[2];
    uint8_t  dsetu8[F73_XDIM][F73_YDIM8],   valu8bits;
    uint16_t dsetu16[F73_XDIM][F73_YDIM16], valu16bits;
    uint32_t dsetu32[F73_XDIM][F73_YDIM32], valu32bits;
    uint64_t dsetu64[F73_XDIM][F73_YDIM64], valu64bits;
    int8_t  dset8[F73_XDIM][F73_YDIM8],   val8bits;
    int16_t dset16[F73_XDIM][F73_YDIM16], val16bits;
    int32_t dset32[F73_XDIM][F73_YDIM32], val32bits;
    int64_t dset64[F73_XDIM][F73_YDIM64], val64bits;
    double  dsetdbl[F73_XDIM][F73_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE78, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Dataset of 8 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U8LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU08, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu8bits = (uint8_t) ~0u;  /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu8[i][0] = valu8bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu8[i][j] = (uint8_t)(dsetu8[i][j-1] << 1);
        }
        valu8bits = (uint8_t)(valu8bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu8);
    /* Attribute of 8 bits unsigned int */
    attr = H5Acreate2(dataset, F73_DATASETU08, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dsetu8);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM16;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U16LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU16, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu16bits = (uint16_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu16[i][0] = valu16bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu16[i][j] = (uint16_t)(dsetu16[i][j-1] << 1);
        }
        valu16bits = (uint16_t)(valu16bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu16);
    /* Attribute of 16 bits unsigned int */
    attr = H5Acreate2(dataset, F73_DATASETU16, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dsetu16);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM32;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U32LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU32, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu32bits = (uint32_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu32[i][0] = valu32bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu32[i][j] = dsetu32[i][j-1] << 1;
        }
        valu32bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu32);
    /* Attribute of 32 bits unsigned int */
    attr = H5Acreate2(dataset, F73_DATASETU32, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dsetu32);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits unsigned int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM64;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_U64LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETU64, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu64bits = (uint64_t) ~0Lu;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu64[i][0] = valu64bits;
        for(j = 1; j < dims[1]; j++) {
            dsetu64[i][j] = dsetu64[i][j-1] << 1;
        }
        valu64bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu64);
    /* Attribute of 64 bits unsigned int */
    attr = H5Acreate2(dataset, F73_DATASETU64, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dsetu64);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 8 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I8LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS08, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val8bits = (int8_t) ~0; /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset8[i][0] = val8bits;
        for(j = 1; j < dims[1]; j++) {
            dset8[i][j] = (int8_t)(dset8[i][j-1] << 1);
        }
        val8bits = (int8_t)(val8bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset8);
    /* Attribute of 8 bits signed int */
    attr = H5Acreate2(dataset, F73_DATASETS08, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dset8);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM16;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I16LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS16, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val16bits = (int16_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset16[i][0] = val16bits;
        for(j = 1; j < dims[1]; j++) {
            dset16[i][j] = (int16_t)(dset16[i][j-1] << 1);
        }
        val16bits = (int16_t)(val16bits << 1);
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset16);
    /* Attribute of 16 bits signed int */
    attr = H5Acreate2(dataset, F73_DATASETS16, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dset16);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM32;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I32LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS32, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val32bits = (int32_t) ~0;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset32[i][0] = val32bits;
        for(j = 1; j < dims[1]; j++) {
            dset32[i][j] = dset32[i][j-1] << 1;
        }
        val32bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset32);
    /* Attribute of 32 bits signed int */
    attr = H5Acreate2(dataset, F73_DATASETS32, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dset32);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits signed int */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM64;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_STD_I64LE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DATASETS64, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val64bits = (int64_t) ~0L;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset64[i][0] = val64bits;
        for(j = 1; j < dims[1]; j++) {
            dset64[i][j] = dset64[i][j-1] << 1;
        }
        val64bits <<= 1;
    }

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset64);
    /* Attribute of 64 bits signed int */
    attr = H5Acreate2(dataset, F73_DATASETS64, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dset64);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Double Dummy set for failure tests */
    dims[0] = F73_XDIM; dims[1] = F73_YDIM8;
    space = H5Screate(H5S_SCALAR);
    tid = H5Tarray_create2(H5T_NATIVE_DOUBLE, F73_ARRAY_RANK, dims);
    dataset = H5Dcreate2(fid, F73_DUMMYDBL, tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++)
            dsetdbl[i][j] = 0.0001F * (float)j + (float)i;

    H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetdbl);
    /* Attribute of double */
    attr = H5Acreate2(dataset, F73_DUMMYDBL, tid, space, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, tid, dsetdbl);
    H5Aclose(attr);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_packedbits
 *
 * Purpose:     Generate a file to be used in the h5dump packed bits tests.
 *   Four datasets of 1, 2, 4 and 8 bytes of unsigned int types are created.
 *   Four more datasets of 1, 2, 4 and 8 bytes of signed int types are created.
 *   Fill them with raw data such that no bit will be all zero in a dataset.
 *   A dummy dataset of double type is created for failure test.
 * Created:  Albert Cheng, 2010/5/10.
 * Modified: Allen Byrne, 2011/1/5 Use file to test Signed/Unsigned datatypes
 *-------------------------------------------------------------------------
 */
static void
gent_intsattrs(void)
{
    hid_t fid, attr, dataset, space, aspace;
    hsize_t dims[2], adims[1];
    uint8_t  dsetu8[F66_XDIM][F66_YDIM8],   asetu8[F66_XDIM*F66_YDIM8],   valu8bits;
    uint16_t dsetu16[F66_XDIM][F66_YDIM16], asetu16[F66_XDIM*F66_YDIM16], valu16bits;
    uint32_t dsetu32[F66_XDIM][F66_YDIM32], asetu32[F66_XDIM*F66_YDIM32], valu32bits;
    uint64_t dsetu64[F66_XDIM][F66_YDIM64], asetu64[F66_XDIM*F66_YDIM64], valu64bits;
    int8_t  dset8[F66_XDIM][F66_YDIM8],   aset8[F66_XDIM*F66_YDIM8],   val8bits;
    int16_t dset16[F66_XDIM][F66_YDIM16], aset16[F66_XDIM*F66_YDIM16], val16bits;
    int32_t dset32[F66_XDIM][F66_YDIM32], aset32[F66_XDIM*F66_YDIM32], val32bits;
    int64_t dset64[F66_XDIM][F66_YDIM64], aset64[F66_XDIM*F66_YDIM64], val64bits;
    double  dsetdbl[F66_XDIM][F66_YDIM8], asetdbl[F66_XDIM*F66_YDIM8];
    unsigned int i, j;

    fid = H5Fcreate(FILE79, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Dataset of 8 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU08, H5T_STD_U8LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu8bits = (uint8_t) ~0u;  /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu8[i][0] = valu8bits;
        asetu8[i*dims[1]] = dsetu8[i][0];
        for(j = 1; j < dims[1]; j++) {
            dsetu8[i][j] = (uint8_t)(dsetu8[i][j-1] << 1);
            asetu8[i*dims[1]+j] = dsetu8[i][j];
        }
        valu8bits = (uint8_t)(valu8bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu8);
    /* Attribute of 8 bits unsigned int */
    adims[0] = F66_XDIM * F66_YDIM8;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETU08, H5T_STD_U8LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_UINT8, asetu8);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM16;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU16, H5T_STD_U16LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu16bits = (uint16_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu16[i][0] = valu16bits;
        asetu16[i*dims[1]] = dsetu16[i][0];
        for(j = 1; j < dims[1]; j++) {
            dsetu16[i][j] = (uint16_t)(dsetu16[i][j-1] << 1);
            asetu16[i*dims[1]+j] = dsetu16[i][j];
        }
        valu16bits = (uint16_t)(valu16bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu16);
    /* Attribute of 16 bits unsigned int */
    adims[0] = F66_XDIM * F66_YDIM16;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETU16, H5T_STD_U16LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_UINT16, asetu16);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM32;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU32, H5T_STD_U32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu32bits = (uint32_t) ~0u;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu32[i][0] = valu32bits;
        asetu32[i*dims[1]] = dsetu32[i][0];
        for(j = 1; j < dims[1]; j++) {
            dsetu32[i][j] = dsetu32[i][j-1] << 1;
            asetu32[i*dims[1]+j] = dsetu32[i][j];
        }
        valu32bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu32);
    /* Attribute of 32 bits unsigned int */
    adims[0] = F66_XDIM * F66_YDIM32;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETU32, H5T_STD_U32LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_UINT32, asetu32);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits unsigned int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM64;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETU64, H5T_STD_U64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    valu64bits = (uint64_t) ~0Lu;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dsetu64[i][0] = valu64bits;
        asetu64[i*dims[1]] = dsetu64[i][0];
        for(j = 1; j < dims[1]; j++) {
            dsetu64[i][j] = dsetu64[i][j-1] << 1;
            asetu64[i*dims[1]+j] = dsetu64[i][j];
        }
        valu64bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetu64);
    /* Attribute of 64 bits unsigned int */
    adims[0] = F66_XDIM * F66_YDIM64;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETU64, H5T_STD_U64LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_UINT64, asetu64);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 8 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS08, H5T_STD_I8LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val8bits = (int8_t) ~0;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset8[i][0] = val8bits;
        aset8[i*dims[1]] = dset8[i][0];
        for(j = 1; j < dims[1]; j++) {
            dset8[i][j] = (int8_t)(dset8[i][j-1] << 1);
            aset8[i*dims[1]+j] = dset8[i][j];
        }
        val8bits = (int8_t)(val8bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_INT8, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset8);
    /* Attribute of 8 bits signed int */
    adims[0] = F66_XDIM * F66_YDIM8;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETS08, H5T_STD_I8LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT8, aset8);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 16 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM16;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS16, H5T_STD_I16LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val16bits = (int16_t) ~0;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset16[i][0] = val16bits;
        aset16[i*dims[1]] = dset16[i][0];
        for(j = 1; j < dims[1]; j++) {
            dset16[i][j] = (int16_t)(dset16[i][j-1] << 1);
            aset16[i*dims[1]+j] = dset16[i][j];
        }
        val16bits = (int16_t)(val16bits << 1);
    }

    H5Dwrite(dataset, H5T_NATIVE_INT16, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset16);
    /* Attribute of 16 bits signed int */
    adims[0] = F66_XDIM * F66_YDIM16;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETS16, H5T_STD_I16LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT16, aset16);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 32 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM32;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS32, H5T_STD_I32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val32bits = (int32_t) ~0;    /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset32[i][0] = val32bits;
        aset32[i*dims[1]] = dset32[i][0];
        for(j = 1; j < dims[1]; j++) {
            dset32[i][j] = dset32[i][j-1] << 1;
            aset32[i*dims[1]+j] = dset32[i][j];
        }
        val32bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset32);
    /* Attribute of 32 bits signed int */
    adims[0] = F66_XDIM * F66_YDIM32;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETS32, H5T_STD_I32LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT32, aset32);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Dataset of 64 bits signed int */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM64;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DATASETS64, H5T_STD_I64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    val64bits = (int64_t) ~0L;   /* all 1s */
    for(i = 0; i < dims[0]; i++){
        dset64[i][0] = val64bits;
        aset64[i*dims[1]] = dset64[i][0];
        for(j = 1; j < dims[1]; j++) {
            dset64[i][j] = dset64[i][j-1] << 1;
            aset64[i*dims[1]+j] = dset64[i][j];
        }
        val64bits <<= 1;
    }

    H5Dwrite(dataset, H5T_NATIVE_INT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset64);
    /* Attribute of 64 bits signed int */
    adims[0] = F66_XDIM * F66_YDIM64;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DATASETS64, H5T_STD_I64LE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_INT64, aset64);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);

    /* Double Dummy set for failure tests */
    dims[0] = F66_XDIM; dims[1] = F66_YDIM8;
    space = H5Screate_simple(2, dims, NULL);
    dataset = H5Dcreate2(fid, F66_DUMMYDBL, H5T_IEEE_F64BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < dims[0]; i++)
        for(j = 0; j < dims[1]; j++) {
            dsetdbl[i][j] = 0.0001F * (float)j + (float)i;
            asetdbl[i*dims[1]+j] = dsetdbl[i][j];
        }

    H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dsetdbl);
    /* Attribute of double */
    adims[0] = F66_XDIM * F66_YDIM8;
    aspace = H5Screate_simple(1, adims, NULL);
    attr = H5Acreate2(dataset, F66_DUMMYDBL, H5T_IEEE_F64BE, aspace, H5P_DEFAULT, H5P_DEFAULT);
    H5Awrite(attr, H5T_NATIVE_DOUBLE, asetdbl);
    H5Aclose(attr);
    H5Sclose(aspace);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(fid);
}

static void gent_bitnopaquefields(void)
{
    /* Compound datatype */
    typedef struct s_t
    {
        unsigned char   a;
        unsigned int    b;
        unsigned long    c;
        unsigned long long    d;
    } s_t;
    hid_t  file, grp=-1, type=-1, space=-1, dset=-1;
    size_t  i;
    hsize_t  nelmts = F80_DIM32;
    unsigned char buf[F80_DIM32];    /* bitfield, opaque */
    unsigned int buf2[F80_DIM32];    /* bitfield, opaque */
    unsigned long buf3[F80_DIM32];    /* bitfield, opaque */
    unsigned long long buf4[F80_DIM32];    /* bitfield, opaque */
    s_t      buf5[F80_DIM32];        /* compound */

    file = H5Fcreate(FILE80, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    if ((grp = H5Gcreate2(file, "bittypetests", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
        /* bitfield_1 */
        if ((type = H5Tcopy(H5T_STD_B8LE)) >= 0) {
            if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                if ((dset = H5Dcreate2(grp, "bitfield_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    for (i = 0; i < nelmts; i++) {
                        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
                    }
                    H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
                    H5Dclose(dset);
                }
                H5Sclose(space);
            }
            H5Tclose(type);
        }

        /* bitfield_2 */
        if ((type = H5Tcopy(H5T_STD_B16LE)) >= 0) {
            if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                if ((dset = H5Dcreate2(grp, "bitfield_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    for (i = 0; i < nelmts; i++) {
                        buf2[i] = (unsigned int)0xffff ^ (unsigned int)(i * 16);
                    }
                    H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2);
                    H5Dclose(dset);
                }
                H5Sclose(space);
            }
            H5Tclose(type);
        }

        /* bitfield_3 */
        if ((type = H5Tcopy(H5T_STD_B32LE)) >= 0) {
            if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                if ((dset = H5Dcreate2(grp, "bitfield_3", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    for (i = 0; i < nelmts; i++) {
                        buf3[i] = (unsigned long)0xffffffff ^ (unsigned long)(i * 32);
                    }
                    H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf3);
                    H5Dclose(dset);
                }
                H5Sclose(space);
            }
            H5Tclose(type);
        }

        /* bitfield_4 */
        if ((type = H5Tcopy(H5T_STD_B64LE)) >= 0) {
            if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                if ((dset = H5Dcreate2(grp, "bitfield_4", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    for (i = 0; i < nelmts; i++) {
                        buf4[i] = (unsigned long long)0xffffffffffffffff ^ (unsigned long long)(i * 64);
                    }
                    H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf4);
                    H5Dclose(dset);
                }
                H5Sclose(space);
            }
            H5Tclose(type);
        }

        H5Gclose(grp);
    }

    if ((grp = H5Gcreate2(file, "opaquetypetests", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
        /* opaque_1 */
        if ((type = H5Tcreate(H5T_OPAQUE, 1)) >= 0) {
            if ((H5Tset_tag(type, "1-byte opaque type")) >= 0) {
                if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                    if ((dset = H5Dcreate2(grp, "opaque_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                        for(i = 0; i < nelmts; i++)
                            buf[i] = (unsigned char)0xff ^ (unsigned char)i;
                        H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
                        H5Dclose(dset);
                    }
                    H5Sclose(space);
                }
            }
            H5Tclose(type);
        }

        /* opaque_2 */
        if ((type = H5Tcreate(H5T_OPAQUE, 2)) >= 0) {
            if ((H5Tset_tag(type, "2-byte opaque type")) >= 0) {
                if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                    if ((dset = H5Dcreate2(grp, "opaque_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                        for(i = 0; i < nelmts; i++)
                            buf2[i] = (unsigned int)0xffff ^ (unsigned int)(i * 16);

                        H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2);
                        H5Dclose(dset);
                    }
                    H5Sclose(space);
                }
            }
            H5Tclose(type);
        }
        H5Gclose(grp);
    }

    if ((grp = H5Gcreate2(file, "cmpdtypetests", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
        /* compound_1 */
        if ((type = H5Tcreate(H5T_COMPOUND, sizeof(s_t))) >= 0) {
            H5Tinsert(type, "a", HOFFSET(s_t, a), H5T_STD_B8LE);
            H5Tinsert(type, "b", HOFFSET(s_t, b), H5T_STD_B16LE);
            H5Tinsert(type, "c", HOFFSET(s_t, c), H5T_STD_B32LE);
            H5Tinsert(type, "d", HOFFSET(s_t, d), H5T_STD_B64LE);
            if ((space = H5Screate_simple(1, &nelmts, NULL)) >= 0) {
                if ((dset = H5Dcreate2(grp, "compound_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    for(i = 0; i < nelmts; i++) {
                        buf5[i].a = (unsigned char)0xff ^ (unsigned char)i;
                        buf5[i].b = (unsigned int)0xffff ^ (unsigned int)(i * 16);
                        buf5[i].c = (unsigned long)0xffffffff ^ (unsigned long)(i * 32);
                        buf5[i].d = (unsigned long long)0xffffffffffffffff ^ (unsigned long long)(i * 64);
                    }

                    H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf5);
                    H5Dclose(dset);
                }
                H5Sclose(space);
            }
            H5Tclose(type);
        }
        H5Gclose(grp);
    }

    H5Fclose(file);
}

/*-------------------------------------------------------------------------
 * Function:    gent_intsfourdims
 *
 * Purpose:     Generate a file to be used in the h5dump subsetting tests.
 *   One datasets of unsigned int types are created in four dimensions 2,4,6,10.
 *-------------------------------------------------------------------------
 */
static void
gent_intsfourdims(void)
{
    hid_t fid, dataset, space;
    hsize_t dims[F81_RANK];
    uint32_t dset1[F81_ZDIM][F81_YDIM][F81_XDIM][F81_WDIM];
    unsigned int i, j, k, l;

    fid = H5Fcreate(FILE81, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Dataset of 32 bits unsigned int */
    dims[0] = F81_ZDIM; dims[1] = F81_YDIM; dims[2] = F81_XDIM; dims[3] = F81_WDIM;
    space = H5Screate_simple(F81_RANK, dims, NULL);
    dataset = H5Dcreate2(fid, F81_DATASETNAME, H5T_STD_U32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    for(i = 0; i < F81_ZDIM; i++)
        for(j = 0; j < F81_YDIM; j++)
            for(k = 0; k < F81_XDIM; k++)
                for(l = 0; l < F81_WDIM; l++)
            dset1[i][j][k][l] = i*F81_YDIM*F81_XDIM*F81_WDIM + j*F81_XDIM*F81_WDIM + k*F81_WDIM + l;

    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1);
    H5Sclose(space);
    H5Dclose(dataset);

    H5Fclose(fid);
}

/*-------------------------------------------------------------------------
 * Function:    gent_compound_complex2
 *
 * Purpose:     Generate a file to be used in testing compound datatypes of
 *   various sizes, dimensions, member types and nesting.
 *-------------------------------------------------------------------------
 */
static void gent_compound_complex2(void)
{
    /* Third-level nested compound */
    typedef struct {
        short  deep_nested_short[10];
        int    deep_nested_int[10];
        long   deep_nested_long[10];
        double deep_nested_double[10];
        float  deep_nested_float[10];
    } third_level_compound;

    /* Second-level multiply-nested compounds */
    typedef struct {
        unsigned int  multiple_nested_a[5];
        int           multiple_nested_b[5];
        unsigned long multiple_nested_c[5];
        long          multiple_nested_d[5];
    } further_nested;

    typedef struct {
        char further_nested_string[11];
        char further_nested_string_array[4][13];
        third_level_compound deep_nest;
    } further_nested2;

    /* First First-level nested compound */
    typedef struct
    {
        double nested_a;
        char   nested_string[23];
        char   nested_string_array[4][12];
    } nested_compound;

    /* Second First-level nested compound */
    typedef struct
    {
        float           a;
        further_nested  b;
        further_nested2 c;
    } multiple_nested_compound;

    /* Compound datatype with different member types */
    typedef struct
    {
        /* Arrays nested inside compound */
        unsigned int             a[4];
        int                      b[6];
        float                    c[2][4];
        nested_compound          d;   /* Compound inside compound */
        multiple_nested_compound e;   /* Compound inside compound with further nested compound */
    } compound;

    compound        *buf;        /* compound */

    hid_t file, type=-1, space=-1, dset=-1;
    hid_t dset_array_a, dset_array_b, dset_array_c;
    hid_t cmpd_tid1 = -1, cmpd_tid2 = -1, cmpd_tid3 = -1;
    size_t  i;
    size_t j, k;
    unsigned dset_array_ndims;
    hsize_t  dset_array_a_dims[1], dset_array_b_dims[1], dset_array_c_dims[2];
    hsize_t  nelmts = F82_DIM32;

    /* Allocate buffer */
    buf = (compound *)HDmalloc(sizeof(compound) * F82_DIM32);
    HDassert(buf);

    file = H5Fcreate(FILE82, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    if ((space = H5Screate_simple(F82_RANK, &nelmts, NULL)) >= 0) {
        /* CompoundComplex1D */
        if ((type = H5Tcreate(H5T_COMPOUND, sizeof(compound))) >= 0) {
            hid_t str_type, array;
            hsize_t dims[1];
            hid_t nest1, nest2;

            /* Insert top-level array members */
            dset_array_ndims = 1; dset_array_a_dims[0] = 4;
            dset_array_a = H5Tarray_create2(H5T_STD_U32LE, dset_array_ndims, dset_array_a_dims);
            H5Tinsert(type, "a", HOFFSET(compound, a), dset_array_a);
            H5Tclose(dset_array_a);

            dset_array_ndims = 1; dset_array_b_dims[0] = 6;
            dset_array_b = H5Tarray_create2(H5T_STD_I32LE, dset_array_ndims, dset_array_b_dims);
            H5Tinsert(type, "b", HOFFSET(compound, b), dset_array_b);
            H5Tclose(dset_array_b);

            dset_array_ndims = 2; dset_array_c_dims[0] = 2; dset_array_c_dims[1] = 4;
            dset_array_c = H5Tarray_create2(H5T_IEEE_F32LE, dset_array_ndims, dset_array_c_dims);
            H5Tinsert(type, "c", HOFFSET(compound, c), dset_array_c);
            H5Tclose(dset_array_c);

            /* Insert first nested compound */
            cmpd_tid1 = H5Tcreate(H5T_COMPOUND, sizeof(nested_compound));

            H5Tinsert(cmpd_tid1, "nested_double", HOFFSET(nested_compound, nested_a), H5T_IEEE_F64LE);

            dims[0] = 1;
            str_type = mkstr(23, H5T_STR_NULLTERM);
            array = H5Tarray_create2(str_type, 1, dims);
            H5Tinsert(cmpd_tid1, "nested_string", HOFFSET(nested_compound, nested_string), array);
            H5Tclose(array);
            H5Tclose(str_type);

            dims[0] = 4;
            str_type = mkstr(12, H5T_STR_NULLTERM);
            array = H5Tarray_create2(str_type, 1, dims);
            H5Tinsert(cmpd_tid1, "nested_string_array", HOFFSET(nested_compound, nested_string_array), array);
            H5Tclose(array);
            H5Tclose(str_type);

            H5Tinsert(type, "nested_compound", HOFFSET(compound, d), cmpd_tid1);

            /* Insert second nested compound */
            cmpd_tid2 = H5Tcreate(H5T_COMPOUND, sizeof(multiple_nested_compound));

            H5Tinsert(cmpd_tid2, "nested_float", HOFFSET(multiple_nested_compound, a), H5T_IEEE_F32LE);

            /* Add first further nested compound */
            nest1 = H5Tcreate(H5T_COMPOUND, sizeof(further_nested));

            dims[0] = 5;
            array = H5Tarray_create2(H5T_STD_U32LE, 1, dims);
            H5Tinsert(nest1, "nested_unsigned_int", HOFFSET(further_nested, multiple_nested_a), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_STD_I32LE, 1, dims);
            H5Tinsert(nest1, "nested_int", HOFFSET(further_nested, multiple_nested_b), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_STD_U64LE, 1, dims);
            H5Tinsert(nest1, "nested_unsigned_long", HOFFSET(further_nested, multiple_nested_c), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_STD_I64LE, 1, dims);
            H5Tinsert(nest1, "nested_long", HOFFSET(further_nested, multiple_nested_d), array);
            H5Tclose(array);

            H5Tinsert(cmpd_tid2, "further_nested_compoundA", HOFFSET(multiple_nested_compound, b), nest1);
            H5Tclose(nest1);

            /* Add second further nested compound */
            nest2 = H5Tcreate(H5T_COMPOUND, sizeof(further_nested2));

            dims[0] = 1;
            str_type = mkstr(11, H5T_STR_NULLTERM);
            array = H5Tarray_create2(str_type, 1, dims);
            H5Tinsert(nest2, "nested_string", HOFFSET(further_nested2, further_nested_string), array);
            H5Tclose(array);
            H5Tclose(str_type);

            dims[0] = 4;
            str_type = mkstr(13, H5T_STR_NULLTERM);
            array = H5Tarray_create2(str_type, 1, dims);
            H5Tinsert(nest2, "nested_string_array", HOFFSET(further_nested2, further_nested_string_array), array);
            H5Tclose(array);
            H5Tclose(str_type);

            /* Add a final third-level nested compound to this second-level compound */
            cmpd_tid3 = H5Tcreate(H5T_COMPOUND, sizeof(third_level_compound));

            dims[0] = 10;
            array = H5Tarray_create2(H5T_STD_I16LE, 1, dims);
            H5Tinsert(cmpd_tid3, "deep_nested_short", HOFFSET(third_level_compound, deep_nested_short), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_STD_I32LE, 1, dims);
            H5Tinsert(cmpd_tid3, "deep_nested_int", HOFFSET(third_level_compound, deep_nested_int), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_STD_I64LE, 1, dims);
            H5Tinsert(cmpd_tid3, "deep_nested_long", HOFFSET(third_level_compound, deep_nested_long), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_IEEE_F64LE, 1, dims);
            H5Tinsert(cmpd_tid3, "deep_nested_double", HOFFSET(third_level_compound, deep_nested_double), array);
            H5Tclose(array);

            array = H5Tarray_create2(H5T_IEEE_F32LE, 1, dims);
            H5Tinsert(cmpd_tid3, "deep_nested_float", HOFFSET(third_level_compound, deep_nested_float), array);
            H5Tclose(array);

            H5Tinsert(nest2, "deep_nested_compound", HOFFSET(further_nested2, deep_nest), cmpd_tid3);

            H5Tinsert(cmpd_tid2, "further_nested_compoundB", HOFFSET(multiple_nested_compound, c), nest2);
            H5Tclose(nest2);

            H5Tinsert(type, "multiple_nested_compound", HOFFSET(compound, e), cmpd_tid2);


            if ((dset = H5Dcreate2(file, F82_DATASETNAME, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                for(i = 0; i < nelmts; i++) {
                    for (j = 0; j < dset_array_a_dims[0]; j++)
                        buf[i].a[j] = (unsigned int)(j + i*10);
                    for (j = 0; j < dset_array_b_dims[0]; j++)
                        buf[i].b[j] = (int)(j - i*10);
                    for (j = 0; j < dset_array_c_dims[0]; j++)
                        for (k = 0; k < dset_array_c_dims[1]; k++)
                            buf[i].c[j][k] = (float)(j + k + i*10) + (float)(j) * 0.1F;

                    /* Set up first nested compound */
                    buf[i].d.nested_a = (double) i;

                    HDstrcpy(buf[i].d.nested_string, "This is a test string.");

                    for (j = 0; j < 4; j++)
                        HDstrcpy(buf[i].d.nested_string_array[j], "String test");

                    /* Set up multiple nested compound */
                    buf[i].e.a = (float) i;

                    for (j = 0; j < 5; j++) {
                        buf[i].e.b.multiple_nested_a[j] = (unsigned int)(j + i*10);
                        buf[i].e.b.multiple_nested_b[j] = (int)(j - i*10);
                        buf[i].e.b.multiple_nested_c[j] = (unsigned long)(j + i*10);
                        buf[i].e.b.multiple_nested_d[j] = (long)(j - i*10);
                    }

                    HDstrcpy(buf[i].e.c.further_nested_string, "1234567890");
                    for (j = 0; j < 4; j++)
                        HDstrcpy(buf[i].e.c.further_nested_string_array[j], "STRING ARRAY");

                    for (j = 0; j < 10; j++) {
                        buf[i].e.c.deep_nest.deep_nested_short[j] = (short)(j + i*10);
                        buf[i].e.c.deep_nest.deep_nested_int[j] = (int)(j - i*10);
                        buf[i].e.c.deep_nest.deep_nested_long[j] = (long)(j + i*10);
                        buf[i].e.c.deep_nest.deep_nested_double[j] = (double)(j + i*10);
                        buf[i].e.c.deep_nest.deep_nested_float[j] = (float)(j + i*10);
                    }
                }

                if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                    HDfprintf(stderr, "gent_tcompound_complex2 H5Dwrite failed\n");

                H5Dclose(dset);
            }
            H5Tclose(type);
        }
        H5Tclose(cmpd_tid1);
        H5Tclose(cmpd_tid2);
        H5Tclose(cmpd_tid3);
        H5Sclose(space);
    }

    /* CompoundComplex2D */
/*   if ((type = H5Tcreate(H5T_COMPOUND, sizeof(s_t))) >= 0) {
        H5Tinsert(type, "a", HOFFSET(s_t, a), H5T_STD_B8LE);
        H5Tinsert(type, "b", HOFFSET(s_t, b), H5T_STD_B16LE);
        H5Tinsert(type, "c", HOFFSET(s_t, c), H5T_STD_B32LE);
        H5Tinsert(type, "d", HOFFSET(s_t, d), H5T_STD_B64LE);
        if ((space = H5Screate_simple(F82_RANK2, &nelmts, NULL)) >= 0) {
            if ((dset = H5Dcreate2(file, "compound_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                for(i = 0; i < nelmts; i++) {
                    buf5[i].a = (unsigned char)0xff ^ (unsigned char)i;
                    buf5[i].b = (unsigned int)0xffff ^ (unsigned int)(i * 16);
                    buf5[i].c = (unsigned long)0xffffffff ^ (unsigned long)(i * 32);
                    buf5[i].d = (unsigned long long)0xffffffffffffffff ^ (unsigned long long)(i * 64);
                }

                H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf5);
                H5Dclose(dset);
            }
            H5Sclose(space);
        }
        H5Tclose(type);
    }
*/
    /* CompoundComplex3D */
/*    if ((type = H5Tcreate(H5T_COMPOUND, sizeof(s_t))) >= 0) {
        H5Tinsert(type, "a", HOFFSET(s_t, a), H5T_STD_B8LE);
        H5Tinsert(type, "b", HOFFSET(s_t, b), H5T_STD_B16LE);
        H5Tinsert(type, "c", HOFFSET(s_t, c), H5T_STD_B32LE);
        H5Tinsert(type, "d", HOFFSET(s_t, d), H5T_STD_B64LE);
        if ((space = H5Screate_simple(F82_RANK3, &nelmts, NULL)) >= 0) {
            if ((dset = H5Dcreate2(file, "compound_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                for(i = 0; i < nelmts; i++) {
                    buf5[i].a = (unsigned char)0xff ^ (unsigned char)i;
                    buf5[i].b = (unsigned int)0xffff ^ (unsigned int)(i * 16);
                    buf5[i].c = (unsigned long)0xffffffff ^ (unsigned long)(i * 32);
                    buf5[i].d = (unsigned long long)0xffffffffffffffff ^ (unsigned long long)(i * 64);
                }

                H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf5);
                H5Dclose(dset);
            }
            H5Sclose(space);
        }
        H5Tclose(type);
    }
*/
    /* CompoundComplex4D */
/*    if ((type = H5Tcreate(H5T_COMPOUND, sizeof(s_t))) >= 0) {
        H5Tinsert(type, "a", HOFFSET(s_t, a), H5T_STD_B8LE);
        H5Tinsert(type, "b", HOFFSET(s_t, b), H5T_STD_B16LE);
        H5Tinsert(type, "c", HOFFSET(s_t, c), H5T_STD_B32LE);
        H5Tinsert(type, "d", HOFFSET(s_t, d), H5T_STD_B64LE);
        if ((space = H5Screate_simple(F82_RANK4, &nelmts, NULL)) >= 0) {
            if ((dset = H5Dcreate2(file, "compound_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                for(i = 0; i < nelmts; i++) {
                    buf5[i].a = (unsigned char)0xff ^ (unsigned char)i;
                    buf5[i].b = (unsigned int)0xffff ^ (unsigned int)(i * 16);
                    buf5[i].c = (unsigned long)0xffffffff ^ (unsigned long)(i * 32);
                    buf5[i].d = (unsigned long long)0xffffffffffffffff ^ (unsigned long long)(i * 64);
                }

                H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf5);
                H5Dclose(dset);
            }
            H5Sclose(space);
        }
        H5Tclose(type);
    }
*/
    H5Fclose(file);

    HDfree(buf);
}

/*-------------------------------------------------------------------------
 * Function:    gent_vlenstr_array
 *
 * Purpose:     Generate a file to be used in testing Arrays of variable
 *              length strings.
 *-------------------------------------------------------------------------
 */
static void gent_vlenstr_array(void)
{
    /* Compound datatype with different member types */
    typedef struct compound
    {
        /* Array of variable-length strings*/
        const char *vlen_array[F83_ARRAYDIM];
    } compound;
    compound        buf[F83_DIM];

    const char *test[F83_ARRAYDIM] = {
            "This is a variable-length test string.",
            "This test string is also variable-length.",
            "A final test of variable-length strings. This string is longer than the others."
    };
    const char    *buffer[F83_DIM*F83_ARRAYDIM];

    hid_t  file, type=-1, space=-1, dset=-1;
    hid_t cmpd_tid1, array_tid;
    int i, j;

    hsize_t dims[] = {F83_DIM}, arraydim[] = {F83_ARRAYDIM};
    /* Initialize scalar data */
    for (i = 0; i < F83_DIM; i++)
        for (j = 0; j < 3; j++)
            buffer[j + 3*i] = test[j];

    /* Initialize compound data */
    for (i = 0; i < F83_DIM; i++)
        for (j = 0; j < 3; j++)
            buf[i].vlen_array[j] = test[j];

    file = H5Fcreate(FILE83, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    if ((space = H5Screate_simple(F83_RANK, dims, NULL)) >= 0) {
        array_tid = H5Tcopy(H5T_C_S1);
        H5Tset_size(array_tid, H5T_VARIABLE);

        /* ScalarArrayOfVlenStr */
        if ((type = H5Tarray_create2(array_tid, F83_RANK, arraydim)) >= 0) {
            if ((dset = H5Dcreate2(file, F83_DATASETNAME, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buffer) < 0)
                    HDfprintf(stderr, "gent_vlenstr_array H5Dwrite failed\n");

                H5Dclose(dset);
            }
            H5Tclose(type);
        }
        H5Tclose(array_tid);
        H5Sclose(space);
    }

    if ((space = H5Screate_simple(F83_RANK, dims, NULL)) >= 0) {
        /* CompoundArrayOfVlenStr */
        if ((type = H5Tcreate(H5T_COMPOUND, sizeof(compound))) >= 0) {
            cmpd_tid1 = H5Tcopy(H5T_C_S1);
            H5Tset_size(cmpd_tid1, H5T_VARIABLE);

            array_tid = H5Tarray_create2(cmpd_tid1, F83_RANK, arraydim);
            H5Tinsert(type, "vlen_str_array", HOFFSET(compound, vlen_array), array_tid);

            if ((dset = H5Dcreate2(file, F83_DATASETNAME2, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                    HDfprintf(stderr, "gent_vlenstr_array H5Dwrite failed\n");

                H5Dclose(dset);
            }
            H5Tclose(cmpd_tid1);
            H5Tclose(type);
        }
        H5Sclose(space);
    }

    H5Fclose(file);
}

/*-------------------------------------------------------------------------
 * Function:    gent_udfilter
 *
 * Purpose:     Generate a file to be used in testing user defined filter plugin3.
 *-------------------------------------------------------------------------
 */
static void gent_udfilter(void)
{
    hid_t    fid;  /* file id */
    hid_t    dcpl; /* dataset creation property list */
    hid_t    dsid;  /* dataset ID */
    hid_t    sid;  /* dataspace ID */
    hid_t    tid;  /* datatype ID */

    hsize_t  dims1[RANK]      = {DIM1,DIM2};
    hsize_t  chunk_dims[RANK] = {CDIM1,CDIM2};
    int      buf1[DIM1][DIM2];
    int      i, j, n, ret;

    for(i=n=0; i<DIM1; i++){
        for(j=0; j<DIM2; j++){
            buf1[i][j]=n++;
        }
    }

    /* create a file */
    fid  = H5Fcreate(FILE84, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    HDassert(fid>=0);

    /* create a space */
    sid = H5Screate_simple(SPACE2_RANK, dims1, NULL);

    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    HDassert(dcpl>=0);

    ret = H5Pset_layout(dcpl, H5D_CHUNKED);
    HDassert(ret >= 0);

    ret = H5Pset_chunk(dcpl, SPACE2_RANK, chunk_dims);
    HDassert(ret >= 0);

    ret = H5Zregister (H5Z_DYNLIBUD);
    HDassert(ret >= 0);

    ret = H5Pset_filter (dcpl, H5Z_FILTER_DYNLIBUD, H5Z_FLAG_MANDATORY, 0, NULL);
    HDassert(ret >= 0);

    /* create the dataset */
    dsid = H5Dcreate2(fid, "dynlibud", H5T_STD_I32LE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    HDassert(dsid >= 0);

    /* write */
    ret = H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1);
    HDassert(ret >= 0);

    /* close */
    ret = H5Dclose(dsid);
    HDassert(ret >= 0);

    /* remove the filters from the dcpl */
    ret = H5Premove_filter(dcpl, H5Z_FILTER_ALL);
    HDassert(ret >= 0);

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    ret = H5Sclose(sid);
    HDassert(ret >= 0);

    ret = H5Pclose(dcpl);
    HDassert(ret >= 0);

    ret = H5Fclose(fid);
    HDassert(ret >= 0);
}

/*-------------------------------------------------------------------------
 * Function:    H5Z_filter_dynlibud
 *
 * Purpose:    A dynlibud filter method that multiplies the original value
 *              during write and divide the original value during read. It
 *              will be built as a shared library.  tools tests will load
 *              and use this filter as a plugin library.
 *
 * Return:    Success:    Data chunk size
 *
 *        Failure:    0
 *-------------------------------------------------------------------------
 */
static size_t
H5Z_filter_dynlibud(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    char *int_ptr = (char *)*buf;          /* Pointer to the data values */
    size_t buf_left = *buf_size;  /* Amount of data buffer left to process */

    /* Check for the correct number of parameters */
    if(cd_nelmts > 0)
        return(0);

    /* Assignment to eliminate unused parameter warning. */
    cd_values = cd_values;

    if(flags & H5Z_FLAG_REVERSE) { /*read*/
        /* Subtract the original value with MULTIPLIER */
        while(buf_left > 0) {
            char temp = *int_ptr;
            *int_ptr = temp - MULTIPLIER;
            int_ptr++;
            buf_left -= sizeof(*int_ptr);
        } /* end while */
    } /* end if */
    else { /*write*/
        /* Add the original value with MULTIPLIER */
        while(buf_left > 0) {
            char temp = *int_ptr;
            *int_ptr = temp + MULTIPLIER;
            int_ptr++;
            buf_left -= sizeof(*int_ptr);
        } /* end while */
    } /* end else */

    return nbytes;
} /* end H5Z_filter_dynlibud() */

/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */

int main(void)
{
    gent_group();
    gent_attribute();
    gent_softlink();
    gent_softlink2();
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
    gent_ldouble();
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
    gent_bitnopaquefields();

    gent_intsfourdims();

    gent_udfilter();

    return 0;
}

