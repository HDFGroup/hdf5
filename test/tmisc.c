/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

/***********************************************************
*
* Test program:	 tmisc
*
* Test miscellaneous features not tested elsewhere.  Generally
*       regression tests for bugs that are reported and don't
*       have an existing test to add them to.
*
*************************************************************/

#include "hdf5.h"
#include "testhdf5.h"

/* Definitions for misc. test #1 */
#define MISC1_FILE	"tmisc.h5"
#define MISC1_VAL       (13417386)      /* 0xccbbaa */
#define MISC1_VAL2      (15654348)      /* 0xeeddcc */
#define MISC1_DSET_NAME "/scalar_set"

/* Definitions for misc. test #2 */
#define MISC2_FILE_1        "tmisc2a.h5"
#define MISC2_FILE_2        "tmisc2b.h5"
#define MISC2_ATT_NAME_1 "scalar_att_1"
#define MISC2_ATT_NAME_2 "scalar_att_2"

typedef struct {
      char *string;
} misc2_struct;

/* Definitions for misc. test #3 */
#define MISC3_FILE          "tmisc3.h5"
#define MISC3_RANK              2
#define MISC3_DIM1              6
#define MISC3_DIM2              6
#define MISC3_CHUNK_DIM1        2
#define MISC3_CHUNK_DIM2        2
#define MISC3_FILL_VALUE        2
#define MISC3_DSET_NAME         "/chunked"

/* Definitions for misc. test #4 */
#define MISC4_FILE_1            "tmisc4a.h5"
#define MISC4_FILE_2            "tmisc4b.h5"
#define MISC4_GROUP_1           "/Group1"
#define MISC4_GROUP_2           "/Group2"

/* Definitions for misc. test #5 */
#define MISC5_FILE              "tmisc5.h5"
#define MISC5_DSETNAME          "dset1"
#define MISC5_DSETRANK          1
#define MISC5_NELMTOPLVL        1
#define MISC5_DBGNELM1          2
#define MISC5_DBGNELM2          1
#define MISC5_DBGNELM3          1
#define MISC5_DBGELVAL1         999999999
#define MISC5_DBGELVAL2         888888888
#define MISC5_DBGELVAL3         777777777

typedef struct
{   
    int st1_el1;
    hvl_t st1_el2;
} misc5_struct1;

typedef struct
{   
    int st2_el1;
    hvl_t st2_el2;
} misc5_struct2;

typedef struct
{   
    int st3_el1;
} misc5_struct3;

typedef struct
{   
    hid_t         st3h_base;
    hid_t         st3h_id;
} misc5_struct3_hndl;

typedef struct
{   
    hid_t         st2h_base;
    hid_t         st2h_id;
    misc5_struct3_hndl *st2h_st3hndl;
} misc5_struct2_hndl;

typedef struct
{   
    hid_t         st1h_base;
    hid_t         st1h_id;
    misc5_struct2_hndl *st1h_st2hndl;
} misc5_struct1_hndl;

/* Definitions for misc. test #6 */
#define MISC6_FILE              "tmisc6.h5"
#define MISC6_DSETNAME1         "dset1"
#define MISC6_DSETNAME2         "dset2"
#define MISC6_NUMATTR           16

/* Definitions for misc. test #7 */
#define MISC7_FILE              "tmisc7.h5"
#define MISC7_DSETNAME1         "Dataset1"
#define MISC7_DSETNAME2         "Dataset2"
#define MISC7_TYPENAME1         "Datatype1"
#define MISC7_TYPENAME2         "Datatype2"

/****************************************************************
**
**  test_misc1(): test unlinking a dataset from a group and immediately
**                      re-using the dataset name
**
****************************************************************/
static void
test_misc1(void)
{
    int i;
    int i_check;
    hid_t file, dataspace, dataset;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Unlinking Dataset and Re-creating It\n"));

    file = H5Fcreate(MISC1_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    /* Write the dataset the first time. */
    dataset = H5Dcreate(file, MISC1_DSET_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    i = MISC1_VAL;
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &i);
    CHECK(ret, FAIL, "H5Dwrite");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Remove the dataset. */
    ret = H5Gunlink(file, MISC1_DSET_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Write the dataset for the second time with a different value. */
    dataset = H5Dcreate(file, MISC1_DSET_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    i = MISC1_VAL2;
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &i);
    CHECK(ret, FAIL, "H5Dwrite");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Now, check the value written to the dataset, after it was re-created */
    file = H5Fopen(MISC1_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    dataset = H5Dopen(file, MISC1_DSET_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &i_check);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(i_check,MISC1_VAL2,"H5Dread");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_misc1() */

static hid_t misc2_create_type(void)
{
    hid_t type, type_tmp;
    herr_t ret;

    type_tmp = H5Tcopy (H5T_C_S1);
    CHECK(type_tmp, FAIL, "H5Tcopy");

    ret = H5Tset_size (type_tmp, H5T_VARIABLE);
    CHECK(ret, FAIL, "H5Tset_size");

    type = H5Tcreate (H5T_COMPOUND, sizeof(misc2_struct));
    CHECK(type, FAIL, "H5Tcreate");

    ret = H5Tinsert (type, "string", offsetof(misc2_struct, string), type_tmp);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tclose(type_tmp);
    CHECK(ret, FAIL, "H5Tclose");

    return type;
}

static void test_misc2_write_attribute(void)
{
    hid_t file1, file2, root1, root2, dataspace, att1, att2;
    hid_t type;
    herr_t ret;
    misc2_struct data, data_check;
    char *string_att1 = HDstrdup("string attribute in file one");
    char *string_att2 = HDstrdup("string attribute in file two");

    type = misc2_create_type();

    dataspace = H5Screate(H5S_SCALAR);
    CHECK(dataspace, FAIL, "H5Screate");

    file2 = H5Fcreate(MISC2_FILE_2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file2, FAIL, "H5Fcreate");

    file1 = H5Fcreate(MISC2_FILE_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1, FAIL, "H5Fcreate");

    root1 = H5Gopen(file1, "/");
    CHECK(root1, FAIL, "H5Gopen");

    att1 = H5Acreate(root1, MISC2_ATT_NAME_1, type, dataspace, H5P_DEFAULT);
    CHECK(att1, FAIL, "H5Acreate");

    data.string = string_att1;

    ret = H5Awrite(att1, type, &data);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att1, type, &data_check);
    CHECK(ret, FAIL, "H5Aread");

    free(data_check.string);

    ret = H5Aclose(att1);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Gclose(root1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file1);
    CHECK(ret, FAIL, "H5Fclose");



    root2 = H5Gopen(file2, "/");
    CHECK(root2, FAIL, "H5Gopen");

    att2 = H5Acreate(root2, MISC2_ATT_NAME_2, type, dataspace, H5P_DEFAULT);
    CHECK(att2, FAIL, "H5Acreate");

    data.string = string_att2;

    ret = H5Awrite(att2, type, &data);
    CHECK(ret, FAIL, "H5Awrite");

    ret = H5Aread(att2, type, &data_check);
    CHECK(ret, FAIL, "H5Aread");

    free(data_check.string);

    ret = H5Aclose(att2);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(root2);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file2);
    CHECK(ret, FAIL, "H5Fclose");

    free(string_att1);
    free(string_att2);
    return;
}


static void test_misc2_read_attribute(const char *filename, const char *att_name)
{
    hid_t file, root, att;
    hid_t type;
    herr_t ret;
    misc2_struct data_check;

    type = misc2_create_type();

    file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    root = H5Gopen(file, "/");
    CHECK(root, FAIL, "H5Gopen");

    att = H5Aopen_name(root, att_name);
    CHECK(att, FAIL, "H5Aopen_name");

    ret = H5Aread(att, type, &data_check);
    CHECK(ret, FAIL, "H5Aread");

    free(data_check.string);

    ret = H5Aclose(att);
    CHECK(ret, FAIL, "HAclose");

    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(root);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    return;
}

/****************************************************************
**
**  test_misc2(): test using the same VL-derived datatype in two
**      different files, which was causing problems with the
**      datatype conversion functions
**
****************************************************************/
static void
test_misc2(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing VL datatype in two different files\n"));

    test_misc2_write_attribute();
    test_misc2_read_attribute(MISC2_FILE_1, MISC2_ATT_NAME_1);
    test_misc2_read_attribute(MISC2_FILE_2, MISC2_ATT_NAME_2);
} /* end test_misc2() */

/****************************************************************
**
**  test_misc3(): Test reading from chunked dataset with non-zero
**      fill value
**
****************************************************************/
static void
test_misc3(void)
{
    hid_t file, dataspace, dataset, dcpl;
    int rank=MISC3_RANK;
    hsize_t dims[MISC3_RANK]={MISC3_DIM1,MISC3_DIM2};
    hsize_t chunk_dims[MISC3_RANK]={MISC3_CHUNK_DIM1,MISC3_CHUNK_DIM2};
    int fill=MISC3_FILL_VALUE;
    int read_buf[MISC3_DIM1][MISC3_DIM2];
    int i,j;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing reading from chunked dataset with non-zero fill-value\n"));

    file = H5Fcreate(MISC3_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create a simple dataspace */
    dataspace = H5Screate_simple(rank,dims,NULL);
    CHECK(dataspace, FAIL, "H5Screate_simple");

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate"); 

    /* Set the chunk information */
    ret = H5Pset_chunk(dcpl,rank,chunk_dims);
    CHECK(dcpl, FAIL, "H5Pset_chunk"); 

    /* Set the fill-value information */
    ret = H5Pset_fill_value(dcpl,H5T_NATIVE_INT,&fill);
    CHECK(dcpl, FAIL, "H5Pset_fill_value"); 

    /* Create the dataset */
    dataset = H5Dcreate(file, MISC3_DSET_NAME, H5T_NATIVE_INT, dataspace, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Read from the dataset (should be fill-values) */
    ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &read_buf);
    CHECK(ret, FAIL, "H5Dread");

    for(i=0; i<MISC3_DIM1; i++)
        for(j=0; j<MISC3_DIM2; j++)
            VERIFY(read_buf[i][j],fill,"H5Dread");

    /* Release resources */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    ret = H5Sclose(dataspace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc3() */

/****************************************************************
**
**  test_misc4(): Test the that 'fileno' field in H5G_stat_t is
**      valid.
**
****************************************************************/
static void
test_misc4(void)
{
    hid_t file1, file2, group1, group2, group3;
    H5G_stat_t stat1, stat2, stat3;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing fileno working in H5G_stat_t\n"));

    file1 = H5Fcreate(MISC4_FILE_1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file1, FAIL, "H5Fcreate");

    /* Create the first group */
    group1 = H5Gcreate(file1, MISC4_GROUP_1, 0);
    CHECK(group1, FAIL, "H5Gcreate");

    /* Create the second group */
    group2 = H5Gcreate(file1, MISC4_GROUP_2, 0);
    CHECK(group2, FAIL, "H5Gcreate");

    file2 = H5Fcreate(MISC4_FILE_2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file2, FAIL, "H5Fcreate");

    /* Create the first group */
    group3 = H5Gcreate(file2, MISC4_GROUP_1, 0);
    CHECK(group3, FAIL, "H5Gcreate");

    /* Get the stat information for each group */
    ret = H5Gget_objinfo(file1,MISC4_GROUP_1,0,&stat1);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    ret = H5Gget_objinfo(file1,MISC4_GROUP_2,0,&stat2);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    ret = H5Gget_objinfo(file2,MISC4_GROUP_1,0,&stat3);
    CHECK(ret, FAIL, "H5Gget_objinfo");

    /* Verify that the fileno values are the same for groups from file1 */
    VERIFY(stat1.fileno[0],stat2.fileno[0],"H5Gget_objinfo");
    VERIFY(stat1.fileno[1],stat2.fileno[1],"H5Gget_objinfo");

    /* Verify that the fileno values are not the same between file1 & file2 */
    if(stat1.fileno[0]==stat3.fileno[0] && stat1.fileno[1]==stat3.fileno[1]) {
        num_errs++;
        printf("Error on line %d: stat1.fileno==stat3.fileno\n",__LINE__);
    } /* end if */
    if(stat2.fileno[0]==stat3.fileno[0] && stat2.fileno[1]==stat3.fileno[1]) {
        num_errs++;
        printf("Error on line %d: stat1.fileno==stat3.fileno\n",__LINE__);
    } /* end if */

    /* Close the objects */
    ret = H5Gclose(group1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group2);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(group3);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file1);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Fclose(file2);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_misc4() */

/****************************************************************
**
**  test_misc5(): Test several level deep nested compound & VL datatypes
**
****************************************************************/

/*********************** struct3 ***********************/

static misc5_struct3_hndl *
create_struct3(void)
{
    misc5_struct3_hndl *str3hndl;       /* New 'struct3' created */
    herr_t ret;                         /* For error checking */

    str3hndl=malloc(sizeof(misc5_struct3_hndl));
    CHECK(str3hndl,NULL,"malloc");

    str3hndl->st3h_base=H5Tcreate( H5T_COMPOUND, sizeof(misc5_struct3));
    CHECK(str3hndl->st3h_base,FAIL,"H5Tcreate");

    ret=H5Tinsert(str3hndl->st3h_base, "st3_el1", HOFFSET( misc5_struct3, st3_el1), H5T_NATIVE_INT);
    CHECK(ret,FAIL,"H5Tinsert");

    str3hndl->st3h_id=H5Tvlen_create(str3hndl->st3h_base);
    CHECK(str3hndl->st3h_id,FAIL,"H5Tvlen_create");

    return(str3hndl);
}

static void 
delete_struct3(misc5_struct3_hndl *str3hndl)
{
    herr_t ret;                         /* For error checking */

    ret=H5Tclose(str3hndl->st3h_id);
    CHECK(ret,FAIL,"H5Tclose");

    ret=H5Tclose(str3hndl->st3h_base);
    CHECK(ret,FAIL,"H5Tclose");

    free(str3hndl);
}

static void 
set_struct3(misc5_struct3 *buf)
{
    buf->st3_el1=MISC5_DBGELVAL3;
}

/*********************** struct2 ***********************/

static misc5_struct2_hndl *
create_struct2(void)
{
    misc5_struct2_hndl *str2hndl;       /* New 'struct2' created */
    herr_t ret;                         /* For error checking */

    str2hndl=malloc(sizeof(misc5_struct2_hndl));
    CHECK(str2hndl,NULL,"malloc");

    str2hndl->st2h_base=H5Tcreate( H5T_COMPOUND, sizeof(misc5_struct2));
    CHECK(str2hndl->st2h_base,FAIL,"H5Tcreate");

    ret=H5Tinsert(str2hndl->st2h_base, "st2_el1", HOFFSET( misc5_struct2, st2_el1), H5T_NATIVE_INT);
    CHECK(ret,FAIL,"H5Tinsert");

    str2hndl->st2h_st3hndl=create_struct3();
    CHECK(str2hndl->st2h_st3hndl,NULL,"create_struct3");

    ret=H5Tinsert(str2hndl->st2h_base, "st2_el2", HOFFSET(misc5_struct2, st2_el2), str2hndl->st2h_st3hndl->st3h_id);
    CHECK(ret,FAIL,"H5Tinsert");

    str2hndl->st2h_id= H5Tvlen_create(str2hndl->st2h_base);
    CHECK(str2hndl->st2h_id,FAIL,"H5Tvlen_create");

    return(str2hndl);
}

static void
delete_struct2(misc5_struct2_hndl *str2hndl)
{
    herr_t ret;                         /* For error checking */

    ret=H5Tclose(str2hndl->st2h_id);
    CHECK(ret,FAIL,"H5Tclose");

    delete_struct3(str2hndl->st2h_st3hndl);

    H5Tclose(str2hndl->st2h_base);
    CHECK(ret,FAIL,"H5Tclose");

    free(str2hndl);
}

static void
set_struct2(misc5_struct2 *buf)
{
    unsigned i;         /* Local index variable */

    buf->st2_el1=MISC5_DBGELVAL2;
    buf->st2_el2.len=MISC5_DBGNELM3;

    buf->st2_el2.p=malloc((buf->st2_el2.len)*sizeof(misc5_struct3));
    CHECK(buf->st2_el2.p,NULL,"malloc");

    for(i=0; i<(buf->st2_el2.len); i++)
        set_struct3(&(((misc5_struct3 *)(buf->st2_el2.p))[i]));
}

static void
clear_struct2(misc5_struct2 *buf)
{
    free(buf->st2_el2.p);
}

/*********************** struct1 ***********************/

static misc5_struct1_hndl *
create_struct1(void)
{
    misc5_struct1_hndl *str1hndl;       /* New 'struct1' created */
    herr_t ret;                         /* For error checking */

    str1hndl=malloc(sizeof(misc5_struct1_hndl));
    CHECK(str1hndl,NULL,"malloc");

    str1hndl->st1h_base=H5Tcreate(H5T_COMPOUND, sizeof(misc5_struct1));
    CHECK(str1hndl->st1h_base,FAIL,"H5Tcreate");

    ret=H5Tinsert(str1hndl->st1h_base, "st1_el1", HOFFSET(misc5_struct1, st1_el1), H5T_NATIVE_INT);
    CHECK(ret,FAIL,"H5Tinsert");

    str1hndl->st1h_st2hndl=create_struct2();
    CHECK(str1hndl->st1h_st2hndl,NULL,"create_struct2");

    ret=H5Tinsert(str1hndl->st1h_base, "st1_el2", HOFFSET(misc5_struct1, st1_el2), str1hndl->st1h_st2hndl->st2h_id);
    CHECK(ret,FAIL,"H5Tinsert");

    str1hndl->st1h_id=H5Tvlen_create(str1hndl->st1h_base);
    CHECK(str1hndl->st1h_id,FAIL,"H5Tvlen_create");

    return(str1hndl);
}

static void
delete_struct1(misc5_struct1_hndl *str1hndl)
{   
    herr_t ret;                         /* For error checking */

    ret=H5Tclose(str1hndl->st1h_id);
    CHECK(ret,FAIL,"H5Tclose");

    delete_struct2(str1hndl->st1h_st2hndl);

    ret=H5Tclose(str1hndl->st1h_base);
    CHECK(ret,FAIL,"H5Tclose");

    free(str1hndl);
}

static void
set_struct1(misc5_struct1 *buf)
{   
    unsigned i;         /* Local index variable */

    buf->st1_el1=MISC5_DBGELVAL1;
    buf->st1_el2.len=MISC5_DBGNELM2;

    buf->st1_el2.p=malloc((buf->st1_el2.len)*sizeof(misc5_struct2));
    CHECK(buf->st1_el2.p,NULL,"malloc");

    for(i=0; i<(buf->st1_el2.len); i++)
        set_struct2(&(((misc5_struct2 *)(buf->st1_el2.p))[i]));
}

static void
clear_struct1(misc5_struct1 *buf)
{
    unsigned i;

    for(i=0;i<buf->st1_el2.len;i++)
        clear_struct2(&((( misc5_struct2 *)(buf->st1_el2.p))[i]));
    free(buf->st1_el2.p);
}

static void
test_misc5(void)
{
    hid_t loc_id, space_id, dataset_id;
    hid_t mem_type_id;
    misc5_struct1_hndl *str1hndl;
    hsize_t dims[MISC5_DSETRANK];
    hvl_t buf;
    unsigned i,j,k;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing several level deep nested compound & VL datatypes \n"));

    /* Write the dataset out */
    loc_id=H5Fcreate(MISC5_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(loc_id,FAIL,"H5Fcreate");

    /* Create the memory structure to write */
    str1hndl=create_struct1();
    CHECK(str1hndl,NULL,"create_struct1");

    /* Create the dataspace */
    dims[0]=MISC5_NELMTOPLVL;
    space_id=H5Screate_simple(MISC5_DSETRANK, dims, NULL);
    CHECK(space_id,FAIL,"H5Screate_simple");

    /* Create the dataset */
    dataset_id=H5Dcreate(loc_id, MISC5_DSETNAME, str1hndl->st1h_id, space_id, H5P_DEFAULT);
    CHECK(dataset_id,FAIL,"H5Dcreate");

    /* Create the variable-length buffer */
    buf.len=MISC5_DBGNELM1;
    buf.p=malloc((buf.len)*sizeof(misc5_struct1));
    CHECK(buf.p,NULL,"malloc");

    /* Create the top-level VL information */
    for(i=0; i<MISC5_DBGNELM1; i++)
        set_struct1(&(((misc5_struct1 *) (buf.p))[i]));

    /* Write the data out */
    ret=H5Dwrite(dataset_id, str1hndl->st1h_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buf);
    CHECK(ret,FAIL,"H5Dwrite");

    /* Release the top-level VL information */
    for(j=0; j<MISC5_DBGNELM1; j++)
        clear_struct1(&((( misc5_struct1 *)(buf.p))[j]));

    /* Free the variable-length buffer */
    free(buf.p);

    /* Close dataset */
    ret=H5Dclose(dataset_id);
    CHECK(ret,FAIL,"H5Dclose");

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret,FAIL,"H5Sclose");

    /* Delete memory structures */
    delete_struct1(str1hndl);

    /* Close file */
    ret=H5Fclose(loc_id);
    CHECK(ret,FAIL,"H5Fclose");


    /* Read the dataset back in & verify it */
    loc_id=H5Fopen(MISC5_FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(loc_id,FAIL,"H5Fopen");

    /* Open dataset again */
    dataset_id=H5Dopen(loc_id, MISC5_DSETNAME);
    CHECK(dataset_id,FAIL,"H5Dopen");

    /* Get the dataset's datatype */
    mem_type_id=H5Dget_type(dataset_id);
    CHECK(mem_type_id,FAIL,"H5Dget_type");

    /* Get the dataset's dataspace */
    space_id=H5Dget_space(dataset_id);
    CHECK(space_id,FAIL,"H5Dget_space");

    /* Read the data back in */
    ret=H5Dread(dataset_id, mem_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buf);
    CHECK(ret,FAIL,"H5Dread");

    /* Verify the correct information was read in */
    for(i=0; i<(buf.len); i++) {
        /* printf("[%d]=%d\n",i, ((misc5_struct1 *)(buf.p))[i].st1_el1); */
        VERIFY(((misc5_struct1 *)(buf.p))[i].st1_el1,MISC5_DBGELVAL1,"H5Dread");
        for(j=0; j<(((misc5_struct1 *)(buf.p)) [i].st1_el2.len); j++) {
            /* printf("   [%d]=%d\n",j, ((misc5_struct2 *)(((misc5_struct1 *) (buf.p))[i].st1_el2.p))[j].st2_el1); */
            VERIFY(((misc5_struct2 *)(((misc5_struct1 *) (buf.p))[i].st1_el2.p))[j].st2_el1, MISC5_DBGELVAL2,"H5Dread");
            for(k=0; k<(((misc5_struct2 *) (((misc5_struct1 *)(buf.p))[i].  st1_el2.p))[j].st2_el2.len); k++) {
                /* printf("      [%d]=%d\n",k, ((misc5_struct3 *)(((misc5_struct2 *) (((misc5_struct1 *)(buf.p))[i].  st1_el2.p))[j].st2_el2.p))[k].st3_el1); */
                VERIFY(((misc5_struct3 *)(((misc5_struct2 *) (((misc5_struct1 *)(buf.p))[i].  st1_el2.p))[j].st2_el2.p))[k].st3_el1, MISC5_DBGELVAL3,"H5Dread");
            } /* end for */
        }
    }

    /* Reclaim the memory for the VL information */
    ret=H5Dvlen_reclaim(mem_type_id, space_id, H5P_DEFAULT, &buf);
    CHECK(ret,FAIL,"H5Dvlen_reclaim");

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret,FAIL,"H5Sclose");

    /* Close dataset */
    ret=H5Tclose(mem_type_id);
    CHECK(ret,FAIL,"H5Tclose");

    /* Close dataset */
    ret=H5Dclose(dataset_id);
    CHECK(ret,FAIL,"H5Dclose");

    /* Close file */
    ret=H5Fclose(loc_id);
    CHECK(ret,FAIL,"H5Fclose");

} /* end test_misc5() */

/****************************************************************
**
**  test_misc6(): Test that object header continuation messages are
**      created correctly.
**
****************************************************************/
static void
test_misc6(void)
{
    hid_t loc_id, space_id, dataset_id;
    hid_t attr_id;
    char attr_name[16];
    unsigned u;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing object header continuation code \n"));

    /* Create the file */
    loc_id=H5Fcreate(MISC6_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(loc_id,FAIL,"H5Fcreate");

    /* Create the dataspace */
    space_id=H5Screate(H5S_SCALAR);
    CHECK(space_id,FAIL,"H5Screate");

    /* Create the first dataset */
    dataset_id=H5Dcreate(loc_id, MISC6_DSETNAME1, H5T_NATIVE_INT, space_id, H5P_DEFAULT);
    CHECK(dataset_id,FAIL,"H5Dcreate");

    /* Close dataset */
    ret=H5Dclose(dataset_id);
    CHECK(ret,FAIL,"H5Dclose");

    /* Create the second dataset */
    dataset_id=H5Dcreate(loc_id, MISC6_DSETNAME2, H5T_NATIVE_INT, space_id, H5P_DEFAULT);
    CHECK(dataset_id,FAIL,"H5Dcreate");

    /* Close dataset */
    ret=H5Dclose(dataset_id);
    CHECK(ret,FAIL,"H5Dclose");

    /* Close file */
    ret=H5Fclose(loc_id);
    CHECK(ret,FAIL,"H5Fclose");

    /* Loop through adding attributes to each dataset */
    for(u=0; u<MISC6_NUMATTR; u++) {
        /* Create name for attribute */
        sprintf(attr_name,"Attr#%u",u);

        /* Open the file */
        loc_id=H5Fopen(MISC6_FILE, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK(loc_id,FAIL,"H5Fopen");


        /* Open first dataset */
        dataset_id=H5Dopen(loc_id, MISC6_DSETNAME1);
        CHECK(dataset_id,FAIL,"H5Dopen");

        /* Add attribute to dataset */
        attr_id=H5Acreate(dataset_id,attr_name,H5T_NATIVE_INT,space_id,H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Acreate");

        /* Close attribute */
        ret=H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");

        /* Close dataset */
        ret=H5Dclose(dataset_id);
        CHECK(ret, FAIL, "H5Dclose");


        /* Open second dataset */
        dataset_id=H5Dopen(loc_id, MISC6_DSETNAME2);
        CHECK(dataset_id,FAIL,"H5Dopen");

        /* Add attribute to dataset */
        attr_id=H5Acreate(dataset_id,attr_name,H5T_NATIVE_INT,space_id,H5P_DEFAULT);
        CHECK(attr_id, FAIL, "H5Acreate");

        /* Close attribute */
        ret=H5Aclose(attr_id);
        CHECK(ret, FAIL, "H5Aclose");

        /* Close dataset */
        ret=H5Dclose(dataset_id);
        CHECK(ret, FAIL, "H5Dclose");


        /* Close file */
        ret=H5Fclose(loc_id);
        CHECK(ret,FAIL,"H5Fclose");

    } /* end for */

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret,FAIL,"H5Sclose");

} /* end test_misc6() */

/****************************************************************
**
**  test_misc7(): Test that datatypes are sensible to store on
**      disk.  (i.e. not partially initialized)
**
****************************************************************/
static void
test_misc7(void)
{
    hid_t fid, did, tid, sid;
    int enum_value=1;
    herr_t ret;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing sensible datatype on disk code \n"));

    /* Attempt to commit a non-sensible datatype */

    /* Create the file */
    fid=H5Fcreate(MISC7_FILE,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);
    CHECK(fid,FAIL,"H5Fcreate");

    /* Create the dataspace */
    sid=H5Screate(H5S_SCALAR);
    CHECK(sid,FAIL,"H5Screate");

    /* Create the compound datatype to commit*/
    tid=H5Tcreate(H5T_COMPOUND,32);
    CHECK(tid,FAIL,"H5Tcreate");

    /* Attempt to commit an empty compound datatype */
    ret=H5Tcommit(fid,MISC7_TYPENAME1,tid);
    VERIFY(ret,FAIL,"H5Tcommit");

    /* Attempt to use empty compound datatype to create dataset */
    did=H5Dcreate(fid,MISC7_DSETNAME1,tid,sid,H5P_DEFAULT);
    VERIFY(ret,FAIL,"H5Dcreate");

    /* Add a field to the compound datatype */
    ret=H5Tinsert(tid,"a",0,H5T_NATIVE_INT);
    CHECK(ret,FAIL,"H5Tinsert");

    /* Attempt to commit the compound datatype now - should work */
    ret=H5Tcommit(fid,MISC7_TYPENAME1,tid);
    CHECK(ret,FAIL,"H5Tcommit");

    /* Attempt to use compound datatype to create dataset now - should work */
    did=H5Dcreate(fid,MISC7_DSETNAME1,tid,sid,H5P_DEFAULT);
    CHECK(did,FAIL,"H5Dcreate");

    /* Close dataset */
    ret=H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close compound datatype */
    ret=H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the enum datatype to commit*/
    tid=H5Tenum_create(H5T_NATIVE_INT);
    CHECK(tid,FAIL,"H5Tenum_create");

    /* Attempt to commit an empty enum datatype */
    ret=H5Tcommit(fid,MISC7_TYPENAME2,tid);
    VERIFY(ret,FAIL,"H5Tcommit");

    /* Attempt to use empty enum datatype to create dataset */
    did=H5Dcreate(fid,MISC7_DSETNAME2,tid,sid,H5P_DEFAULT);
    VERIFY(did,FAIL,"H5Dcreate");

    /* Add a member to the enum datatype */
    ret=H5Tenum_insert(tid,"a",&enum_value);
    CHECK(ret,FAIL,"H5Tenum_insert");

    /* Attempt to commit the enum datatype now - should work */
    ret=H5Tcommit(fid,MISC7_TYPENAME2,tid);
    CHECK(ret,FAIL,"H5Tcommit");

    /* Attempt to use enum datatype to create dataset now - should work */
    did=H5Dcreate(fid,MISC7_DSETNAME2,tid,sid,H5P_DEFAULT);
    CHECK(did,FAIL,"H5Dcreate");

    /* Close dataset */
    ret=H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close enum datatype */
    ret=H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close dataspace */
    ret=H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret=H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_misc7() */

/****************************************************************
**
**  test_misc(): Main misc. test routine.
** 
****************************************************************/
void 
test_misc(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Miscellaneous Routines\n"));

    test_misc1();   /* Test unlinking a dataset & immediately re-using name */
    test_misc2();   /* Test storing a VL-derived datatype in two different files */
    test_misc3();   /* Test reading from chunked dataset with non-zero fill value */
    test_misc4();   /* Test retrieving the fileno for various objects with H5Gget_objinfo() */
    test_misc5();   /* Test several level deep nested compound & VL datatypes */
    test_misc6();   /* Test object header continuation code */
    test_misc7();       /* Test for sensible datatypes stored on disk */

} /* test_misc() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_misc
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_misc(void)
{
    remove(MISC1_FILE);
    remove(MISC2_FILE_1);
    remove(MISC2_FILE_2);
    remove(MISC3_FILE);
    remove(MISC4_FILE_1);
    remove(MISC4_FILE_2);
    remove(MISC5_FILE);
    remove(MISC6_FILE);
    remove(MISC7_FILE);
}
