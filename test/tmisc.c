/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

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
#define MISC3_FILE              "tmisc3.h5"
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
    CHECK(ret, FAIL, "HAclose");

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
    CHECK(ret, FAIL, "H5Aclose");

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
**  test_misc(): Main misc. test routine.
** 
****************************************************************/
void 
test_misc(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Miscellaneous Routines\n"));

    test_misc1();       /* Test unlinking a dataset & immediately re-using name */
    test_misc2();       /* Test storing a VL-derived datatype in two different files */
    test_misc3();       /* Test reading from chunked dataset with non-zero fill value */
    test_misc4();       /* Test retrieving the fileno for various objects with H5Gget_objinfo() */

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
}
