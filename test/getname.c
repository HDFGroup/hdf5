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
 * Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
 *              April 12, 2002
 *
 * Purpose:     Tests the "ID to name" functionality
 */

#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5G_TESTING

#include "h5test.h"
#include "H5Gpkg.h"		/* Groups				*/


/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

const char *FILENAME[] = {
    "getname",
    "getname1",
    "getname2",
    "getname3",
    NULL
};

#define RANK 2
#define NX 4
#define NY 5

#define NAME_BUF_SIZE   64
#define SMALL_NAME_BUF_SIZE   2

static int
check_name(hid_t id, const char *chk_name, const char *chk_user_path)
{
    char name[NAME_BUF_SIZE];           /* Buffer to hold name and its size */
    char user_path[NAME_BUF_SIZE];      /* Buffer to hold user path */
    size_t user_path_len;               /* Length of user path */
    unsigned user_path_hidden;          /* Whether the user path is hidden */

    /* Get name */
    *name = '\0';
    if(H5Iget_name(id, name, NAME_BUF_SIZE) < 0) goto error;

    /* Get user path */
    *user_path = '\0';
    if(H5G_user_path_test(id, user_path, &user_path_len, &user_path_hidden) < 0) goto error;

    /* Check on name from H5Iget_name() */
    if(HDstrcmp(name, chk_name)) goto error;

    /* Check on user path */
    if(HDstrcmp(user_path, chk_user_path)) goto error;

    /* Check that if user path is hidden, the name from H5Iget_name() and the user path should be different */
    if(user_path_hidden && !HDstrcmp(chk_name, chk_user_path)) goto error;

    /* Everything matches */
    return 0;

error:
    /* Something doesn't match or something bad happened */
    return -1;
}

int main( void )
{
 char filename0[1024];
 char filename1[1024];
 char filename2[1024];
 char filename3[1024];
 hid_t   fapl;
 hid_t   file_id, file1_id, file2_id, file3_id;
 hid_t   group_id, group2_id, group3_id, group4_id, group5_id, group6_id, group7_id;
 hid_t   dataset_id, dataset2_id;
 hid_t   space_id;
 hid_t   type_id, type2_id;
 hsize_t dims[1] = { 5 };

 /* Name length */
 size_t  name_len;

 /* Reset the library and get the file access property list */
 h5_reset();
 fapl = h5_fileaccess();

 /* Initialize the file names */
 h5_fixname(FILENAME[0], fapl, filename0, sizeof filename0);
 h5_fixname(FILENAME[1], fapl, filename1, sizeof filename1);
 h5_fixname(FILENAME[2], fapl, filename2, sizeof filename2);
 h5_fixname(FILENAME[3], fapl, filename3, sizeof filename3);

 /* Create a new file_id using default properties. */
 if ((file_id = H5Fcreate( filename0, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gcreate, one group
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gcreate, one group");

 /* Create group "g0" in the root group using absolute name */
 if ((group_id = H5Gcreate( file_id, "/g0", 0 ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g0", "/g0") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gcreate, more than one group
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gcreate, more than one group");

 /* Create group "g1" in the root group using absolute name */
 if ((group_id = H5Gcreate( file_id, "/g1", 0 ))<0) TEST_ERROR;

 /* Create group "g2" in group "g1" using absolute name */
 if ((group2_id = H5Gcreate( file_id, "/g1/g2", 0 ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gopen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gopen");

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "/g1" ))<0) TEST_ERROR;

 /* Reopen the group */
 if ((group2_id = H5Gopen( file_id, "/g1/g2" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 PASSED();




/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Dcreate
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Dcreate");

 /* Create the dataspace  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file_id , "d1", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/d1", "/d1") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "g1" ))<0) TEST_ERROR;

 /* Create a new dataset inside "g1" */
 if ((dataset_id = H5Dcreate( group_id , "d1", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/g1/d1", "/g1/d1") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Dclose( dataset_id );
 H5Sclose( space_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Dopen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Dopen");

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "d1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/d1", "/d1") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );


 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "g1" ))<0) TEST_ERROR;

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( group_id, "d1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/g1/d1", "/g1/d1") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Gclose( group_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with a long path
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with a long path");

 /* Create group "g2/bar/baz" */
 if ((group_id = H5Gcreate( file_id, "g2", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "g2/bar", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "g2/bar/baz", 0 ))<0) TEST_ERROR;

 /* Create a dataset */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;
 if ((dataset_id = H5Dcreate( group3_id , "d1", H5T_NATIVE_INT, space_id,
  H5P_DEFAULT ))<0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "/g2/bar/baz/d1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/g2/bar/baz/d1", "/g2/bar/baz/d1") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Tcommit
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Tcommit");

 /* Create a datatype */
 if ((type_id = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0) TEST_ERROR;

 /* Insert fields */
 if (H5Tinsert (type_id, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0) TEST_ERROR;
 if (H5Tinsert (type_id, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0) TEST_ERROR;
 if (H5Tinsert (type_id, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT)<0) TEST_ERROR;

 /* Save datatype for later */
 if (H5Tcommit (file_id, "t1", type_id)<0) TEST_ERROR;

 /* Verify */
 if(check_name(type_id, "/t1", "/t1") < 0) TEST_ERROR;

 /* Close datatype */
 H5Tclose(type_id);

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Topen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Topen");

 /* Open the named datatype */
 if((type_id=H5Topen(file_id, "t1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(type_id, "/t1", "/t1") < 0) TEST_ERROR;

 /* Close datatype */
 H5Tclose(type_id);

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and H5Gopen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Gopen");

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "/g1" ))<0) TEST_ERROR;

 /* Rename group */
 if (H5Gmove( file_id, "/g1", "/g1a" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g1a", "/g1a") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 PASSED();




/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and H5Dopen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Dopen");

  /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "/d1"))<0) TEST_ERROR;

 /* Rename dataset */
 if (H5Gmove( file_id, "/d1", "/d1a" )<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/d1a", "/d1a") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );

 PASSED();




/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and H5Topen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Topen");

 /* Open the named datatype */
 if((type_id=H5Topen(file_id, "/t1"))<0) TEST_ERROR;

 /* Rename datatype */
 if (H5Gmove( file_id, "/t1", "/t1a" )<0) TEST_ERROR;

 /* Verify */
 if(check_name(type_id, "/t1a", "/t1a") < 0) TEST_ERROR;

 /* Close datatype */
 H5Tclose(type_id);

 PASSED();

 /*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and relative names
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and relative names");

 /* Create group "/g3" */
 if ((group_id = H5Gcreate( file_id, "/g3", 0 ))<0) TEST_ERROR;

 /* Create group "/g3/foo" using absolute name */
 if ((group2_id = H5Gcreate( file_id, "/g3/foo1", 0 ))<0) TEST_ERROR;

 /* Open group "/g3/foo" again */
 if ((group3_id = H5Gopen( file_id, "/g3/foo1"))<0) TEST_ERROR;

 /* Rename group */
 if (H5Gmove( group_id, "foo1", "foo2" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g3", "/g3") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g3/foo2", "/g3/foo2") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g3/foo2", "/g3/foo2") < 0) TEST_ERROR;

 /* Rename group again */
 if (H5Gmove( file_id, "g3/foo2", "g3/foo1" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g3", "/g3") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g3/foo1", "/g3/foo1") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g3/foo1", "/g3/foo1") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );


 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and a long path
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and a long path");

 /* Create group "g4/A/B" */
 if ((group_id = H5Gcreate( file_id, "g4", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "g4/A", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "g4/A/B", 0 ))<0) TEST_ERROR;

 /* Create group "g5/C" */
 if ((group4_id = H5Gcreate( file_id, "g5", 0 ))<0) TEST_ERROR;
 if ((group5_id = H5Gcreate( file_id, "g5/C", 0 ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g4/A/B", "/g4/A/B") < 0) TEST_ERROR;

 /* Move group "B" to "D"*/
 if (H5Gmove( file_id, "/g4/A/B", "/g5/C/D" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g5/C/D", "/g5/C/D") < 0) TEST_ERROR;

 /* Move group "/g5/C/D" back to "/g4/A/B" using relative name */
 if (H5Gmove2( group5_id, "D", group2_id, "B" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g4/A/B", "/g4/A/B") < 0) TEST_ERROR;

 /* Move group "/g4/A/B" to "/g4/F/B" using relative name */
 if (H5Gmove2( group_id, "A", group_id, "F")<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g4/F/B", "/g4/F/B") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g4/F", "/g4/F") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Gclose( group5_id );

 PASSED();


 /*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and a long path #2
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and a long path #2");

 /* Create group "g6/A/B" and "g7" */
 if ((group_id = H5Gcreate( file_id, "g6", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "g6/A", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "g6/A/B", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file_id, "g7", 0 ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g6/A/B", "/g6/A/B") < 0) TEST_ERROR;

 /* Move group "A" to "C"*/
 if (H5Gmove( file_id, "/g6/A", "/g7/C" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g7/C", "/g7/C") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g7/C/B", "/g7/C/B") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gunlink
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gunlink");

  /* Create a new group. */
 if ((group_id = H5Gcreate( file_id, "/g8", 0 ))<0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file_id, "/g8")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gunlink and a long path
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gunlink and a long path");

 /* Create group "g9/a/b" */
 if ((group_id = H5Gcreate( file_id, "g9", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "g9/a", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "g9/a/b", 0 ))<0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file_id, "/g9/a")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Recreate groups */
 if ((group2_id = H5Gcreate( group_id, "a", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( group_id, "a/b", 0 ))<0) TEST_ERROR;

 /* Delete, using relative path */
 if (H5Gunlink( group_id, "a")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Close */
 H5Gclose( group_id );

 /* Create group "g10/a/b" */
 if ((group_id = H5Gcreate( file_id, "g10", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "g10/a", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "g10/a/b", 0 ))<0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file_id, "/g10/a/b")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group3_id );

 /* Recreate group */
 if ((group3_id = H5Gcreate( group_id, "a/b", 0 ))<0) TEST_ERROR;

 /* Delete, using relative path */
 if (H5Gunlink( group_id, "a/b")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group3_id );

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gunlink, same names
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gunlink, same names");


  /* Create group "g11/g" */
 if ((group_id = H5Gcreate( file_id, "g11", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "g11/g", 0 ))<0) TEST_ERROR;

 /* Create two datasets "g11/d" and "g11/g/d"*/
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;
 if ((dataset_id = H5Dcreate( group_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;
 if ((dataset2_id = H5Dcreate( group2_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file_id, "/g11/d")<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "", "") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset2_id, "/g11/g/d", "/g11/g/d") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Dclose( dataset2_id );
 H5Sclose( space_id );
 H5Gclose( group_id );
 H5Gclose( group2_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Fmount; with IDs on the list
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Fmount; with IDs on the list");

 /* Create a group "g12" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g12", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 /* Create second file and dataset "d" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 /* Create a dataspace  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;

 /* Create the dataset */
 if ((dataset_id = H5Dcreate( file1_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );

 /* Mount second file under "g12" in the first file */
 if (H5Fmount(file_id, "/g12", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Access dataset D in the first file under "/G/D" name */
 if ((dataset_id = H5Dopen( file_id, "/g12/d"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/g12/d", "/g12/d") < 0) TEST_ERROR;

 if (H5Funmount(file_id, "/g12")<0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Fclose( file1_id );
 H5Sclose( space_id );


 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Fmount; long name
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Fmount; long name");

 /* Create a group "g13/g1/g2" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g13", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g13/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g13/g1/g2", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "g" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g14", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g14/g3", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g14/g3/g4", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Mount second file under "/g13/g1" in the first file */
 if (H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g13/g1/g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;

 if (H5Funmount(file_id, "/g13/g1")<0) TEST_ERROR;


 /* Verify */
 if(check_name(group_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );


 /* Access group in the file to mount */
 if ((group3_id = H5Gopen( file1_id, "/g14/g3/g4"))<0) TEST_ERROR;

 /* Mount second file under "/g13/g1" in the first file (again) */
 if (H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Get a group ID for the parent of the newly mounted group */
 if ((group2_id = H5Gopen( file_id, "/g13"))<0) TEST_ERROR;

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g13/g1/g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g14/g3/g4", "/g14/g3/g4") < 0) TEST_ERROR;

 if (H5Funmount(group2_id, "g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g14/g3/g4", "/g14/g3/g4") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Mount second file under "/g13/g1" in the first file (again) */
 if (H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Get a group ID for the newly mounted group */
 if ((group2_id = H5Gopen( file_id, "/g13/g1"))<0) TEST_ERROR;

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g13/g1/g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR;

 if (H5Funmount(group2_id, ".")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 /* Mount second file under "/g13/g1" in the first file, using relative path */

 if ((group3_id = H5Gopen( file_id, "/g13"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g13", "/g13") < 0) TEST_ERROR;

 if (H5Fmount(group3_id, "g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Get a group ID for the newly mounted group */
 if ((group2_id = H5Gopen( file_id, "/g13/g1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR;

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g13/g1/g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 /* Access group in the first file, with relative path */
 if ((group_id = H5Gopen( group2_id, "g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 if (H5Funmount(group2_id, ".")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Mount second file under "/g13/g1" in the first file, using relative path */

 if ((group3_id = H5Gopen( file_id, "/g13/g1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR;

 if (H5Fmount(group3_id, ".", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Get a group ID for the newly mounted group */
 if ((group2_id = H5Gopen( file_id, "/g13/g1"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR;

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g13/g1/g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 /* Access group in the first file, with relative path */
 if ((group_id = H5Gopen( group2_id, "g14/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g13/g1/g14/g3/g4", "/g13/g1/g14/g3/g4") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 if (H5Funmount(group2_id, ".")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g13/g1", "/g13/g1") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 H5Fclose( file1_id );


 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Funmount
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Funmount");

 /* Create a group "g15/g1/g2" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g15", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g15/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g15/g1/g2", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file_id, "/g15/g1/g2/g3", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );

 /* Create second file and group "g" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g16", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g16/g4", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g16/g4/g5", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g15/g1/g2/g3"))<0) TEST_ERROR;

 /* Mount second file under "/g13/g1" in the first file */
 if (H5Fmount(file_id, "/g15/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Access group in the second file */
 if ((group2_id = H5Gopen( file_id, "/g15/g1/g16/g4/g5"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "/g15/g1/g2/g3") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g15/g1/g16/g4/g5", "/g15/g1/g16/g4/g5") < 0) TEST_ERROR;

 if (H5Funmount(file_id, "/g15/g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g15/g1/g2/g3", "/g15/g1/g2/g3") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Fclose( file1_id );


 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with a defined type dataset
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with a defined type dataset");

 /* Create a datatype */
 if ((type_id = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0) TEST_ERROR;

 /* Insert fields */
 if (H5Tinsert (type_id, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0) TEST_ERROR;
 if (H5Tinsert (type_id, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0) TEST_ERROR;
 if (H5Tinsert (type_id, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT)<0) TEST_ERROR;

  /* Create group "g17" */
 if ((group_id = H5Gcreate( file_id, "g17", 0 ))<0) TEST_ERROR;

 /* Save datatype for later */
 if (H5Tcommit (group_id, "t", type_id)<0) TEST_ERROR;

 /* Create a dataspace  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( group_id , "d", type_id, space_id,
  H5P_DEFAULT ))<0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Tclose( type_id );
 H5Sclose( space_id );
 H5Gclose( group_id );

 /* Open the named datatype */
 if((type_id=H5Topen(file_id, "/g17/t"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(type_id, "/g17/t", "/g17/t") < 0) TEST_ERROR;

 /* Close datatype */
 H5Tclose(type_id);

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "/g17/d"))<0) TEST_ERROR;

 /* Get datatype*/
 if((type_id=H5Dget_type(dataset_id))<0) TEST_ERROR;

 /* Verify */
 if(check_name(type_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Tclose( type_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with objects that have two names
 *-------------------------------------------------------------------------
 */

TESTING("H5Iget_name with datasets that have two names");

/* Open dataset named "d"*/
if ((dataset_id = H5Dopen( file_id, "/g17/d"))<0) TEST_ERROR;

/* Create link to dataset named "link" */
if (H5Glink2(dataset_id,".",H5G_LINK_HARD,file_id,"/g17/link")<0) TEST_ERROR;
if ((dataset2_id = H5Dopen( file_id, "/g17/link"))<0) TEST_ERROR;

/* Make sure that the two IDs use two different names */
 if(check_name(dataset_id, "/g17/d", "/g17/d") < 0) TEST_ERROR;
 if(check_name(dataset2_id, "/g17/link", "/g17/link") < 0) TEST_ERROR;

if(H5Dclose(dataset_id)<0) TEST_ERROR;
if(H5Dclose(dataset2_id)<0) TEST_ERROR;

PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with different files, test1
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with different files");

 /* Create a new file using default properties. */
 if ((file2_id = H5Fcreate( filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;

/* Create a new file using default properties. */
 if ((file3_id = H5Fcreate( filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;

  /* Create the dataspace  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file2_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset2_id = H5Dcreate( file3_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file2_id, "/d")<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "", "") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset2_id, "/d", "/d") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Dclose( dataset2_id );
 H5Sclose( space_id );
 H5Fclose( file2_id );
 H5Fclose( file3_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with different files, test2
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with different files #2");

 /* Create a new file using default properties. */
 if ((file2_id = H5Fcreate( filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;

/* Create a new file using default properties. */
 if ((file3_id = H5Fcreate( filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;

  /* Create the dataspace  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file2_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset2_id = H5Dcreate( file3_id , "d", H5T_NATIVE_INT, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file3_id, "/d")<0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset_id, "/d", "/d") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(dataset2_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Dclose( dataset_id );
 H5Dclose( dataset2_id );
 H5Sclose( space_id );
 H5Fclose( file2_id );
 H5Fclose( file3_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with a small buffer for name
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with a small buffer for name");

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "/g17" ))<0) TEST_ERROR;

{
 /*small buffer to hold name and its size */
 char    name2[SMALL_NAME_BUF_SIZE];

 /* Get name */
 name_len=H5Iget_name( group_id, name2, SMALL_NAME_BUF_SIZE );

 /* Check that name is longer */
 if(name_len <= SMALL_NAME_BUF_SIZE) TEST_ERROR;
 if(HDstrcmp(name2, "/")) TEST_ERROR;
}

 /* Verify */
 if(check_name(group_id, "/g17", "/g17") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );

 PASSED();


 /*-------------------------------------------------------------------------
 * Test H5Iget_name with a dynamic buffer for name
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with a dynamic buffer for name");

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "/g17" ))<0) TEST_ERROR;

 /* Get name */
 name_len = H5Iget_name(group_id, NULL, NAME_BUF_SIZE);

{
 /* dynamic buffer to hold name */
 char    *name3;

 /* Include the extra null character */
 name3 = HDmalloc(name_len + 1);
 if(!name3) TEST_ERROR;

 /* Get name with dynamic buffer */
 if(H5Iget_name(group_id, name3, name_len + 1) < 0) TEST_ERROR;

 /* Verify */
 if(HDstrcmp(name3, "/g17")) TEST_ERROR;
 *name3 = '\0';

  /* Get name with smaller buffer */
 if(H5Iget_name(group_id, name3, 3) < 0) TEST_ERROR;

 /* Verify */
 if(HDstrcmp(name3, "/g")) TEST_ERROR;

 HDfree(name3);
}

 /* Close */
 H5Gclose( group_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with invalid IDs
 *-------------------------------------------------------------------------
 */


 TESTING("H5Iget_name with invalid IDs");

 /* Create a dataspace  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;

 /* Define a datatype */
 if ((type_id = H5Tcopy(H5T_NATIVE_INT))<0) TEST_ERROR;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file_id , "d2", type_id, space_id, H5P_DEFAULT ))<0) TEST_ERROR;

{
    char name[NAME_BUF_SIZE];   /* Buffer to hold name and its size */

    /* Get name for non commited datatype, it should fail */
    H5E_BEGIN_TRY {
        if(H5Iget_name( type_id, name, NAME_BUF_SIZE) > 0) TEST_ERROR;
    } H5E_END_TRY;

    /* Get name for dataspace, it should fail */
    H5E_BEGIN_TRY {
        if(H5Iget_name( space_id, name, NAME_BUF_SIZE) > 0) TEST_ERROR;
    } H5E_END_TRY;
}

 /* Close */
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Tclose( type_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with added names with mounting
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with added names with mounting");

 /* Create a group "g18/g2" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g18", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g18/g2", 0 ))<0) TEST_ERROR;

 /* Also create a dataset and a datatype */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) TEST_ERROR;
 if ((type_id = H5Tcopy(H5T_NATIVE_INT))<0) TEST_ERROR;
 if ((dataset_id = H5Dcreate( file_id, "g18/d2", type_id, space_id,
  H5P_DEFAULT ))<0) TEST_ERROR;

 if (H5Tcommit(file_id, "g18/t2", type_id) <0) TEST_ERROR;

 /* Create second file and group "/g3/g4/g5" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
 if ((group3_id = H5Gcreate( file1_id, "/g3", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file1_id, "/g3/g4", 0 ))<0) TEST_ERROR;
 if ((group5_id = H5Gcreate( file1_id, "/g3/g4/g5", 0 ))<0) TEST_ERROR;

 /* Mount first file at "g3/g4" in the second file */
 if (H5Fmount(file1_id, "/g3/g4", file_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Get name for the group ID in the first file, should be "/g18/g2" still */
 if(check_name(group2_id, "/g18/g2", "/g18/g2") < 0) TEST_ERROR;

 /* Get name for the dataset ID in the first file, should be "/g18/g2/d2" still */
 if(check_name(dataset_id, "/g18/d2", "/g18/d2") < 0) TEST_ERROR;

 /* Get name for the datatype ID in the first file, should be "/g18/g2/t2" still */
 if(check_name(type_id, "/g18/t2", "/g18/t2") < 0) TEST_ERROR;

 /* Open the mounted group, dataset, and datatype through their new names */
 if ((group6_id = H5Gopen( file1_id, "/g3/g4/g18/g2" ))<0) TEST_ERROR;
 if ((dataset2_id = H5Dopen( file1_id, "/g3/g4/g18/d2" ))<0) TEST_ERROR;
 if ((type2_id = H5Topen( file1_id, "/g3/g4/g18/t2" ))<0) TEST_ERROR;

 /* Verify names */
 if(check_name(group6_id, "/g3/g4/g18/g2", "/g3/g4/g18/g2") < 0) TEST_ERROR;
 if(check_name(dataset2_id, "/g3/g4/g18/d2", "/g3/g4/g18/d2") < 0) TEST_ERROR;
 if(check_name(type2_id, "/g3/g4/g18/t2", "/g3/g4/g18/t2") < 0) TEST_ERROR;

 /* Verify that old IDs still refer to objects by their old names */
 if(check_name(group2_id, "/g18/g2", "/g18/g2") < 0) TEST_ERROR;
 if(check_name(dataset_id, "/g18/d2", "/g18/d2") < 0) TEST_ERROR;
 if(check_name(type_id, "/g18/t2", "/g18/t2") < 0) TEST_ERROR;

 /* Unmount */
 if (H5Funmount(file1_id, "/g3/g4")<0) TEST_ERROR;

 /* Get name for the IDs of the first file, should be unchanged */
 if(check_name(group2_id, "/g18/g2", "/g18/g2") < 0) TEST_ERROR;
 if(check_name(dataset_id, "/g18/d2", "/g18/d2") < 0) TEST_ERROR;
 if(check_name(type_id, "/g18/t2", "/g18/t2") < 0) TEST_ERROR;

 /* Get name for the IDs of the second file, should be "" */
 if(check_name(group6_id, "", "") < 0) TEST_ERROR;
 if(check_name(dataset2_id, "", "") < 0) TEST_ERROR;
 if(check_name(type2_id, "", "") < 0) TEST_ERROR;

 H5Tclose( type_id );
 H5Tclose( type2_id );
 H5Dclose( dataset_id );
 H5Dclose( dataset2_id );
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Gclose( group5_id );
 H5Gclose( group6_id );
 H5Fclose( file1_id );

PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Fclose
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Fclose");

 /* Create a file and group "/g1/g2" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
 if ((group_id = H5Gcreate( file1_id, "/g1", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g1/g2", 0 ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR;

 /* Close file */
 H5Fclose( file1_id );

 /* Verify */
 if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Fmount and H5Gunlink
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Fmount and H5Gunlink");

 /* Create a file and group "/g1/g2" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
 if ((group_id = H5Gcreate( file1_id, "/g1", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g1/g2", 0 ))<0) TEST_ERROR;

 /* Create a new file and group "/g3/g4" in it */
 if ((file2_id = H5Fcreate( filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file2_id, "/g3", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file2_id, "/g3/g4", 0 ))<0) TEST_ERROR;

 /* Mount first file at "/g3/g4" in the second file */
 if(H5Fmount(file2_id, "/g3/g4", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Open the mounted group */
 if ((group5_id = H5Gopen( file2_id, "/g3/g4/g1/g2" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR;

 /* Delete */
 if (H5Gunlink( file1_id, "/g3/g4/g1/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "", "") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Gclose( group5_id );
 H5Fclose( file1_id );
 H5Fclose( file2_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Fmount and H5Gmove
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Fmount and H5Gmove");

 /* Create a file and group "/g1/g2" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
 if ((group_id = H5Gcreate( file1_id, "/g1", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g1/g2", 0 ))<0) TEST_ERROR;

 /* Create a new file and group "/g3/g4" in it */
 if ((file2_id = H5Fcreate( filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file2_id, "/g3", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file2_id, "/g3/g4", 0 ))<0) TEST_ERROR;

 /* Mount first file at "g3/g4" in the second file */
 if(H5Fmount(file2_id, "/g3/g4", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g3/g4", "/g3/g4") < 0) TEST_ERROR;

 /* Open the mounted group */
 if ((group5_id = H5Gopen( file2_id, "/g3/g4/g1/g2" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR;

 /* Open another mounted group, in the middle of the path */
 if ((group6_id = H5Gopen( file2_id, "/g3/g4/g1" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR;

 /* Rename group */
 if (H5Gmove( file2_id, "/g3/g4/g1/g2", "/g3/g4/g1/g5" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g3/g4/g1/g5", "/g3/g4/g1/g5") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g1/g5", "/g1/g5") < 0) TEST_ERROR;

 /* Rename group */
 if (H5Gmove( file2_id, "/g3/g4/g1", "/g3/g4/g1a" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g3/g4/g1a/g5", "/g3/g4/g1a/g5") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g1a/g5", "/g1a/g5") < 0) TEST_ERROR;

 /* Verify */
 if(check_name(group6_id, "/g3/g4/g1a", "/g3/g4/g1a") < 0) TEST_ERROR;
 if(check_name(group_id, "/g1a", "/g1a") < 0) TEST_ERROR;

 /* Rename middle group back, using relative path */
 if (H5Gmove( group3_id, "g4/g1a", "g4/g1" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g3/g4/g1/g5", "/g3/g4/g1/g5") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g1/g5", "/g1/g5") < 0) TEST_ERROR;
 if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR;
 if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR;

 /* Rename end group back, using relative path */
 if (H5Gmove( group3_id, "g4/g1/g5", "g4/g1/g2" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g1/g2", "/g1/g2") < 0) TEST_ERROR;
 if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR;
 if(check_name(group_id, "/g1", "/g1") < 0) TEST_ERROR;

 /* Rename mount point */
 if (H5Gmove( file2_id, "/g3/g4", "/g3/g4a" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g3/g4a", "/g3/g4a") < 0) TEST_ERROR;
 if(check_name(group5_id, "/g3/g4a/g1/g2", "/g3/g4a/g1/g2") < 0) TEST_ERROR;
 if(check_name(group6_id, "/g3/g4a/g1", "/g3/g4a/g1") < 0) TEST_ERROR;

 /* Rename mount point back, using relative path*/
 if (H5Gmove( group3_id, "g4a", "g4" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g3/g4", "/g3/g4") < 0) TEST_ERROR;
 if(check_name(group5_id, "/g3/g4/g1/g2", "/g3/g4/g1/g2") < 0) TEST_ERROR;
 if(check_name(group6_id, "/g3/g4/g1", "/g3/g4/g1") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Gclose( group5_id );
 H5Gclose( group6_id );
 H5Fclose( file1_id );
 H5Fclose( file2_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Glink hard
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink hard");

 /* Create group "g19/g1" */
 if ((group_id = H5Gcreate( file_id, "/g19", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g19/g1", 0 ))<0) TEST_ERROR;

  /* Create hard link to "g19/g1/ group */
 if (H5Glink(file_id, H5G_LINK_HARD, "/g19/g1", "/g19/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g19/g2" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR;

 /* Rename original group */
 if (H5Gmove( file_id, "/g19/g1", "/g19/g3" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g19/g3", "/g19/g3") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR;

 /* Rename original group back, using relative path */
 if (H5Gmove( group_id, "g3", "g1" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR;

  /* Create another hard link to "/g19/g1" group */
 if (H5Glink(file_id, H5G_LINK_HARD, "/g19/g1", "/g19/g3")<0) TEST_ERROR;

 /* Open the group */
 if ((group4_id = H5Gopen( file_id, "/g19/g3" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g19/g3", "/g19/g3") < 0) TEST_ERROR;

 /* Delete group */
 if (H5Gunlink( file_id, "/g19/g3")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR;

 /* Close the unlinked group */
 H5Gclose( group4_id );

  /* Create another hard link to "/g19/g1" group */
 if (H5Glink(file_id, H5G_LINK_HARD, "/g19/g1", "/g19/g3")<0) TEST_ERROR;

 /* Open the group */
 if ((group4_id = H5Gopen( file_id, "/g19/g3" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g19/g3", "/g19/g3") < 0) TEST_ERROR;

 /* Delete group, using relative path */
 if (H5Gunlink( group_id, "g3")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g19/g1", "/g19/g1") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g19/g2", "/g19/g2") < 0) TEST_ERROR;

 /* Close the unlinked group */
 H5Gclose( group4_id );

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Glink symbolic
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink symbolic");

 /* Create group "g20/g1" */
 if ((group_id = H5Gcreate( file_id, "/g20", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g20/g1", 0 ))<0) TEST_ERROR;

  /* Create symbolic link to "g20/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g20/g1", "/g20/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g20/g1", "/g20/g1") < 0) TEST_ERROR;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g20/g2" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g20/g2", "/g20/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Glink symbolic and move target
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink symbolic and move target");

 /* Create group "g21/g1" */
 if ((group_id = H5Gcreate( file_id, "/g21", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g21/g1", 0 ))<0) TEST_ERROR;

 /* Create symbolic link to "g21/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g21/g1", "/g21/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g21/g1", "/g21/g1") < 0) TEST_ERROR;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g21/g2" ))<0) TEST_ERROR;

 /* Rename group */
 if (H5Gmove( file_id, "/g21/g1", "/g21/g3" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g21/g3", "/g21/g3") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g21/g2", "/g21/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Glink symbolic and move source
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink symbolic and move source");

 /* Create group "g22/g1" */
 if ((group_id = H5Gcreate( file_id, "/g22", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g22/g1", 0 ))<0) TEST_ERROR;

 /* Create symbolic link to "g22/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g22/g1", "/g22/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g22/g1", "/g22/g1") < 0) TEST_ERROR;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g22/g2" ))<0) TEST_ERROR;

 /* Rename soft link */
 if (H5Gmove( file_id, "/g22/g2", "/g22/g3" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g22/g1", "/g22/g1") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g22/g3", "/g22/g3") < 0) TEST_ERROR;

 /* Rename soft link, using relative paths */
 if (H5Gmove( group_id, "g3", "g2" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g22/g1", "/g22/g1") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g22/g2", "/g22/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Glink symbolic and unlink target
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink symbolic and unlink target");

 /* Create group "g23/g1" */
 if ((group_id = H5Gcreate( file_id, "/g23", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g23/g1", 0 ))<0) TEST_ERROR;

 /* Create symbolic link to "g23/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g23/g1", "/g23/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g23/g1", "/g23/g1") < 0) TEST_ERROR;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g23/g2" ))<0) TEST_ERROR;

 /* Delete group */
 if (H5Gunlink( file_id, "/g23/g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g23/g2", "/g23/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Glink symbolic and unlink source
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink symbolic and unlink source");

 /* Create group "g24/g1" */
 if ((group_id = H5Gcreate( file_id, "/g24", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g24/g1", 0 ))<0) TEST_ERROR;

 /* Create symbolic link to "g24/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g24/g1", "/g24/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g24/g1", "/g24/g1") < 0) TEST_ERROR;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g24/g2" ))<0) TEST_ERROR;

 /* Delete group */
 if (H5Gunlink( file_id, "/g24/g2")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with several nested mounted files
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with several nested mounted files");

 /* Create a group "g25/g1/g2" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g25", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g25/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g25/g1/g2", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "/g26/g3/g4" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g26", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g26/g3", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g26/g3/g4", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create third file and group "/g27/g5/g6" in it */
 file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file2_id, "/g27", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file2_id, "/g27/g5", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file2_id, "/g27/g5/g6", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create fourth file and group "/g28/g5/g6" in it */
 file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file3_id, "/g28", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file3_id, "/g28/g7", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file3_id, "/g28/g7/g8", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Access group which will be hidden in the first file */
 if ((group_id = H5Gopen( file_id, "/g25/g1/g2"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g25/g1/g2", "/g25/g1/g2") < 0) TEST_ERROR;

 /* Mount second file under "/g25/g1" in the first file */
 if (H5Fmount(file_id, "/g25/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "/g25/g1/g2") < 0) TEST_ERROR;

 /* Access group which will be hidden in the second file */
 if ((group2_id = H5Gopen( file_id, "/g25/g1/g26/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g25/g1/g26/g3/g4", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR;

 /* Mount third file under "/g25/g1/g26/g3" in the first file */
 if (H5Fmount(file_id, "/g25/g1/g26/g3", file2_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR;

 /* Access group in the third file */
 if ((group3_id = H5Gopen( file_id, "/g25/g1/g26/g3/g27/g5/g6"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g25/g1/g26/g3/g27/g5/g6", "/g25/g1/g26/g3/g27/g5/g6") < 0) TEST_ERROR;

 /* Mount fourth file under "/g25/g1/g26/g3/g27/g5" in the first file */
 if (H5Fmount(file_id, "/g25/g1/g26/g3/g27/g5", file3_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "/g25/g1/g26/g3/g27/g5/g6") < 0) TEST_ERROR;

 /* Access group in the fourth file */
 if ((group4_id = H5Gopen( file_id, "/g25/g1/g26/g3/g27/g5/g28/g7/g8"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g25/g1/g26/g3/g27/g5/g28/g7/g8", "/g25/g1/g26/g3/g27/g5/g28/g7/g8") < 0) TEST_ERROR;

 if (H5Funmount(file_id, "/g25/g1/g26/g3/g27/g5")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g25/g1/g26/g3/g27/g5/g6", "/g25/g1/g26/g3/g27/g5/g6") < 0) TEST_ERROR;
 if(check_name(group2_id, "", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR;
 if(check_name(group_id, "", "/g25/g1/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group4_id );
 H5Fclose( file3_id );

 if (H5Funmount(file_id, "/g25/g1/g26/g3")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g25/g1/g26/g3/g4", "/g25/g1/g26/g3/g4") < 0) TEST_ERROR;
 if(check_name(group_id, "", "/g25/g1/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group3_id );
 H5Fclose( file2_id );

 if (H5Funmount(file_id, "/g25/g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;
 if(check_name(group_id, "/g25/g1/g2", "/g25/g1/g2") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Fclose( file1_id );


 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name and H5Gmove with repeated path components
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name and H5Gmove with repeated path components");

 /* Create a group "g29/g1/g2/g1/g2" in a file */
 if ((group_id = H5Gcreate( file_id, "/g29", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g29/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g29/g1/g2", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file_id, "/g29/g1/g2/g1", 0 ))<0) TEST_ERROR;
 if ((group5_id = H5Gcreate( file_id, "/g29/g1/g2/g1/g2", 0 ))<0) TEST_ERROR;

 /* Rename group */
 if (H5Gmove( file_id, "/g29/g1/g2/g1/g2", "/g29/g1/g2/g1/g3" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g29/g1/g2/g1/g3", "/g29/g1/g2/g1/g3") < 0) TEST_ERROR;

 /* Rename group in middle of path, keeping within the same group */
 if (H5Gmove( file_id, "/g29/g1/g2/g1", "/g29/g1/g2/g3" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g29/g1/g2/g3", "/g29/g1/g2/g3") < 0) TEST_ERROR;
 if(check_name(group5_id, "/g29/g1/g2/g3/g3", "/g29/g1/g2/g3/g3") < 0) TEST_ERROR;

 /* Rename group in middle of path, moving to another group in file */
 if (H5Gmove( file_id, "/g29/g1/g2/g3", "/g29/g3" )<0)  TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g29/g3", "/g29/g3") < 0) TEST_ERROR;
 if(check_name(group5_id, "/g29/g3/g3", "/g29/g3/g3") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Gclose( group5_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with higher mounted file
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with higher mounted file");

 /* Create a group "/g30/g1/g2" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g30", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g30/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g30/g1/g2", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "/g31/g3/g4" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g31", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g31/g3", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g31/g3/g4", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create third file and group "/g32/g5/g6" in it */
 file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file2_id, "/g32", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file2_id, "/g32/g5", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file2_id, "/g32/g5/g6", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create fourth file and group "/g33/g5/g6" in it */
 file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file3_id, "/g33", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file3_id, "/g33/g7", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file3_id, "/g33/g7/g8", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Access group which will be hidden in the first file */
 if ((group_id = H5Gopen( file_id, "/g30/g1/g2"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g30/g1/g2", "/g30/g1/g2") < 0) TEST_ERROR;

 /* Mount second file under "/g30/g1" in the first file */
 if (H5Fmount(file_id, "/g30/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "/g30/g1/g2") < 0) TEST_ERROR;

 /* Access group which will be hidden in the second file */
 if ((group2_id = H5Gopen( file_id, "/g30/g1/g31/g3/g4"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g30/g1/g31/g3/g4", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR;

 /* Mount third file under "/g30/g1/g31/g3" in the first file */
 if (H5Fmount(file_id, "/g30/g1/g31/g3", file2_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR;

 /* Access group which will be hidden in the third file */
 if ((group3_id = H5Gopen( file_id, "/g30/g1/g31/g3/g32/g5/g6"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g30/g1/g31/g3/g32/g5/g6", "/g30/g1/g31/g3/g32/g5/g6") < 0) TEST_ERROR;

 /* Mount fourth file under "/g30" in the first file, hiding the files below it */
 if (H5Fmount(file_id, "/g30", file3_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "/g30/g1/g31/g3/g32/g5/g6") < 0) TEST_ERROR;

 /* Access group which will be in the fourth file */
 if ((group4_id = H5Gopen( file_id, "/g30/g33/g7/g8"))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g30/g33/g7/g8", "/g30/g33/g7/g8") < 0) TEST_ERROR;

 /* Unmount fourth file */
 if (H5Funmount(file_id, "/g30")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "/g30/g1/g31/g3/g32/g5/g6", "/g30/g1/g31/g3/g32/g5/g6") < 0) TEST_ERROR;
 if(check_name(group2_id, "", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR;
 if(check_name(group_id, "", "/g30/g1/g2") < 0) TEST_ERROR;

 /* Unmount third file */
 if (H5Funmount(file_id, "/g30/g1/g31/g3")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "/g30/g1/g31/g3/g4", "/g30/g1/g31/g3/g4") < 0) TEST_ERROR;
 if(check_name(group_id, "", "/g30/g1/g2") < 0) TEST_ERROR;

 /* Unmount second file */
 if (H5Funmount(file_id, "/g30/g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;
 if(check_name(group_id, "/g30/g1/g2", "/g30/g1/g2") < 0) TEST_ERROR;

 /* Close groups */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );

 /* Close files */
 H5Fclose( file1_id );
 H5Fclose( file2_id );
 H5Fclose( file3_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with multiple hard links and mounted files
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with multiple hard links and mounted files");

 /* Create second file and group "/g35/g3/g4" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g35", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g35/g3", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g35/g3/g4", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create group "/g34/g1/g2" in first file */
 if ((group_id = H5Gcreate( file_id, "/g34", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g34/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g34/g1/g2", 0 ))<0) TEST_ERROR;

  /* Create hard link to "/g34/g1/g2 group */
 if (H5Glink(file_id, H5G_LINK_HARD, "/g34/g1/g2", "/g34/g2a")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g34/g1/g2", "/g34/g1/g2") < 0) TEST_ERROR;

 /* Open the link to the group */
 if ((group4_id = H5Gopen( file_id, "/g34/g2a" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group4_id, "/g34/g2a", "/g34/g2a") < 0) TEST_ERROR;

 /* Mount second file under "/g34/g1" in the first file */
 if (H5Fmount(file_id, "/g34/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "", "/g34/g1/g2") < 0) TEST_ERROR;
 if(check_name(group4_id, "/g34/g2a", "/g34/g2a") < 0) TEST_ERROR;

 /* Unmount second file */
 if (H5Funmount(file_id, "/g34/g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group3_id, "/g34/g1/g2", "/g34/g1/g2") < 0) TEST_ERROR;
 if(check_name(group4_id, "/g34/g2a", "/g34/g2a") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Fclose( file1_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with mounted files and unlinking
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with mounted files and unlinking");

 /* Create group "/g36/g1/g2" in first file */
 if ((group_id = H5Gcreate( file_id, "/g36", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file_id, "/g36/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file_id, "/g36/g1/g2", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "/g37/g4" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g37", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g37/g4", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g37/g4/g5a", 0 ))<0) TEST_ERROR;
 if ((group4_id = H5Gcreate( file1_id, "/g37/g4/g5b", 0 ))<0) TEST_ERROR;

 /* Mount second file under "/g36/g1" in the first file */
 if (H5Fmount(file_id, "/g36/g1", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 /* Open group in mounted file */
 if ((group5_id = H5Gopen( file_id, "/g36/g1/g37/" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group5_id, "/g36/g1/g37", "/g36/g1/g37") < 0) TEST_ERROR;

 /* Open group to delete in mounted file */
 if ((group6_id = H5Gopen( file_id, "/g36/g1/g37/g4/g5a" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group6_id, "/g36/g1/g37/g4/g5a", "/g36/g1/g37/g4/g5a") < 0) TEST_ERROR;

 /* Delete end group in mounted file, using relative paths */
 if (H5Gunlink( group5_id, "g4/g5a")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group6_id, "", "") < 0) TEST_ERROR;
 if(check_name(group3_id, "", "") < 0) TEST_ERROR;

 /* Close deleted group */
 H5Gclose( group6_id );

 /* Open groups to delete in mounted file */
 if ((group6_id = H5Gopen( file_id, "/g36/g1/g37/g4" ))<0) TEST_ERROR;
 if ((group7_id = H5Gopen( file_id, "/g36/g1/g37/g4/g5b" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group6_id, "/g36/g1/g37/g4", "/g36/g1/g37/g4") < 0) TEST_ERROR;
 if(check_name(group7_id, "/g36/g1/g37/g4/g5b", "/g36/g1/g37/g4/g5b") < 0) TEST_ERROR;

 /* Delete middle group in mounted file, using relative paths */
 if (H5Gunlink( group5_id, "g4")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group6_id, "", "") < 0) TEST_ERROR;
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;
 if(check_name(group7_id, "", "") < 0) TEST_ERROR;
 if(check_name(group4_id, "", "") < 0) TEST_ERROR;

 /* Close deleted groups */
 H5Gclose( group6_id );
 H5Gclose( group7_id );

 /* Close group in mounted file */
 H5Gclose( group5_id );

 if (H5Funmount(file_id, "/g36/g1")<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Fclose( file1_id );

 PASSED();



/*-------------------------------------------------------------------------
 * Test H5Iget_name with mounting already mounted files
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with mounting already mounted files");

 /* Create file and group "/g38/g1/g2" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g38", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g38/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g38/g1/g2", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "/g39/g1/g2" in it */
 file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file2_id, "/g39", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file2_id, "/g39/g3", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file2_id, "/g39/g3/g4", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create third file and group "/g40/g5/g6" in it */
 file3_id = H5Fcreate(filename3, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file3_id, "/g40", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file3_id, "/g40/g5", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file3_id, "/g40/g5/g6", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Mount second file under "/g38/g1" in the first file */
 if (H5Fmount(file1_id, "/g38/g1", file2_id, H5P_DEFAULT)<0) TEST_ERROR;

 if ((group_id = H5Gopen( file1_id, "/g38/g1/g39/g3/g4" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g38/g1/g39/g3/g4", "/g38/g1/g39/g3/g4") < 0) TEST_ERROR;

 /* Mount first file under "/g40/g5" in the third file */
 if (H5Fmount(file3_id, "/g40/g5", file1_id, H5P_DEFAULT)<0) TEST_ERROR;

 if ((group2_id = H5Gopen( file3_id, "/g40/g5/g38/g1/g39/g3/g4" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "/g40/g5/g38/g1/g39/g3/g4", "/g40/g5/g38/g1/g39/g3/g4") < 0) TEST_ERROR;
 if(check_name(group_id, "/g38/g1/g39/g3/g4", "/g38/g1/g39/g3/g4") < 0) TEST_ERROR;

 /* Unmount first file */
 if (H5Funmount(file3_id, "/g40/g5")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;
 if(check_name(group_id, "/g38/g1/g39/g3/g4", "/g38/g1/g39/g3/g4") < 0) TEST_ERROR;

 /* Unmount second file */
 if (H5Funmount(file1_id, "/g38/g1")<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Fclose( file1_id );
 H5Fclose( file2_id );
 H5Fclose( file3_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with opening object in unmounted file
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with opening object in unmounted file");

 /* Create file and group "/g39/g1/g2" in it */
 file1_id = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file1_id, "/g41", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file1_id, "/g41/g1", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file1_id, "/g41/g1/g2", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "/g42/g1/g2" in it */
 file2_id = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

 if ((group_id = H5Gcreate( file2_id, "/g42", 0 ))<0) TEST_ERROR;
 if ((group2_id = H5Gcreate( file2_id, "/g42/g3", 0 ))<0) TEST_ERROR;
 if ((group3_id = H5Gcreate( file2_id, "/g42/g3/g4", 0 ))<0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Mount second file under "/g41/g1" in the first file */
 if (H5Fmount(file1_id, "/g41/g1", file2_id, H5P_DEFAULT)<0) TEST_ERROR;

 if ((group_id = H5Gopen( file1_id, "/g41/g1/g42/g3" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group_id, "/g41/g1/g42/g3", "/g41/g1/g42/g3") < 0) TEST_ERROR;

 /* Unmount file */
 if (H5Funmount(file1_id, "/g41/g1")<0) TEST_ERROR;

 if ((group2_id = H5Gopen( group_id, "g4" ))<0) TEST_ERROR;

 /* Verify */
 if(check_name(group2_id, "", "") < 0) TEST_ERROR;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Fclose( file1_id );
 H5Fclose( file2_id );

 PASSED();

/*-------------------------------------------------------------------------
 * end tests
 *-------------------------------------------------------------------------
 */


 /* Close file */
 H5Fclose( file_id );
 puts("All getname tests passed.");
 h5_cleanup(FILENAME, fapl);
 return 0;

error:
 H5Fclose( file_id );
 H5_FAILED();
 return 1;
}

