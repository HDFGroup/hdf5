#include "hdf5.h"
#include "h5test.h"
 


/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

#define FILE0 "getname.h5"
#define FILE1 "getname1.h5"
#define FILE2 "getname2.h5"
#define FILE3 "getname3.h5"



#define RANK 2
#define NX 4
#define NY 5


int check_name( char *name, const char* check )
{

 int ret = HDstrcmp( name, check );
 HDstrcpy( name, "" );
 return ret;
  
}

                           

int main( void )
{
 
 hid_t   file_id, file1_id, file2_id, file3_id;
 hid_t   group_id, group2_id, group3_id, group4_id, group5_id, group6_id;
 hid_t   dataset_id, dataset2_id;
 hid_t   space_id;  
 hid_t   type_id;
 hsize_t dims[1] = { 5 };

 /*buffer to hold name and its size */
 char    name[20];
 size_t  size=20;

 /*small buffer to hold name and its size */
 char    name2[2];
 size_t  size2=2;

 /*dynamic buffer to hold name and its size */
 char    *name3 = NULL;
 size_t  name_len;

 
 /* Create a new file_id using default properties. */
 if ((file_id = H5Fcreate( FILE0, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;




/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gcreate 
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gcreate");

 /* Create group "g1" in the root group using absolute name */
 if ((group_id = H5Gcreate( file_id, "/g1", 0 ))<0) goto out;

 /* Create group "g2" in group "g1" using absolute name */
 if ((group2_id = H5Gcreate( file_id, "/g1/g2", 0 ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group_id, name, size )<0) goto out;
  
 /* Verify */
 if (check_name( name, "/g1" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;
 
 /* Verify */
 if (check_name( name, "/g1/g2" )!=0)
  goto out;
  
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
 if ((group_id = H5Gopen( file_id, "/g1" ))<0) goto out;

 /* Reopen the group */
 if ((group2_id = H5Gopen( file_id, "/g1/g2" ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1" )!=0)
  goto out;
 
 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1/g2" )!=0)
  goto out;
 
 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Dcreate
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Dcreate");
  
 /* Create the data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file_id , "d1", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/d1" )!=0)
  goto out;
  
 /* Close */
 H5Dclose( dataset_id ); 

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "g1" ))<0) goto out;

 /* Create a new dataset inside "g1" */
 if ((dataset_id = H5Dcreate( group_id , "d1", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;
 
 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1/d1" )!=0)
  goto out;
 
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
 if ((dataset_id = H5Dopen( file_id, "d1"))<0) goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/d1" )!=0)
  goto out;

 /* Close */
 H5Dclose( dataset_id ); 


 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "g1" ))<0) goto out;

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( group_id, "d1"))<0) goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1/d1" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "g2", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "g2/bar", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "g2/bar/baz", 0 ))<0) goto out;
  
 /* Create a dataset */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;
 if ((dataset_id = H5Dcreate( group3_id , "d1", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;
  
 /* Close */
 H5Dclose( dataset_id ); 
 H5Sclose( space_id );
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "/g2/bar/baz/d1"))<0) goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "/g2/bar/baz/d1" )!=0)
  goto out;

 /* Close */
 H5Dclose( dataset_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Tcommit
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Tcommit");

 /* Create a datatype */
 if ((type_id = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0) goto out;

 /* Insert fields */
 if (H5Tinsert (type_id, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0) goto out;
 if (H5Tinsert (type_id, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0) goto out;
 if (H5Tinsert (type_id, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT)<0) goto out;
  
 /* Save datatype for later */
 if (H5Tcommit (file_id, "t1", type_id)<0) goto out;
 
 /* Get name */
 if (H5Iget_name( type_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/t1" )!=0)
  goto out;

 /* Close datatype */
 H5Tclose(type_id);

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Topen 
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Topen");

 /* Open the named datatype */
 if((type_id=H5Topen(file_id, "t1"))<0) goto out;
 
 /* Get name */
 if (H5Iget_name( type_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/t1" )!=0)
  goto out;


 /* Close datatype */
 H5Tclose(type_id);

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and H5Gopen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Gopen");

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "/g1" ))<0) goto out;


 /* Rename group */
 if (H5Gmove( file_id, "/g1", "/g1a" )<0)  goto out;


 /* Get name */
 if (H5Iget_name( group_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1a" )!=0)
  goto out;

 
 /* Close */
 H5Gclose( group_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and H5Dopen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Dopen");

  /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "/d1"))<0) goto out;

 /* Rename dataset */
 if (H5Gmove( file_id, "/d1", "/d1a" )<0) goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/d1a" )!=0)
  goto out;

 /* Close */
 H5Dclose( dataset_id ); 

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and H5Topen
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Topen");

 /* Open the named datatype */
 if((type_id=H5Topen(file_id, "/t1"))<0) goto out;

 /* Rename datatype */
 if (H5Gmove( file_id, "/t1", "/t1a" )<0) goto out;
 
 /* Get name */
 if (H5Iget_name( type_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/t1a" )!=0)
  goto out;


 /* Close datatype */
 H5Tclose(type_id);

 PASSED();

 /*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and relative names
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and relative names");

 /* Create group "/g3" */
 if ((group_id = H5Gcreate( file_id, "/g3", 0 ))<0) goto out;  

 /* Create group "/g3/foo" using absolute name */
 if ((group2_id = H5Gcreate( file_id, "/g3/foo1", 0 ))<0) goto out;

 /* Rename group */
 if (H5Gmove( group_id, "foo1", "foo2" )<0)  goto out;


 /* Get name */
 if (H5Iget_name( group_id, name, size )<0) goto out;
 
 /* Verify */
 if (check_name( name, "/g3" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g3/foo2" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );

 
 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and a long path
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Gopen");

 /* Create group "g4/A/B" */
 if ((group_id = H5Gcreate( file_id, "g4", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "g4/A", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "g4/A/B", 0 ))<0) goto out;

 /* Create group "g5/C" */
 if ((group4_id = H5Gcreate( file_id, "g5", 0 ))<0) goto out;
 if ((group5_id = H5Gcreate( file_id, "g5/C", 0 ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g4/A/B" )!=0)
  goto out;

 /* Move group "B" to "D"*/
 if (H5Gmove( file_id, "/g4/A/B", "/g5/C/D" )<0)  goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;
 
 /* Verify */
 if (check_name( name, "/g5/C/D" )!=0)
  goto out;
  
 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );
 H5Gclose( group5_id );

 PASSED();


 /*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gmove and a long path
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gmove and H5Gopen");

 /* Create group "g6/A/B" and "g7" */
 if ((group_id = H5Gcreate( file_id, "g6", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "g6/A", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "g6/A/B", 0 ))<0) goto out;
 if ((group4_id = H5Gcreate( file_id, "g7", 0 ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g6/A/B" )!=0)
  goto out;

 /* Move group "A" to "C"*/
 if (H5Gmove( file_id, "/g6/A", "/g7/C" )<0)  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;
 
 /* Verify */
 if (check_name( name, "/g7/C" )!=0)
  goto out;
  
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
 if ((group_id = H5Gcreate( file_id, "/g8", 0 ))<0) goto out;

 /* Delete */
 if (H5Gunlink( file_id, "/g8")<0) 
  goto out;
 
 /* Get name */
 if (H5Iget_name( group_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gunlink and a long path
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gunlink and a long path");

 /* Create group "g9/a/b" */
 if ((group_id = H5Gcreate( file_id, "g9", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "g9/a", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "g9/a/b", 0 ))<0) goto out;

 /* Delete */
 if (H5Gunlink( file_id, "/g9/a")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create group "g10/a/b" */
 if ((group_id = H5Gcreate( file_id, "g10", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "g10/a", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "g10/a/b", 0 ))<0) goto out;

 /* Delete */
 if (H5Gunlink( file_id, "/g10/a/b")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Gunlink, same names
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Gunlink, same names");


  /* Create group "g11/g" */
 if ((group_id = H5Gcreate( file_id, "g11", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "g11/g", 0 ))<0) goto out;
  
 /* Create two datasets "g11/d" and "g11/g/d"*/
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;
 if ((dataset_id = H5Dcreate( group_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;
 if ((dataset2_id = H5Dcreate( group2_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Delete */
 if (H5Gunlink( file_id, "/g11/d")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( dataset2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g11/g/d" )!=0)
  goto out;
  
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
 if ((group_id = H5Gcreate( file_id, "/g12", 0 ))<0) goto out;
 
 /* Close */
 H5Gclose( group_id );

 /* Create second file and dataset "d" in it */
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

 /* Create a data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;
 
 /* Create the dataset */
 if ((dataset_id = H5Dcreate( file1_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Close */
 H5Dclose( dataset_id );

 /* Mount second file under "g12" in the first file */
 if (H5Fmount(file_id, "/g12", file1_id, H5P_DEFAULT)<0) goto out;

 /* Access dataset D in the first file under "/G/D" name */
 if ((dataset_id = H5Dopen( file_id, "/g12/d"))<0) goto out;

 /* Get name */
 if (H5Iget_name( dataset_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "/g12/d" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g13", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g13/g1", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "/g13/g1/g2", 0 ))<0) goto out;
 
 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Create second file and group "g" in it */
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

 if ((group_id = H5Gcreate( file1_id, "/g14", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file1_id, "/g14/g3", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file1_id, "/g14/g3/g4", 0 ))<0) goto out;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Mount second file under "/g13/g1" in the first file */
 if (H5Fmount(file_id, "/g13/g1", file1_id, H5P_DEFAULT)<0) goto out;

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g13/g1/g14/g3/g4"))<0) goto out;

 /* Get name */
 if (H5Iget_name( group_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "/g13/g1/g14/g3/g4" )!=0)
  goto out;

 if (H5Funmount(file_id, "/g13/g1")<0) goto out;


 /* Get name */
 if (H5Iget_name( group_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );
 H5Fclose( file1_id );


 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with H5Funmount 
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Funmount");

 /* Create a group "g15/g1/g2" in the first file */
 if ((group_id = H5Gcreate( file_id, "/g15", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g15/g1", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file_id, "/g15/g1/g2", 0 ))<0) goto out;
 if ((group4_id = H5Gcreate( file_id, "/g15/g1/g2/g3", 0 ))<0) goto out;
 
 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 H5Gclose( group4_id );

 /* Create second file and group "g" in it */
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

 if ((group_id = H5Gcreate( file1_id, "/g16", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file1_id, "/g16/g4", 0 ))<0) goto out;
 if ((group3_id = H5Gcreate( file1_id, "/g16/g4/g5", 0 ))<0) goto out;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );

 /* Access group in the first file */
 if ((group_id = H5Gopen( file_id, "/g15/g1/g2/g3"))<0) goto out;
 
 /* Mount second file under "/g13/g1" in the first file */
 if (H5Fmount(file_id, "/g15/g1", file1_id, H5P_DEFAULT)<0) goto out;

 /* Get name */
 if (H5Iget_name( group_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 if (H5Funmount(file_id, "/g15/g1")<0) goto out;

 /* Get name */
 if (H5Iget_name( group_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "/g15/g1/g2/g3" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );
 H5Fclose( file1_id );


 PASSED();

/*-------------------------------------------------------------------------
 * Test H5Iget_name with a defined type dataset
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with a defined type dataset");

 /* Create a datatype */
 if ((type_id = H5Tcreate (H5T_COMPOUND, sizeof(s1_t)))<0) goto out;

 /* Insert fields */
 if (H5Tinsert (type_id, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT)<0) goto out;
 if (H5Tinsert (type_id, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT)<0) goto out;
 if (H5Tinsert (type_id, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT)<0) goto out;

  /* Create group "g17" */
 if ((group_id = H5Gcreate( file_id, "g17", 0 ))<0) goto out;
  
 /* Save datatype for later */
 if (H5Tcommit (group_id, "t", type_id)<0) goto out;

 /* Create a data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( group_id , "d", type_id, space_id, 
  H5P_DEFAULT ))<0) goto out;
   
 /* Close */
 H5Dclose( dataset_id ); 
 H5Tclose( type_id );
 H5Sclose( space_id );
 H5Gclose( group_id );

 /* Open the named datatype */
 if((type_id=H5Topen(file_id, "/g17/t"))<0) goto out;
 
 /* Get name */
 if (H5Iget_name( type_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g17/t" )!=0)
  goto out;

 /* Close datatype */
 H5Tclose(type_id);

 /* Reopen the dataset */
 if ((dataset_id = H5Dopen( file_id, "/g17/d"))<0) goto out;

 /* Get datatype*/
 if((type_id=H5Dget_type(dataset_id))<0) goto out;


 /* Get name */
 if (H5Iget_name( type_id, name, size )< 0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
 H5Dclose( dataset_id );
 H5Tclose( type_id );

 PASSED();


/*-------------------------------------------------------------------------
 * Test H5Iget_name with different files, test1
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with different files");
 
 /* Create a new file using default properties. */
 if ((file2_id = H5Fcreate( FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;

/* Create a new file using default properties. */
 if ((file3_id = H5Fcreate( FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;

  /* Create the data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file2_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Create a new dataset */
 if ((dataset2_id = H5Dcreate( file3_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Delete */
 if (H5Gunlink( file2_id, "/d")<0) 
  goto out;
 
 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( dataset2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/d" )!=0)
  goto out;
  
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

 TESTING("H5Iget_name with different files");
 
 /* Create a new file using default properties. */
 if ((file2_id = H5Fcreate( FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;

/* Create a new file using default properties. */
 if ((file3_id = H5Fcreate( FILE3, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;

  /* Create the data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file2_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Create a new dataset */
 if ((dataset2_id = H5Dcreate( file3_id , "d", H5T_NATIVE_INT, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Delete */
 if (H5Gunlink( file3_id, "/d")<0) 
  goto out;
 
 /* Get name */
 if (H5Iget_name( dataset_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/d"  )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( dataset2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;
  
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
 if ((group_id = H5Gopen( file_id, "/g15" ))<0) goto out;

 /* Get name */
 name_len=H5Iget_name( group_id, name2, size2 );

 if ( name_len > size2 )
  /* Get name with a larger buffer */
  name_len=H5Iget_name( group_id, name, size );

 /* Verify */
 if (check_name( name, "/g15" )!=0)
  goto out;
  
 /* Close */
 H5Gclose( group_id );

 PASSED();


 /*-------------------------------------------------------------------------
 * Test H5Iget_name with a dynamic buffer for name
 *-------------------------------------------------------------------------
 */  

 TESTING("H5Iget_name with a dynamic buffer for name");

 /* Reopen the group */
 if ((group_id = H5Gopen( file_id, "/g15" ))<0) goto out;

 /* Get name */
 name_len=H5Iget_name( group_id, NULL, size );

 /* Include the extra null character */
 name3 = malloc(name_len+1);
  
 /* Get name */
 if (H5Iget_name( group_id, name3, name_len+1 )<0) goto out;

 /* Verify */
 if (check_name( name3, "/g15"  )!=0)
  goto out;

  /* Get name */
 if (H5Iget_name( group_id, name3, 3 )<0) goto out;

 /* Verify */
 if (check_name( name3, "/g"  )!=0)
  goto out;

 if ( name3 )
  free(name3);

  
 /* Close */
 H5Gclose( group_id );

 PASSED();




/*-------------------------------------------------------------------------
 * Test H5Iget_name with invalid IDs
 *-------------------------------------------------------------------------
 */


 TESTING("H5Iget_name with invalid IDs");
  
 /* Create a data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;

 /* Define a datatype */
 if ((type_id = H5Tcopy(H5T_NATIVE_INT))<0) goto out;

 /* Create a new dataset */
 if ((dataset_id = H5Dcreate( file_id , "d2", type_id, space_id, 
  H5P_DEFAULT ))<0) goto out;

 /* Get name for non commited datatype, it should fail */
 if (H5Iget_name( type_id, name, size ) >0) goto out;

 /* Get name for data space, it should fail */
 if (H5Iget_name( space_id, name, size ) >0) goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g18", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g18/g2", 0 ))<0) goto out;
 
 /* Create second file and group "/g3/g4/g5" in it */
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 if ((group3_id = H5Gcreate( file1_id, "/g3", 0 ))<0) goto out;
 if ((group4_id = H5Gcreate( file1_id, "/g3/g4", 0 ))<0) goto out;
 if ((group5_id = H5Gcreate( file1_id, "/g3/g4/g5", 0 ))<0) goto out;

 /* Mount first file at "g3/g4" in the second file */
 if (H5Fmount(file1_id, "/g3/g4", file_id, H5P_DEFAULT)<0) goto out;

 /* Get name for the ID of the first file, should be "/g19/g2" still */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g18/g2" )!=0)
  goto out;

 /* Open the mounted group */
 if ((group6_id = H5Gopen( file_id, "/g3/g4/g18/g2" ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group6_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g3/g4/g18/g2" )!=0)
  goto out;

 /* Unmount */
 if (H5Funmount(file1_id, "/g3/g4")<0) goto out;

 /* Get name for the ID of the first file, should be "/g18/g2" still */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g18/g2" )!=0)
  goto out;

 /* Get name for the ID of the secondt file, should be "" */
 if (H5Iget_name( group6_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
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
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 if ((group_id = H5Gcreate( file1_id, "/g1", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file1_id, "/g1/g2", 0 ))<0) goto out;

 /* Get name for the ID */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1/g2" )!=0)
  goto out;

 /* Close file */
 H5Fclose( file1_id );

  /* Get name for the ID */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1/g2" )!=0)
  goto out;

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
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 if ((group_id = H5Gcreate( file1_id, "/g1", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file1_id, "/g1/g2", 0 ))<0) goto out;
 
 /* Create a new file and group "/g3/g4" in it */
 if ((file2_id = H5Fcreate( FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;
 if ((group3_id = H5Gcreate( file2_id, "/g3", 0 ))<0) goto out;
 if ((group4_id = H5Gcreate( file2_id, "/g3/g4", 0 ))<0) goto out;

 /* Mount first file at "g3/g4" in the second file */
 if(H5Fmount(file2_id, "/g3/g4", file1_id, H5P_DEFAULT)<0) goto out;
 
 /* Open the mounted group */
 if ((group5_id = H5Gopen( file2_id, "/g3/g4/g1/g2" ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group5_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g3/g4/g1/g2" )!=0)
  goto out;

 /* Delete */
 if (H5Gunlink( file1_id, "/g3/g4/g1/g2")<0) 
  goto out;
 
 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;
  
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
 file1_id = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
 if ((group_id = H5Gcreate( file1_id, "/g1", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file1_id, "/g1/g2", 0 ))<0) goto out;
 
 /* Create a new file and group "/g3/g4" in it */
 if ((file2_id = H5Fcreate( FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, 
  H5P_DEFAULT ))<0) goto out;
 if ((group3_id = H5Gcreate( file2_id, "/g3", 0 ))<0) goto out;
 if ((group4_id = H5Gcreate( file2_id, "/g3/g4", 0 ))<0) goto out;


 /* Mount first file at "g3/g4" in the second file */
 if(H5Fmount(file2_id, "/g3/g4", file1_id, H5P_DEFAULT)<0) goto out;
 
 /* Open the mounted group */
 if ((group5_id = H5Gopen( file2_id, "/g3/g4/g1/g2" ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group5_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g3/g4/g1/g2" )!=0)
  goto out;

 /* Rename group */
 if (H5Gmove( file2_id, "/g3/g4/g1/g2", "/g3/g4/g1/g5" )<0)  goto out;
  
 /* Get name */
 if (H5Iget_name( group5_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g3/g4/g1/g5" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g1/g5" )!=0)
  goto out;
  
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
 * Test H5Iget_name with H5Glink hard
 *-------------------------------------------------------------------------
 */

 TESTING("H5Iget_name with H5Glink hard");

 /* Create group "g19/g1" */
 if ((group_id = H5Gcreate( file_id, "/g19", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g19/g1", 0 ))<0) goto out;

  /* Create hard link to "g19/g1/ group */
 if (H5Glink(file_id, H5G_LINK_HARD, "/g19/g1", "/g19/g2")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g19/g1" )!=0)
  goto out;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g19/g2" ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g19/g2" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g20", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g20/g1", 0 ))<0) goto out;

  /* Create symbolic link to "g20/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g20/g1", "/g20/g2")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g20/g1" )!=0)
  goto out;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g20/g2" ))<0) goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g20/g2" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g21", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g21/g1", 0 ))<0) goto out;

 /* Create symbolic link to "g21/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g21/g1", "/g21/g2")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g21/g1" )!=0)
  goto out;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g21/g2" ))<0) goto out;

 /* Rename group */
 if (H5Gmove( file_id, "/g21/g1", "/g21/g3" )<0)  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g21/g3" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g21/g2" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g22", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g22/g1", 0 ))<0) goto out;

 /* Create symbolic link to "g22/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g22/g1", "/g22/g2")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g22/g1" )!=0)
  goto out;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g22/g2" ))<0) goto out;

 /* Rename group */
 if (H5Gmove( file_id, "/g22/g2", "/g22/g3" )<0)  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g22/g1" )!=0)
  goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g22/g3" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g23", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g23/g1", 0 ))<0) goto out;

 /* Create symbolic link to "g23/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g23/g1", "/g23/g2")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g23/g1" )!=0)
  goto out;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g23/g2" ))<0) goto out;

 /* Delete group */
 if (H5Gunlink( file_id, "/g23/g1")<0) goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g23/g2" )!=0)
  goto out;

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
 if ((group_id = H5Gcreate( file_id, "/g24", 0 ))<0) goto out;
 if ((group2_id = H5Gcreate( file_id, "/g24/g1", 0 ))<0) goto out;

 /* Create symbolic link to "g24/g1/ group */
 if (H5Glink(file_id, H5G_LINK_SOFT, "/g24/g1", "/g24/g2")<0) 
  goto out;

 /* Get name */
 if (H5Iget_name( group2_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "/g24/g1" )!=0)
  goto out;

 /* Open the group */
 if ((group3_id = H5Gopen( file_id, "/g24/g2" ))<0) goto out;

 /* Delete group */
 if (H5Gunlink( file_id, "/g24/g2")<0) goto out;

 /* Get name */
 if (H5Iget_name( group3_id, name, size )<0) goto out;

 /* Verify */
 if (check_name( name, "" )!=0)
  goto out;

 /* Close */
 H5Gclose( group_id );
 H5Gclose( group2_id );
 H5Gclose( group3_id );
 
 PASSED();

 
/*-------------------------------------------------------------------------
 * end tests 
 *-------------------------------------------------------------------------
 */


 
 /* Close file */
 H5Fclose( file_id );
  
 return 0;

out:
 H5Fclose( file_id );
 H5_FAILED();
 return 1;
  
 
}

