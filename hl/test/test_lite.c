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

#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#if H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 6
#include "H5LT.h"
#else
#include <hdf5_hl.h>
#endif


#define FILE_NAME "test_lite1.h5"
#define FILE_NAME2 "test_lite2.h5"

#define DSET0_NAME "2D int array"
#define DSET1_NAME "dataset char"
#define DSET2_NAME "dataset short"
#define DSET3_NAME "dataset int"
#define DSET4_NAME "dataset long"
#define DSET5_NAME "dataset float"
#define DSET6_NAME "dataset double"
#define DSET7_NAME "dataset string"

#define DIM 6

#define ATTR1_NAME "attr string"
#define ATTR2_NAME "attr char"
#define ATTR3_NAME "attr short"
#define ATTR4_NAME "attr int"
#define ATTR5_NAME "attr long"
#define ATTR6_NAME "attr uchar"
#define ATTR7_NAME "attr ushort"
#define ATTR8_NAME "attr uint"
#define ATTR9_NAME "attr ulong"
#define ATTR10_NAME "attr float"
#define ATTR11_NAME "attr double"

static herr_t make_attributes( hid_t loc_id, const char* obj_name );



/*-------------------------------------------------------------------------
 * test dataset functions
 *-------------------------------------------------------------------------
 */

static int test_dsets( void )
{
 int     rank     = 2;
 hsize_t dims[2]  = {2,3};
 hid_t   file_id;
 hid_t   dataset_id;
 char    data_char_in[DIM]    = {1,2,3,4,5,6};
 char    data_char_out[DIM];
 short   data_short_in[DIM]   = {1,2,3,4,5,6};
 short   data_short_out[DIM];
 int     data_int_in[DIM]     = {1,2,3,4,5,6};
 int     data_int_out[DIM];
 long    data_long_in[DIM]    = {1,2,3,4,5,6};
 long    data_long_out[DIM];
 float   data_float_in[DIM]   = {1,2,3,4,5,6};
 float   data_float_out[DIM];
 double  data_double_in[DIM]  = {1,2,3,4,5,6};
 double  data_double_out[DIM];
	char    *data_string_in = "This is a string";
	char    data_string_out[20];
 int     i;


 /* Create a new file using default properties. */
 file_id = H5Fcreate( FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

/*-------------------------------------------------------------------------
 * H5LTmake_dataset test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset");

 /* Make dataset */
 if ( H5LTmake_dataset( file_id, DSET0_NAME, rank, dims, H5T_NATIVE_INT, data_int_in ) < 0 )
  goto out;

 /* Read dataset using the basic HDF5 API */

 if ( ( dataset_id = H5Dopen ( file_id, DSET0_NAME) ) < 0 )
  goto out;

 if ( H5Dread ( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_int_out ) < 0 )
  goto out;

 if ( H5Dclose( dataset_id ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_int_in[i] != data_int_out[i] ) {
    goto out;
  }
 }

 PASSED();

/*-------------------------------------------------------------------------
 * read using the LT function H5LTread_dataset
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTread_dataset");

 if ( H5LTread_dataset( file_id, DSET0_NAME, H5T_NATIVE_INT, data_int_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_int_in[i] != data_int_out[i] ) {
    goto out;
  }
 }

 PASSED();

/*-------------------------------------------------------------------------
 * test the H5LTmake_dataset_ functions
 *-------------------------------------------------------------------------
 */


/*-------------------------------------------------------------------------
 * H5LTmake_dataset_char
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_char");

 /* Make dataset char */
 if ( H5LTmake_dataset_char( file_id, DSET1_NAME, rank, dims, data_char_in ) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset( file_id, DSET1_NAME, H5T_NATIVE_CHAR, data_char_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_char_in[i] != data_char_out[i] ) {
    goto out;
  }
 }

 /* Read dataset */
 if ( H5LTread_dataset_char( file_id, DSET1_NAME, data_char_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_char_in[i] != data_char_out[i] ) {
    goto out;
  }
 }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTmake_dataset_short
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_short");

 /* Make dataset short */
 if ( H5LTmake_dataset_short( file_id, DSET2_NAME, rank, dims, data_short_in ) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset( file_id, DSET2_NAME, H5T_NATIVE_SHORT, data_short_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_short_in[i] != data_short_out[i] ) {
    goto out;
  }
 }

 /* Read dataset */
 if ( H5LTread_dataset_short( file_id, DSET2_NAME, data_short_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_short_in[i] != data_short_out[i] ) {
    goto out;
  }
 }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTmake_dataset_int
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_int");

 /* Make dataset int */
 if ( H5LTmake_dataset_int( file_id, DSET3_NAME, rank, dims, data_int_in ) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset( file_id, DSET3_NAME, H5T_NATIVE_INT, data_int_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_int_in[i] != data_int_out[i] ) {
    goto out;
  }
 }

 /* Read dataset */
 if ( H5LTread_dataset_int( file_id, DSET3_NAME, data_int_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_int_in[i] != data_int_out[i] ) {
    goto out;
  }
 }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTmake_dataset_long
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_long");

 /* Make dataset long */
 if ( H5LTmake_dataset_long( file_id, DSET4_NAME, rank, dims, data_long_in ) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset( file_id, DSET4_NAME, H5T_NATIVE_LONG, data_long_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_long_in[i] != data_long_out[i] ) {
    goto out;
  }
 }

  /* Read dataset */
 if ( H5LTread_dataset_long( file_id, DSET4_NAME, data_long_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_long_in[i] != data_long_out[i] ) {
    goto out;
  }
 }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTmake_dataset_float
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_float");

 /* Make dataset float */
 if ( H5LTmake_dataset_float( file_id, DSET5_NAME, rank, dims, data_float_in ) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset( file_id, DSET5_NAME, H5T_NATIVE_FLOAT, data_float_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_float_in[i] != data_float_out[i] ) {
    goto out;
  }
 }

  /* Read dataset */
 if ( H5LTread_dataset_float( file_id, DSET5_NAME, data_float_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_float_in[i] != data_float_out[i] ) {
    goto out;
  }
 }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTmake_dataset_double
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_double");

 /* Make dataset double */
 if ( H5LTmake_dataset_double( file_id, DSET6_NAME, rank, dims, data_double_in ) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset( file_id, DSET6_NAME, H5T_NATIVE_DOUBLE, data_double_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_double_in[i] != data_double_out[i] ) {
    goto out;
  }
 }

  /* Read dataset */
 if ( H5LTread_dataset_double( file_id, DSET6_NAME, data_double_out ) < 0 )
  goto out;

 for (i = 0; i < DIM; i++)
 {
  if ( data_double_in[i] != data_double_out[i] ) {
    goto out;
  }
 }

	PASSED();


/*-------------------------------------------------------------------------
 * H5LTmake_dataset_string
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTmake_dataset_string");

 /* Make dataset string */
 if ( H5LTmake_dataset_string(file_id,DSET7_NAME,data_string_in) < 0 )
  goto out;

 /* Read dataset */
 if ( H5LTread_dataset_string(file_id,DSET7_NAME,data_string_out) < 0 )
  goto out;

	if ( strcmp(data_string_in,data_string_out) != 0 )
		goto out;



/*-------------------------------------------------------------------------
 * end tests
 *-------------------------------------------------------------------------
 */

 /* Close the file. */
 H5Fclose( file_id );

 PASSED();


 return 0;

out:
 /* Close the file. */
 H5_FAILED();
 return -1;
}

/*-------------------------------------------------------------------------
 * test attribute functions
 *-------------------------------------------------------------------------
 */

static int test_attr( void )
{

 hid_t   file_id;
 hid_t   dataset_id;
 hid_t   group_id;
 hid_t   space_id;
 hsize_t dims[1] = { 5 };

 /* Create a new file using default properties. */
 file_id = H5Fcreate( FILE_NAME2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

/*-------------------------------------------------------------------------
 * Create a dataset named "dset" on the root group
 *-------------------------------------------------------------------------
 */

 /* Create the data space  */
 if ((space_id = H5Screate_simple( 1, dims, NULL ))<0) goto out;

 /* Create the dataset */
 if ((dataset_id = H5Dcreate( file_id , "dset", H5T_NATIVE_INT, space_id,
  H5P_DEFAULT ))<0) goto out;

 /* Close */
 H5Dclose( dataset_id );

/*-------------------------------------------------------------------------
 * Create a group named "grp" on the root group
 *-------------------------------------------------------------------------
 */

 /* Create a group. */
 if ((group_id = H5Gcreate( file_id, "grp", 0 ))<0)
  goto out;

 /* Close */
 H5Gclose( group_id );

/*-------------------------------------------------------------------------
 *
 * Create attributes in the root group
	* Note that we are calling the H5LTset_attribute functions with the name "."
 *
 *-------------------------------------------------------------------------
 */
 if (make_attributes( file_id, "." )<0)
  goto out;

/*-------------------------------------------------------------------------
 *
 * Create attributes in the dataset "dset"
 *
 *-------------------------------------------------------------------------
 */
 if (make_attributes( file_id, "dset" )<0)
  goto out;

/*-------------------------------------------------------------------------
 *
 * Create attributes in the group "grp"
	*
 *-------------------------------------------------------------------------
 */
 if (make_attributes( file_id, "grp" )<0)
  goto out;

/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */
 /* Close the file. */
 H5Fclose( file_id );

 return 0;

out:
 /* Close the file. */
	H5Fclose( file_id );
 H5_FAILED();
 return -1;
}

/*-------------------------------------------------------------------------
 * make_attributes
 *-------------------------------------------------------------------------
 */

static herr_t make_attributes( hid_t loc_id, const char* obj_name )
{

 int         rank_out;
 hsize_t     *dims_out = 0;
 H5T_class_t type_class;
 size_t      type_size;
 int         i;

 char    attr_str_in[]     = {"My attribute"};
 char    attr_str_out[20];
 char    attr_char_in[5]   = {1,2,3,4,5};
 char    attr_char_out[5];
 short   attr_short_in[5]  = {1,2,3,4,5};
 short   attr_short_out[5];
 int     attr_int_in[5]    = {1,2,3,4,5};
 int     attr_int_out[5];
 long    attr_long_in[5]   = {1,2,3,4,5};
 long    attr_long_out[5];
 float   attr_float_in[5]  = {1,2,3,4,5};
 float   attr_float_out[5];
 double  attr_double_in[5] = {1,2,3,4,5};
 double  attr_double_out[5];
 unsigned char    attr_uchar_in[5]   = {1,2,3,4,5};
 unsigned char    attr_uchar_out[5];
 unsigned short   attr_ushort_in[5]  = {1,2,3,4,5};
 unsigned short   attr_ushort_out[5];
 unsigned int     attr_uint_in[5]    = {1,2,3,4,5};
 unsigned int     attr_uint_out[5];
 unsigned long    attr_ulong_in[5]   = {1,2,3,4,5};
 unsigned long    attr_ulong_out[5];

/*-------------------------------------------------------------------------
 * H5LTset_attribute_string test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_string");

 /* Set the attribute */
 if ( H5LTset_attribute_string( loc_id, obj_name, ATTR1_NAME, attr_str_in ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_string test
 *-------------------------------------------------------------------------
 */

	TESTING("H5LTget_attribute_string");


 /* Get the attribute */
 if ( H5LTget_attribute_string( loc_id, obj_name, ATTR1_NAME, attr_str_out ) < 0 )
  return -1;

 if ( strcmp( attr_str_in, attr_str_out ) != 0 )
 {
  return -1;
 }

	PASSED();


/*-------------------------------------------------------------------------
 * H5LTset_attribute_char test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_char");

 /* Set the attribute */
 if ( H5LTset_attribute_char( loc_id, obj_name, ATTR2_NAME, attr_char_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_char test
 *-------------------------------------------------------------------------
 */

	TESTING("H5LTget_attribute_char");

 /* Get the attribute */
 if ( H5LTget_attribute_char( loc_id, obj_name, ATTR2_NAME, attr_char_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_char_in[i] != attr_char_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR2_NAME, H5T_NATIVE_CHAR, attr_char_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_char_in[i] != attr_char_out[i] ) {
    return -1;
   }
  }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_short test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_short");

 /* Set the attribute */
 if ( H5LTset_attribute_short( loc_id, obj_name, ATTR3_NAME, attr_short_in, 5 ) < 0 )
  return -1;

	PASSED();


/*-------------------------------------------------------------------------
 * H5LTget_attribute_short test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_short");

 /* Get the attribute */
 if ( H5LTget_attribute_short( loc_id, obj_name, ATTR3_NAME, attr_short_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_short_in[i] != attr_short_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR3_NAME, H5T_NATIVE_SHORT, attr_short_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_short_in[i] != attr_short_out[i] ) {
    return -1;
   }
  }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTset_attribute_int test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_int");

 /* Set the attribute */
 if ( H5LTset_attribute_int( loc_id, obj_name, ATTR4_NAME, attr_int_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_int test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_int");

 /* Get the attribute */
 if ( H5LTget_attribute_int( loc_id, obj_name, ATTR4_NAME, attr_int_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_int_in[i] != attr_int_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR4_NAME, H5T_NATIVE_INT, attr_int_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_int_in[i] != attr_int_out[i] ) {
    return -1;
   }
  }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_long test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_long");

 /* Set the attribute */
 if ( H5LTset_attribute_long( loc_id, obj_name, ATTR5_NAME, attr_long_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_long test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_long");

 /* Get the attribute */
 if ( H5LTget_attribute_long( loc_id, obj_name, ATTR5_NAME, attr_long_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_long_in[i] != attr_long_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR5_NAME, H5T_NATIVE_LONG, attr_long_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_long_in[i] != attr_long_out[i] ) {
    return -1;
   }
  }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_uchar test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_uchar");

 /* Set the attribute */
 if ( H5LTset_attribute_uchar( loc_id, obj_name, ATTR6_NAME, attr_uchar_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_uchar test
 *-------------------------------------------------------------------------
 */

	TESTING("H5LTget_attribute_uchar");

 /* Get the attribute */
 if ( H5LTget_attribute_uchar( loc_id, obj_name, ATTR6_NAME, attr_uchar_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_uchar_in[i] != attr_uchar_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR6_NAME, H5T_NATIVE_UCHAR, attr_uchar_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_uchar_in[i] != attr_uchar_out[i] ) {
    return -1;
   }
  }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_ushort test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_ushort");

 /* Set the attribute */
 if ( H5LTset_attribute_ushort( loc_id, obj_name, ATTR7_NAME, attr_ushort_in, 5 ) < 0 )
  return -1;

	PASSED();


/*-------------------------------------------------------------------------
 * H5LTget_attribute_ushort test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_ushort");

 /* Get the attribute */
 if ( H5LTget_attribute_ushort( loc_id, obj_name, ATTR7_NAME, attr_ushort_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_ushort_in[i] != attr_ushort_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR7_NAME, H5T_NATIVE_USHORT, attr_ushort_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_ushort_in[i] != attr_ushort_out[i] ) {
    return -1;
   }
  }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTset_attribute_int test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_uint");

 /* Set the attribute */
 if ( H5LTset_attribute_uint( loc_id, obj_name, ATTR8_NAME, attr_uint_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_int test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_uint");

 /* Get the attribute */
 if ( H5LTget_attribute_uint( loc_id, obj_name, ATTR8_NAME, attr_uint_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_uint_in[i] != attr_uint_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR8_NAME, H5T_NATIVE_UINT, attr_uint_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_uint_in[i] != attr_uint_out[i] ) {
    return -1;
   }
  }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_ulong test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_ulong");

 /* Set the attribute */
 if ( H5LTset_attribute_ulong( loc_id, obj_name, ATTR9_NAME, attr_ulong_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_long test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_ulong");

 /* Get the attribute */
 if ( H5LTget_attribute_ulong( loc_id, obj_name, ATTR9_NAME, attr_ulong_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_ulong_in[i] != attr_ulong_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR9_NAME, H5T_NATIVE_ULONG, attr_ulong_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_ulong_in[i] != attr_ulong_out[i] ) {
    return -1;
   }
  }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTset_attribute_float test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_float");

 /* Set the attribute */
 if ( H5LTset_attribute_float( loc_id, obj_name, ATTR10_NAME, attr_float_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_float test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_float");


 /* Get the attribute */
 if ( H5LTget_attribute_float( loc_id, obj_name, ATTR10_NAME, attr_float_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_float_in[i] != attr_float_out[i] ) {
    return -1;
   }
  }

	/* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR10_NAME, H5T_NATIVE_FLOAT, attr_float_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_float_in[i] != attr_float_out[i] ) {
    return -1;
   }
  }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTset_attribute_double test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTset_attribute_double");

 /* Set the attribute */
 if ( H5LTset_attribute_double( loc_id, obj_name, ATTR11_NAME, attr_double_in, 5 ) < 0 )
  return -1;

	PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_double test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_double");

 /* Get the attribute */
 if ( H5LTget_attribute_double( loc_id, obj_name, ATTR11_NAME, attr_double_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_double_in[i] != attr_double_out[i] ) {
    return -1;
   }
  }

 /* Get the attribute */
 if ( H5LTget_attribute( loc_id, obj_name, ATTR11_NAME, H5T_NATIVE_DOUBLE, attr_double_out ) < 0 )
  return -1;

 for (i = 0; i < 5; i++)
  {
   if ( attr_double_in[i] != attr_double_out[i] ) {
    return -1;
   }
  }

 PASSED();


/*-------------------------------------------------------------------------
 * H5LTget_attribute_ndims test
 *-------------------------------------------------------------------------
 */


 TESTING("H5LTget_attribute_ndims");

 if ( H5LTget_attribute_ndims( loc_id, obj_name, ATTR2_NAME, &rank_out ) < 0 )
  return -1;

 if ( rank_out != 1 ) {
  return -1;
 }

 PASSED();

/*-------------------------------------------------------------------------
 * H5LTget_attribute_info test
 *-------------------------------------------------------------------------
 */

 TESTING("H5LTget_attribute_info");

 dims_out = malloc( sizeof(hsize_t) * rank_out );

 if ( H5LTget_attribute_info( loc_id, obj_name, ATTR2_NAME, dims_out, &type_class,
       &type_size) < 0 )
  return -1;

  for (i = 0; i < rank_out; i++)
  {
   if ( dims_out[i] != 5 ) {
    return -1;
   }
  }

 if ( type_class != H5T_INTEGER ) {
   return -1;
  }

 if ( dims_out )
  free( dims_out );

  PASSED();

 return 0;
}

/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */

int main( void )
{
 int  nerrors=0;

 /* test dataset functions */
 nerrors += test_dsets();

 /* test attribute functions */
 nerrors += test_attr();

 /* check for errors */
 if (nerrors)
  goto error;

 return 0;

error:
 return 1;


}

