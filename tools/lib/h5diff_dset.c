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

#include "h5diff.h"
#include "H5private.h" 
#include <assert.h>



/*-------------------------------------------------------------------------
 * Function: diff_dataset
 *
 * Purpose: check for comparable datasets and read into a compatible 
 *  memory type
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int diff_dataset( hid_t file1_id, 
                  hid_t file2_id, 
                  const char *obj1_name, 
                  const char *obj2_name, 
                  diff_opt_t *options )
{
 hid_t        dset1_id  =-1;
 hid_t        dset2_id  =-1;
 hid_t        space1_id =-1;
 hid_t        space2_id =-1;
 hid_t        f_type1=-1, f_type2=-1; /* file data type */ 
 hid_t        m_type1=-1, m_type2=-1; /* memory data type */
 size_t       m_size1, m_size2;       /* size of type in memory */
 H5T_sign_t   sign1, sign2;           /* sign of type */
 int          rank1, rank2; 
 void         *buf1=NULL, *buf2=NULL;
 hsize_t      tot_cnt1, tot_cnt2;
 hsize_t      dims1[32], dims2[32];
 hsize_t      maxdim1[32], maxdim2[32];
 H5T_class_t  tclass1;
 H5T_class_t  tclass2;
 int          nfound=0;               /* number of differences found */
 const char   *name1=NULL;            /* relative names */
 const char   *name2=NULL;
 int          maxdim_diff=0;          /* maximum dimensions are different */
 int          dim_diff=0;             /* current dimensions are different */
 int          can1, can2;             /* supported diff */
 int          i;


 /* disable error reporting */
 H5E_BEGIN_TRY {

/*-------------------------------------------------------------------------
 * open the handles
 *-------------------------------------------------------------------------
 */

 /* Open the datasets */
 if ( (dset1_id = H5Dopen(file1_id,obj1_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj1_name );
  goto out;
 }
 if ( (dset2_id = H5Dopen(file2_id,obj2_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj2_name );
  goto out;
 }
 /* enable error reporting */
 } H5E_END_TRY;

  /* Get the dataspace handle */
 if ( (space1_id = H5Dget_space(dset1_id)) < 0 )
  goto out;

 /* Get rank */
 if ( (rank1 = H5Sget_simple_extent_ndims(space1_id)) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space2_id = H5Dget_space(dset2_id)) < 0 )
  goto out;

 /* Get rank */
 if ( (rank2 = H5Sget_simple_extent_ndims(space2_id)) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space1_id,dims1,maxdim1) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space2_id,dims2,maxdim2) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Get the file data type 
 *-------------------------------------------------------------------------
 */

 /* Get the data type */
 if ( (f_type1 = H5Dget_type(dset1_id)) < 0 )
  goto out;

 /* Get the data type */
 if ( (f_type2 = H5Dget_type(dset2_id)) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * check for the same class 
 *-------------------------------------------------------------------------
 */

 if ((tclass1=H5Tget_class(f_type1))<0) 
  goto out;

 if ((tclass2=H5Tget_class(f_type2))<0) 
  goto out;

 if ( tclass1 != tclass2 )
 {
  if (options->verbose) {
   printf("Comparison not supported\n");
   printf("<%s> is of class %s and <%s> is of class %s\n", 
    obj1_name, get_class(tclass1), 
    obj2_name, get_class(tclass2) );
  }
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for non supported classes
 *-------------------------------------------------------------------------
 */

 assert(tclass1==tclass2);
 switch (tclass1) 
 {
 case H5T_TIME:
 case H5T_STRING:
 case H5T_BITFIELD:
 case H5T_OPAQUE:
 case H5T_COMPOUND:
 case H5T_REFERENCE:
 case H5T_ENUM:
 case H5T_VLEN:
 case H5T_ARRAY:
  if (options->verbose ) {
   printf("Comparison not supported\n");
   printf("<%s> is of class %s and <%s> is of class %s\n", 
    obj1_name, get_class(tclass1), 
    obj2_name, get_class(tclass2) );
  }
  goto out;
 default:
  break;
 }

/*-------------------------------------------------------------------------
 * check for the same rank
 *-------------------------------------------------------------------------
 */
 
 if ( rank1 != rank2 )
 {
  if (options->verbose) {
   printf("Comparison not supported\n");
   printf("<%s> has rank %d, dimensions ", obj1_name, rank1);
   print_dims(rank1,dims1);
   printf(", max dimensions ");
   print_dims(rank1,maxdim1);
   printf("\n" );
   printf("<%s> has rank %d, dimensions ", obj2_name, rank2);
   print_dims(rank2,dims2);
   printf(", max dimensions ");
   print_dims(rank2,maxdim2);
  }
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for different dimensions
 *-------------------------------------------------------------------------
 */
 
 assert(rank1==rank2);
 for ( i=0; i<rank1; i++) 
 {
  if ( maxdim1[i] != maxdim2[i] )
   maxdim_diff=1;
  if ( dims1[i] != dims2[i] )
   dim_diff=1;
 }

/*-------------------------------------------------------------------------
 * current dimensions
 *-------------------------------------------------------------------------
 */

 if (dim_diff==1)
 {
  if (options->verbose) {
   printf("Comparison not supported\n");
   printf("<%s> has rank %d, dimensions ", obj1_name, rank1);
   print_dims(rank1,dims1);
   printf(", max dimensions ");
   print_dims(rank1,maxdim1);
   printf("\n" );
   printf("<%s> has rank %d, dimensions ", obj2_name, rank2);
   print_dims(rank2,dims2);
   printf(", max dimensions ");
   print_dims(rank2,maxdim2);
  }
  goto out;
 }

/*-------------------------------------------------------------------------
 * maximum dimensions; just give a warning
 *-------------------------------------------------------------------------
 */
 if (maxdim_diff==1)
 {
  if (options->verbose) {
   printf( "Warning: Different maximum dimensions\n");
   printf("<%s> has max dimensions ", obj1_name);
   print_dims(rank1,maxdim1);
   printf("\n");
   printf("<%s> has max dimensions ", obj2_name);
   print_dims(rank2,maxdim2);
   printf("\n");
  }
 }
  
/*-------------------------------------------------------------------------
 * get number of elements
 *-------------------------------------------------------------------------
 */

 tot_cnt1 = 1;
 for (i = 0; i < rank1; i++) 
 {
  tot_cnt1 *= dims1[i];
 }
 
 tot_cnt2 = 1;
 for (i = 0; i < rank2; i++) 
 {
  tot_cnt2 *= dims2[i];
 }

 assert(tot_cnt1==tot_cnt2);

/*-------------------------------------------------------------------------
 * check for equal file datatype; warning only
 *-------------------------------------------------------------------------
 */

 if ( (H5Tequal(f_type1, f_type2)==0) && options->verbose) 
 {
  printf("Warning: Different storage datatype\n");
  printf("<%s> has file datatype ", obj1_name);
  print_type(f_type1);
  printf("\n");
  printf("<%s> has file datatype ", obj2_name);
  print_type(f_type2);
  printf("\n");
 }

/*-------------------------------------------------------------------------
 * memory type and sizes
 *-------------------------------------------------------------------------
 */

 m_type1 = H5Tget_native_type( f_type1 , H5T_DIR_DEFAULT);
 m_type2 = H5Tget_native_type( f_type2 , H5T_DIR_DEFAULT);

 m_size1 = H5Tget_size( m_type1 );
 m_size2 = H5Tget_size( m_type2 );

#if defined (H5DIFF_DEBUG)
 print_sizes(obj1_name,obj2_name,f_type1,f_type2,m_type1,m_type2);
#endif 

/*-------------------------------------------------------------------------
 * check for the comparable types in diff_array
 *-------------------------------------------------------------------------
 */

 can1=diff_can(m_type1);
 can2=diff_can(m_type2);
 if ( (can1==0 || can2==0))
 {
  if (options->verbose) {
   printf("Comparison not supported\n");
   if ( can1==0 )
    printf("<%s> type is not supported\n", obj1_name);
   if ( can2==0 )
    printf("<%s> type is not supported\n", obj2_name);
  }
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for different signed/unsigned types
 *-------------------------------------------------------------------------
 */

 sign1=H5Tget_sign(m_type1);
 sign2=H5Tget_sign(m_type2);
 if ( sign1 != sign2 )
 {
  if (options->verbose) {
   printf("Comparison not supported\n");
   printf("<%s> has sign %s\n", obj1_name, get_sign(sign1));
   printf("<%s> has sign %s", obj2_name, get_sign(sign2));
  }
  goto out;
 }


/*-------------------------------------------------------------------------
 * "upgrade" the smaller memory size 
 *-------------------------------------------------------------------------
 */

 if ( m_size1 != m_size2 )
 {
  if ( m_size1 < m_size2 )
  {
   assert( (H5Tclose(m_type1)) >=0);
   m_type1 = H5Tget_native_type( f_type2 , H5T_DIR_DEFAULT);
   m_size1 = H5Tget_size( m_type1 );
  }
  else
  {
   assert( (H5Tclose(m_type2)) >=0);
   m_type2 = H5Tget_native_type( f_type1 , H5T_DIR_DEFAULT);
   m_size2 = H5Tget_size( m_type2 );
  }
#if defined (H5DIFF_DEBUG)
  printf("WARNING: Size was upgraded\n");
  print_sizes(obj1_name,obj2_name,f_type1,f_type2,m_type1,m_type2);
#endif 
 }
 assert(m_size1==m_size2);

 buf1 = (void *) HDmalloc((unsigned) (tot_cnt1*m_size1));
 buf2 = (void *) HDmalloc((unsigned) (tot_cnt2*m_size2));

 if ( buf1 == NULL || buf2 == NULL )
 {
  printf( "cannot read into memory\n" );
  goto out;
 }

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 if ( H5Dread(dset1_id,m_type1,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf1) < 0 )
  goto out;

 if ( H5Dread(dset2_id,m_type2,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * array compare
 *-------------------------------------------------------------------------
 */
 if (options->verbose)
  printf( "Comparing <%s> with <%s>\n", obj1_name, obj2_name );
 name1=diff_basename(obj1_name);
 name2=diff_basename(obj2_name);
 nfound = diff_array(buf1,buf2,tot_cnt1,rank1,dims1,options,name1,name2,m_type1);
 if (options->verbose)
  printf("%d differences found\n", nfound );

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

out:

 if ( buf1) HDfree(buf1);
 if ( buf2) HDfree(buf2);
 
 /* Close */
 if ( dset1_id!=-1 )  assert( (H5Dclose(dset1_id)) >=0);
 if ( dset2_id!=-1 )  assert( (H5Dclose(dset2_id)) >=0);
 if ( space1_id!=-1 ) assert( (H5Sclose(space1_id)) >=0);
 if ( space2_id!=-1 ) assert( (H5Sclose(space2_id)) >=0);
 if ( f_type1!=-1 )   assert( (H5Tclose(f_type1)) >=0);
 if ( f_type2!=-1 )   assert( (H5Tclose(f_type2)) >=0);
 if ( m_type1!=-1 )   assert( (H5Tclose(m_type1)) >=0);
 if ( m_type2!=-1 )   assert( (H5Tclose(m_type2)) >=0);
 
 return nfound;

}

