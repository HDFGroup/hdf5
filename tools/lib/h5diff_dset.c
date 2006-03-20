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
#include "ph5diff.h"
#include "H5private.h"
#include "h5tools.h"

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
 *-------------------------------------------------------------------------
 */
hsize_t diff_dataset( hid_t file1_id,
                      hid_t file2_id,
                      const char *obj1_name,
                      const char *obj2_name,
                      diff_opt_t *options )
{
 hid_t   dset1_id=-1;
 hid_t   dset2_id=-1;
 hid_t   dcpl1_id=-1;
 hid_t   dcpl2_id=-1;
 hsize_t nfound=0;

/*-------------------------------------------------------------------------
 * open the handles
 *-------------------------------------------------------------------------
 */
 /* disable error reporting */
 H5E_BEGIN_TRY {
 /* Open the datasets */
 if ( (dset1_id = H5Dopen(file1_id,obj1_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj1_name );
  goto error;
 }
 if ( (dset2_id = H5Dopen(file2_id,obj2_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj2_name );
  goto error;
 }
 /* enable error reporting */
 } H5E_END_TRY;


 if ((dcpl1_id=H5Dget_create_plist(dset1_id))<0)
  goto error;
 if ((dcpl2_id=H5Dget_create_plist(dset2_id))<0)
  goto error;

/*-------------------------------------------------------------------------
 * check if the dataset creation property list has filters that
 * are not registered in the current configuration
 * 1) the external filters GZIP and SZIP might not be available
 * 2) the internal filters might be turned off
 *-------------------------------------------------------------------------
 */
 if ((h5tools_canreadf((options->m_verbose?obj1_name:NULL),dcpl1_id)==1) &&
     (h5tools_canreadf((options->m_verbose?obj2_name:NULL),dcpl2_id)==1))
 {
  nfound=diff_datasetid(dset1_id,
                       dset2_id,
                       obj1_name,
                       obj2_name,
                       options);
 }
/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
 /* disable error reporting */
 H5E_BEGIN_TRY {
  H5Pclose(dcpl1_id);
  H5Pclose(dcpl2_id);
  H5Dclose(dset1_id);
  H5Dclose(dset2_id);
   /* enable error reporting */
 } H5E_END_TRY;

 return nfound;

error:
 options->err_stat=1;
 /* disable error reporting */
 H5E_BEGIN_TRY {
  H5Pclose(dcpl1_id);
  H5Pclose(dcpl2_id);
  H5Dclose(dset1_id);
  H5Dclose(dset2_id);
   /* enable error reporting */
 } H5E_END_TRY;

 return nfound;

}




/*-------------------------------------------------------------------------
 * Function: diff_datasetid
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
 *-------------------------------------------------------------------------
 */
hsize_t diff_datasetid( hid_t dset1_id,
                        hid_t dset2_id,
                        const char *obj1_name,
                        const char *obj2_name,
                        diff_opt_t *options )
{
 hid_t        space1_id =-1;
 hid_t        space2_id =-1;
 hid_t        f_type1=-1, f_type2=-1; /* file data type */
 hid_t        m_type1=-1, m_type2=-1; /* memory data type */
 size_t       m_size1, m_size2;       /* size of type in memory */
 H5T_sign_t   sign1, sign2;           /* sign of type */
 int          rank1, rank2;
 void         *buf1=NULL, *buf2=NULL;
 hsize_t      nelmts1, nelmts2;
 hsize_t      dims1[H5S_MAX_RANK];
 hsize_t      dims2[H5S_MAX_RANK];
 hsize_t      maxdim1[H5S_MAX_RANK];
 hsize_t      maxdim2[H5S_MAX_RANK];
 const char   *name1=NULL;            /* relative names */
 const char   *name2=NULL;
 hsize_t      storage_size1;
 hsize_t      storage_size2;
 hsize_t      nfound=0;               /* number of differences found */
 int          cmp=1;                  /* do diff or not */
 int          i;

  /* Get the dataspace handle */
 if ( (space1_id = H5Dget_space(dset1_id)) < 0 )
  goto error;

 /* Get rank */
 if ( (rank1 = H5Sget_simple_extent_ndims(space1_id)) < 0 )
  goto error;

 /* Get the dataspace handle */
 if ( (space2_id = H5Dget_space(dset2_id)) < 0 )
  goto error;

 /* Get rank */
 if ( (rank2 = H5Sget_simple_extent_ndims(space2_id)) < 0 )
  goto error;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space1_id,dims1,maxdim1) < 0 )
  goto error;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space2_id,dims2,maxdim2) < 0 )
  goto error;

/*-------------------------------------------------------------------------
 * Get the file data type
 *-------------------------------------------------------------------------
 */

 /* Get the data type */
 if ( (f_type1 = H5Dget_type(dset1_id)) < 0 )
  goto error;

 /* Get the data type */
 if ( (f_type2 = H5Dget_type(dset2_id)) < 0 )
  goto error;


/*-------------------------------------------------------------------------
 * check for empty datasets
 *-------------------------------------------------------------------------
 */

 storage_size1=H5Dget_storage_size(dset1_id);
 storage_size2=H5Dget_storage_size(dset2_id);
 if (storage_size1<=0 && storage_size2<=0)
 {
  if (options->m_verbose && obj1_name && obj2_name)
   parallel_print("<%s> and <%s> are empty datasets\n", obj1_name, obj2_name);
  cmp=0;
  options->not_cmp=1;
 }


/*-------------------------------------------------------------------------
 * check for comparable TYPE and SPACE
 *-------------------------------------------------------------------------
 */

 if (diff_can_type(f_type1,
  f_type2,
  rank1,
  rank2,
  dims1,
  dims2,
  maxdim1,
  maxdim2,
  obj1_name,
  obj2_name,
  options)!=1)
 {
  cmp=0;
  options->not_cmp=1;
 }

/*-------------------------------------------------------------------------
 * only attempt to compare if possible
 *-------------------------------------------------------------------------
 */
 if (cmp)
 {

/*-------------------------------------------------------------------------
 * get number of elements
 *-------------------------------------------------------------------------
 */
 nelmts1 = 1;
 for (i = 0; i < rank1; i++)
 {
  nelmts1 *= dims1[i];
 }

 nelmts2 = 1;
 for (i = 0; i < rank2; i++)
 {
  nelmts2 *= dims2[i];
 }

 /* only assert if the space is the same */
 assert(nelmts1==nelmts2);

/*-------------------------------------------------------------------------
 * memory type and sizes
 *-------------------------------------------------------------------------
 */
 if ((m_type1=h5tools_get_native_type(f_type1))<0)
    goto error;

 if ((m_type2=h5tools_get_native_type(f_type2))<0)
    goto error;

 m_size1 = H5Tget_size( m_type1 );
 m_size2 = H5Tget_size( m_type2 );

#if defined (H5DIFF_DEBUG)
 if (obj1_name)
  print_sizes(obj1_name,obj2_name,f_type1,f_type2,m_type1,m_type2);
#endif

/*-------------------------------------------------------------------------
 * check for different signed/unsigned types
 *-------------------------------------------------------------------------
 */

 sign1=H5Tget_sign(m_type1);
 sign2=H5Tget_sign(m_type2);
 if ( sign1 != sign2 )
 {
  if (options->m_verbose && obj1_name) {
   parallel_print("Comparison not supported: <%s> has sign %s ", obj1_name, get_sign(sign1));
   parallel_print("and <%s> has sign %s\n", obj2_name, get_sign(sign2));
  }
  
  cmp=0;
  options->not_cmp=1;
 }

/*-------------------------------------------------------------------------
 * "upgrade" the smaller memory size
 *-------------------------------------------------------------------------
 */

 if ( m_size1 != m_size2 )
 {
  if ( m_size1 < m_size2 )
  {
   H5Tclose(m_type1);
   
   if ((m_type1=h5tools_get_native_type(f_type2))<0)
    goto error;
   
   m_size1 = H5Tget_size( m_type1 );
  }
  else
  {
   H5Tclose(m_type2);
   
   if ((m_type2=h5tools_get_native_type(f_type1))<0)
    goto error;
   
   m_size2 = H5Tget_size( m_type2 );
  }
#if defined (H5DIFF_DEBUG)
  printf("WARNING: Size was upgraded\n");
  if (obj1_name)
   print_sizes(obj1_name,obj2_name,f_type1,f_type2,m_type1,m_type2);
#endif
 }
 assert(m_size1==m_size2);


 buf1 = (void *) HDmalloc((unsigned) (nelmts1*m_size1));
 buf2 = (void *) HDmalloc((unsigned) (nelmts2*m_size2));

 if ( buf1 == NULL || buf2 == NULL )
 {
  printf( "cannot read into memory\n" );
  goto error;
 }

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 if ( H5Dread(dset1_id,m_type1,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf1) < 0 )
  goto error;

 if ( H5Dread(dset2_id,m_type2,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2) < 0 )
  goto error;

/*-------------------------------------------------------------------------
 * array compare
 *-------------------------------------------------------------------------
 */
 if (obj1_name) {
  name1=diff_basename(obj1_name);
 }
 if (obj2_name) {
  name2=diff_basename(obj2_name);
 }
 nfound = diff_array(buf1,
                     buf2,
                     nelmts1,
                     rank1,
                     dims1,
                     options,
                     name1,
                     name2,
                     m_type1,
                     dset1_id,
                     dset2_id);
/*-------------------------------------------------------------------------
 * compare attributes
 * the if condition refers to cases when the dataset is a referenced object
 *-------------------------------------------------------------------------
 */

 if (obj1_name)
  diff_attr(dset1_id,dset2_id,obj1_name,obj2_name,options);

 }/*cmp*/

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */


 if ( buf1) HDfree(buf1);
 if ( buf2) HDfree(buf2);
 /* close */
 /* disable error reporting */
 H5E_BEGIN_TRY {
  H5Sclose(space1_id);
  H5Sclose(space2_id);
  H5Tclose(f_type1);
  H5Tclose(f_type2);
  H5Tclose(m_type1);
  H5Tclose(m_type2);
   /* enable error reporting */
 } H5E_END_TRY;

 return nfound;

error:
 options->err_stat=1;
 if ( buf1) HDfree(buf1);
 if ( buf2) HDfree(buf2);
 /* close */
 /* disable error reporting */
 H5E_BEGIN_TRY {
  H5Sclose(space1_id);
  H5Sclose(space2_id);
  H5Tclose(f_type1);
  H5Tclose(f_type2);
  H5Tclose(m_type1);
  H5Tclose(m_type2);
   /* enable error reporting */
 } H5E_END_TRY;

 return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_can_type
 *
 * Purpose: check for comparable TYPE and SPACE
 *
 * Return:
 *  1, can compare
 *  0, cannot compare
 * -1, error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 3, 2003
 *
 *-------------------------------------------------------------------------
 */

int diff_can_type( hid_t       f_type1, /* file data type */
                   hid_t       f_type2, /* file data type */
                   int         rank1,
                   int         rank2,
                   hsize_t     *dims1,
                   hsize_t     *dims2,
                   hsize_t     *maxdim1,
                   hsize_t     *maxdim2,
                   const char  *obj1_name,
                   const char  *obj2_name,
                   diff_opt_t  *options )
{


 H5T_class_t  tclass1;
 H5T_class_t  tclass2;
 int          maxdim_diff=0;          /* maximum dimensions are different */
 int          dim_diff=0;             /* current dimensions are different */
 int          i;

/*-------------------------------------------------------------------------
 * check for the same class
 *-------------------------------------------------------------------------
 */

 if ((tclass1=H5Tget_class(f_type1))<0)
  return -1;

 if ((tclass2=H5Tget_class(f_type2))<0)
  return -1;

 if ( tclass1 != tclass2 )
 {
  if (options->m_verbose && obj1_name) {
   printf("Comparison not possible: <%s> is of class %s and <%s> is of class %s\n",
    obj1_name, get_class(tclass1),
    obj2_name, get_class(tclass2) );
  }
  return 0;
 }

/*-------------------------------------------------------------------------
 * check for non supported classes
 *-------------------------------------------------------------------------
 */

 assert(tclass1==tclass2);
 switch (tclass1)
 {
 case H5T_INTEGER:
 case H5T_FLOAT:
 case H5T_COMPOUND:
 case H5T_STRING:
 case H5T_ARRAY:
 case H5T_BITFIELD:
 case H5T_OPAQUE:
 case H5T_ENUM:
 case H5T_VLEN:
 case H5T_REFERENCE:

  break;

 default: /*H5T_TIME */
  if (options->m_verbose && obj1_name )
   printf("Comparison not supported: <%s> and <%s> are of class %s\n",
    obj1_name,obj2_name,get_class(tclass2) );
  return 0;
 }

/*-------------------------------------------------------------------------
 * check for equal file datatype; warning only
 *-------------------------------------------------------------------------
 */

 if ( (H5Tequal(f_type1, f_type2)==0) && options->m_verbose && obj1_name)
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
 * check for the same rank
 *-------------------------------------------------------------------------
 */

 if ( rank1 != rank2 )
 {
  if (options->m_verbose && obj1_name) {
   printf("Comparison not supported: <%s> has rank %d, dimensions ", obj1_name, rank1);
   print_dims(rank1,dims1);
   printf(", max dimensions ");
   print_dims(rank1,maxdim1);
   printf("\n" );
   printf("<%s> has rank %d, dimensions ", obj2_name, rank2);
   print_dims(rank2,dims2);
   printf(", max dimensions ");
   print_dims(rank2,maxdim2);
  }
  return 0;
 }

/*-------------------------------------------------------------------------
 * check for different dimensions
 *-------------------------------------------------------------------------
 */

 assert(rank1==rank2);
 for ( i=0; i<rank1; i++)
 {
  if (maxdim1 && maxdim2)
  {
   if ( maxdim1[i] != maxdim2[i] )
    maxdim_diff=1;
  }
  if ( dims1[i] != dims2[i] )
   dim_diff=1;
 }

/*-------------------------------------------------------------------------
 * current dimensions
 *-------------------------------------------------------------------------
 */

 if (dim_diff==1)
 {
  if (options->m_verbose && obj1_name) {
   printf("Comparison not supported: <%s> has rank %d, dimensions ", obj1_name, rank1);
   print_dims(rank1,dims1);
   if (maxdim1 && maxdim2) {
    printf(", max dimensions ");
    print_dims(rank1,maxdim1);
    printf("\n" );
    printf("<%s> has rank %d, dimensions ", obj2_name, rank2);
    print_dims(rank2,dims2);
    printf(", max dimensions ");
    print_dims(rank2,maxdim2);
   }
  }
  return 0;
 }

/*-------------------------------------------------------------------------
 * maximum dimensions; just give a warning
 *-------------------------------------------------------------------------
 */
 if (maxdim1 && maxdim2 && maxdim_diff==1 && obj1_name )
 {
  if (options->m_verbose) {
   printf( "Warning: Different maximum dimensions\n");
   printf("<%s> has max dimensions ", obj1_name);
   print_dims(rank1,maxdim1);
   printf("\n");
   printf("<%s> has max dimensions ", obj2_name);
   print_dims(rank2,maxdim2);
   printf("\n");
  }
 }

 return 1;
}
