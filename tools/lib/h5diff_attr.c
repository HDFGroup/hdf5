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
 * Function: diff_attr
 *
 * Purpose: diff attributes located in LOC1_ID and LOC2_ID, which are 
 *  obtained either from 
 * loc_id = H5Gopen( fid, name);
 * loc_id = H5Dopen( fid, name);
 * loc_id = H5Topen( fid, name);
 *
 * Return: 
 *  0 : no differences found
 *  1 : differences found
 * -1 : error ocurred 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November, 03, 2003
 *
 *-------------------------------------------------------------------------
 */

int diff_attr(hid_t loc1_id, 
              hid_t loc2_id, 
              diff_opt_t *options
              )
{
 hid_t      attr1_id;     /* attr ID */ 
 hid_t      attr2_id;     /* attr ID */ 
 hid_t      space1_id;    /* space ID */ 
 hid_t      space2_id;    /* space ID */ 
 hid_t      ftype1_id;    /* file data type ID */ 
 hid_t      ftype2_id;    /* file data type ID */ 
 hid_t      mtype1_id;    /* memory data type ID */
 hid_t      mtype2_id;    /* memory data type ID */
	size_t     msize1;       /* memory size of memory type */
 size_t     msize2;       /* memory size of memory type */
 void       *buf1=NULL;   /* data buffer */
 void       *buf2=NULL;   /* data buffer */
 hsize_t    nelmts1;      /* number of elements in dataset */
 int        rank1;        /* rank of dataset */
 int        rank2;        /* rank of dataset */
 hsize_t    dims1[H5S_MAX_RANK];/* dimensions of dataset */
 hsize_t    dims2[H5S_MAX_RANK];/* dimensions of dataset */
	char       name1[255];
 char       name2[255];
	int        n1, n2, i, j, nfound;

 if ((n1 = H5Aget_num_attrs(loc1_id))<0) 
  goto error;
 if ((n2 = H5Aget_num_attrs(loc2_id))<0) 
  goto error;

 if (n1!=n2)
  return 1;
 
 for ( i = 0; i < n1; i++)
 {

  /* reset buffers for every attribute, we might goto out and call free */
  buf1=NULL;   
  buf2=NULL;   

/*-------------------------------------------------------------------------
 * open
 *-------------------------------------------------------------------------
 */
  /* open attribute */
  if ((attr1_id = H5Aopen_idx(loc1_id, i))<0) 
   goto error;
  if ((attr2_id = H5Aopen_idx(loc2_id, i))<0) 
   goto error;
  
  /* get name */
  if (H5Aget_name( attr1_id, 255, name1 )<0) 
   goto error;
  if (H5Aget_name( attr1_id, 255, name2 )<0) 
   goto error;

  if (HDstrcmp(name1,name2)!=0)
  {
   if (options->verbose) 
   {
    printf("Different name for attributes: <%s> and <%s>\n", name1, name2);
   }
   H5Aclose(attr1_id);
   H5Aclose(attr2_id);
   return 1;
  }
  
		/* get the file datatype  */
		if ((ftype1_id = H5Aget_type( attr1_id )) < 0 )
			goto error;
  if ((ftype2_id = H5Aget_type( attr2_id )) < 0 )
			goto error;
		
		/* get the dataspace handle  */
		if ((space1_id = H5Aget_space( attr1_id )) < 0 )
			goto error;
  if ((space2_id = H5Aget_space( attr2_id )) < 0 )
			goto error;
		
		/* get dimensions  */
		if ( (rank1 = H5Sget_simple_extent_dims(space1_id, dims1, NULL)) < 0 )
			goto error;
  if ( (rank2 = H5Sget_simple_extent_dims(space2_id, dims2, NULL)) < 0 )
			goto error;


/*-------------------------------------------------------------------------
 * check for comparable TYPE and SPACE
 *-------------------------------------------------------------------------
 */

 if (diff_can_type(ftype1_id, 
  ftype2_id, 
  rank1, 
  rank2,
  dims1, 
  dims2,
  NULL, 
  NULL,
  name1, 
  name2, 
  options)!=1)
  goto error;
	
/*-------------------------------------------------------------------------
 * read to memory
 *-------------------------------------------------------------------------
 */
 nelmts1=1;
 for (j=0; j<rank1; j++) 
  nelmts1*=dims1[j];
 if ((mtype1_id=H5Tget_native_type(ftype1_id,H5T_DIR_DEFAULT))<0)
  goto error;
 if ((mtype2_id=H5Tget_native_type(ftype2_id,H5T_DIR_DEFAULT))<0)
  goto error;
 if ((msize1=H5Tget_size(mtype1_id))<0)
  goto error;
 if ((msize2=H5Tget_size(mtype2_id))<0)
  goto error;
 
 assert(msize1==msize2);
 
 buf1=(void *) HDmalloc((unsigned)(nelmts1*msize1));
 buf2=(void *) HDmalloc((unsigned)(nelmts1*msize2));
 if ( buf1==NULL || buf2==NULL){
  printf( "cannot read into memory\n" );
  goto error;
 }
 if (H5Aread(attr1_id,mtype1_id,buf1)<0)
  goto error;
 if (H5Aread(attr2_id,mtype2_id,buf2)<0)
  goto error;

   
/*-------------------------------------------------------------------------
 * array compare
 *-------------------------------------------------------------------------
 */

 if (options->verbose)
  printf( "Comparing <%s> with <%s>\n", name1, name2 );
 nfound = diff_array(buf1,buf2,nelmts1,rank1,dims1,options,name1,name2,mtype1_id);
 if (options->verbose)
  printf("%d attribute differences found\n", nfound );

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

 if (H5Tclose(ftype1_id)<0) goto error;
 if (H5Tclose(ftype2_id)<0) goto error;
 if (H5Tclose(mtype1_id)<0) goto error;
 if (H5Tclose(mtype2_id)<0) goto error;
 if (H5Sclose(space1_id)<0) goto error;
 if (H5Sclose(space2_id)<0) goto error;
 if (H5Aclose(attr1_id)<0) goto error;
 if (H5Aclose(attr2_id)<0) goto error;
 if (buf1)
  HDfree(buf1);
 if (buf2)
  HDfree(buf2);
 } /* i */
	
 return 0;

error:
 H5E_BEGIN_TRY {
  H5Tclose(ftype1_id);
  H5Tclose(ftype2_id);
		H5Tclose(mtype1_id);
  H5Tclose(mtype2_id);
		H5Sclose(space1_id);
  H5Sclose(space2_id);
		H5Aclose(attr1_id);
  H5Aclose(attr2_id);
  if (buf1)
   HDfree(buf1);
  if (buf2)
   HDfree(buf2);
 } H5E_END_TRY;
 return -1;
}


