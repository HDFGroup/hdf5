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


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "H5private.h"
#include "h5repack.h"


/*-------------------------------------------------------------------------
 * Function: do_copy_refobjs
 *
 * Purpose: duplicate all referenced HDF5 objects in the file 
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December, 10, 2003
 *
 *-------------------------------------------------------------------------
 */

int do_copy_refobjs(hid_t fidin, 
                    hid_t fidout, 
                    int nobjects,        /* number of objects */
                    trav_info_t *travi,  /* array of object names */
                    pack_opt_t *options) /* repack options */
{
 hid_t     grp_in;       /* group ID */ 
 hid_t     grp_out;      /* group ID */ 
 hid_t     dset_in;      /* read dataset ID */ 
 hid_t     dset_out;     /* write dataset ID */ 
 hid_t     type_in;      /* named type ID */ 
 hid_t     dcpl_id;      /* dataset creation property list ID */ 
 hid_t     space_id;     /* space ID */ 
 hid_t     ftype_id;     /* file data type ID */ 
 hid_t     mtype_id;     /* memory data type ID */
 size_t    msize;        /* memory size of memory type */
 hsize_t   nelmts;       /* number of elements in dataset */
 int       rank;         /* rank of dataset */
 hsize_t   dims[H5S_MAX_RANK];/* dimensions of dataset */
 int       i, j;

 /* initialize to test for non reference cases */
 
/*-------------------------------------------------------------------------
 * browse
 *-------------------------------------------------------------------------
 */
 
 for ( i = 0; i < nobjects; i++)
 {

  switch ( travi[i].type )
  {
 /*-------------------------------------------------------------------------
  * H5G_GROUP
  *-------------------------------------------------------------------------
  */
  case H5G_GROUP:
 
   if((grp_in = H5Gopen (fidin,travi[i].name))<0) 
    goto error;

   if (H5Gclose(grp_in)<0) 
    goto error;
  
   break;
   
  /*-------------------------------------------------------------------------
   * H5G_DATASET
   *-------------------------------------------------------------------------
   */
  case H5G_DATASET:
   
   if ((dset_in=H5Dopen(fidin,travi[i].name))<0) 
    goto error;
   if ((space_id=H5Dget_space(dset_in))<0) 
    goto error;
   if ((ftype_id=H5Dget_type (dset_in))<0) 
    goto error;
   if ((dcpl_id=H5Dget_create_plist(dset_in))<0) 
    goto error;
   if ( (rank=H5Sget_simple_extent_ndims(space_id))<0)
    goto error;
   if ( H5Sget_simple_extent_dims(space_id,dims,NULL)<0)
    goto error;
   nelmts=1;
   for (j=0; j<rank; j++) 
    nelmts*=dims[j];
   if ((mtype_id=H5Tget_native_type(ftype_id,H5T_DIR_DEFAULT))<0)
    goto error;
   if ((msize=H5Tget_size(mtype_id))==0)
    goto error;
   

/*-------------------------------------------------------------------------
 * object references are a special case
 * we cannot just copy the buffers, but instead we recreate the reference 
 *-------------------------------------------------------------------------
 */
   if (H5Tequal(mtype_id, H5T_STD_REF_OBJ)) 
   {
    H5G_obj_t   obj_type;
    hid_t       refobj_id;
    hobj_ref_t  *refbuf;
    const char* refname;
    hobj_ref_t  *buf;

   /*-------------------------------------------------------------------------
    * read to memory
    *-------------------------------------------------------------------------
    */
    
    buf=(void *) HDmalloc((unsigned)(nelmts*msize));
    if ( buf==NULL){
     printf( "cannot read into memory\n" );
     goto error;
    }
    if (H5Dread(dset_in,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
     goto error;

    
    if ((obj_type = H5Rget_obj_type(dset_in,H5R_OBJECT,buf))<0)
     goto error;
    refbuf=HDmalloc((unsigned)nelmts*msize);
    if ( refbuf==NULL){
     printf( "cannot allocate memory\n" );
     goto error;
    }
    for ( j=0; j<nelmts; j++)
    {
     if ((refobj_id = H5Rdereference(dset_in,H5R_OBJECT,&buf[j]))<0)
      goto error;
     
      /* get the name. a valid name could only occur in the 
     second traversal of the file */
     if ((refname=MapIdToName(refobj_id,nobjects,travi,options))!=NULL)
     {
      /* create the reference */
      if (H5Rcreate(&refbuf[j],fidout,refname,H5R_OBJECT,-1)<0)
       goto error;
      
      if (options->verbose)
       printf("object <%s> reference created to <%s>\n",travi[i].name,refname);
     }
     close_obj(obj_type,refobj_id);
    }/*  j */

  
   
    /*-------------------------------------------------------------------------
     * create/write dataset/close
     *-------------------------------------------------------------------------
     */
    if ((dset_out=H5Dcreate(fidout,travi[i].name,ftype_id,space_id,dcpl_id))<0) 
     goto error;
    if (H5Dwrite(dset_out,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,refbuf)<0)
     goto error;
   
    free(buf);
    free(refbuf);
  
   }/*H5T_STD_REF_OBJ*/


   /*-------------------------------------------------------------------------
    * open previously created object in 1st traversal
    *-------------------------------------------------------------------------
    */
   
   else
   {
    if ((dset_out=H5Dopen(fidout,travi[i].name))<0) 
     goto error;
   }
   
   /*-------------------------------------------------------------------------
    * copy referenced objects in attributes
    *-------------------------------------------------------------------------
    */
   
   if (do_copy_refobjs_inattr(dset_in,dset_out,options,nobjects,travi,fidout)<0) 
    goto error;
   
   
   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
   
   if (H5Tclose(ftype_id)<0) 
    goto error;
   if (H5Tclose(mtype_id)<0) 
    goto error;
   if (H5Pclose(dcpl_id)<0) 
    goto error;
   if (H5Sclose(space_id)<0) 
    goto error;
   if (H5Dclose(dset_in)<0) 
    goto error;
   if (H5Dclose(dset_out)<0) 
    goto error;
  
   
   break;
   
  /*-------------------------------------------------------------------------
   * H5G_TYPE
   *-------------------------------------------------------------------------
   */
  case H5G_TYPE:
   
   if ((type_in = H5Topen (fidin,travi[i].name))<0) 
    goto error;
  
   if (H5Tclose(type_in)<0) 
    goto error;
 
   
   break;
   
   
  /*-------------------------------------------------------------------------
   * H5G_LINK
   *-------------------------------------------------------------------------
   */
   
  case H5G_LINK:
   
   /*nothing to do */
   break;
   
  default:
  
   break;
  }
 }
 
 
 
/*-------------------------------------------------------------------------
 * the root is a special case, we get an ID for the root group 
 * and copy its attributes using that ID
 * it must be done last, because the attributes might contain references to
 * objects in the object list
 *-------------------------------------------------------------------------
 */
 
 if ((grp_out = H5Gopen(fidout,"/"))<0) 
  goto error;
 
 if ((grp_in  = H5Gopen(fidin,"/"))<0) 
  goto error;
 
 if (do_copy_refobjs_inattr(grp_in,grp_out,options,nobjects,travi,fidout)<0) 
  goto error;
 
 if (H5Gclose(grp_out)<0) 
  goto error;
 if (H5Gclose(grp_in)<0) 
  goto error;
 
 return 0;
 
error:
 H5E_BEGIN_TRY {
  H5Gclose(grp_in);
  H5Gclose(grp_out);
  H5Pclose(dcpl_id);
  H5Sclose(space_id);
  H5Dclose(dset_in);
  H5Dclose(dset_out);
  H5Tclose(ftype_id);
  H5Tclose(mtype_id);
  H5Tclose(type_in);
 } H5E_END_TRY;
 return -1;
 
}


/*-------------------------------------------------------------------------
 * Function: do_copy_refobjs_inattr
 *
 * Purpose: copy attributes located in LOC_IN, which is obtained either from 
 * loc_id = H5Gopen( fid, name);
 * loc_id = H5Dopen( fid, name);
 * loc_id = H5Topen( fid, name);
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 28, 2003
 *
 *-------------------------------------------------------------------------
 */

int do_copy_refobjs_inattr(hid_t loc_in, 
              hid_t loc_out, 
              pack_opt_t *options,
              int nobjects,        /* number of objects */
              trav_info_t *travi,  /* array of object names */
              hid_t fidout         /* for saving references */
              )
{
 hid_t      attr_id;      /* attr ID */ 
 hid_t      attr_out;     /* attr ID */ 
 hid_t      space_id;     /* space ID */ 
 hid_t      ftype_id;     /* file data type ID */ 
 hid_t      mtype_id;     /* memory data type ID */
 size_t     msize;        /* memory size of type */
 hsize_t    nelmts;       /* number of elements in dataset */
 int        rank;         /* rank of dataset */
 hsize_t    dims[H5S_MAX_RANK];/* dimensions of dataset */
 char       name[255];
 int        n, j;
 unsigned   u;

 if ((n = H5Aget_num_attrs(loc_in))<0) 
  goto error;
 
 for ( u = 0; u < (unsigned)n; u++)
 {

/*-------------------------------------------------------------------------
 * open
 *-------------------------------------------------------------------------
 */
  /* open attribute */
  if ((attr_id = H5Aopen_idx(loc_in, u))<0) 
   goto error;
  
  /* get name */
  if (H5Aget_name( attr_id, 255, name )<0) 
   goto error;

  /* get the file datatype  */
  if ((ftype_id = H5Aget_type( attr_id )) < 0 )
   goto error;
  
  /* get the dataspace handle  */
  if ((space_id = H5Aget_space( attr_id )) < 0 )
   goto error;
  
  /* get dimensions  */
  if ( (rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0 )
   goto error;

  
  /*-------------------------------------------------------------------------
   * elements
   *-------------------------------------------------------------------------
   */
  nelmts=1;
  for (j=0; j<rank; j++) 
   nelmts*=dims[j];
  if ((mtype_id=H5Tget_native_type(ftype_id,H5T_DIR_DEFAULT))<0)
   goto error;
  if ((msize=H5Tget_size(mtype_id))==0)
   goto error;



/*-------------------------------------------------------------------------
 * object references are a special case
 * we cannot just copy the buffers, but instead we recreate the reference 
 *-------------------------------------------------------------------------
 */
   if (H5Tequal(mtype_id, H5T_STD_REF_OBJ)) 
   {
    H5G_obj_t   obj_type;
    hid_t       refobj_id;
    hobj_ref_t  *refbuf;
    int         i;
    const char* refname;
    hobj_ref_t  *buf;
 
    
    buf=(void *) HDmalloc((unsigned)(nelmts*msize));
    if ( buf==NULL){
     printf( "cannot read into memory\n" );
     goto error;
    }
    if (H5Aread(attr_id,mtype_id,buf)<0)
     goto error;
      
    if ((obj_type = H5Rget_obj_type(attr_id,H5R_OBJECT,buf))<0)
     goto error;
    refbuf=HDmalloc((unsigned)nelmts*msize);
    if ( refbuf==NULL){
     printf( "cannot allocate memory\n" );
     goto error;
    }
    for ( i=0; i<nelmts; i++)
    {
     if ((refobj_id = H5Rdereference(attr_id,H5R_OBJECT,&buf[i]))<0)
      goto error;
     
      /* get the name. a valid name could only occur in the 
     second traversal of the file */
     if ((refname=MapIdToName(refobj_id,nobjects,travi,options))!=NULL)
     {
      /* create the reference */
      if (H5Rcreate(&refbuf[i],fidout,refname,H5R_OBJECT,-1)<0)
       goto error;
      
      if (options->verbose)
       printf("object <%s> reference created to <%s>\n",name,refname);
     }
     close_obj(obj_type,refobj_id);
    }/*  i */

    
    /*-------------------------------------------------------------------------
     * copy 
     *-------------------------------------------------------------------------
    */
    
    if ((attr_out=H5Acreate(loc_out,name,ftype_id,space_id,H5P_DEFAULT))<0)
     goto error;
    if(H5Awrite(attr_out,mtype_id,refbuf)<0)
     goto error;
    
    if (H5Aclose(attr_out)<0) 
     goto error;

    free(refbuf);
    free(buf);
  
   }/*H5T_STD_REF_OBJ*/
   
   
/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

  if (H5Tclose(ftype_id)<0) goto error;
  if (H5Tclose(mtype_id)<0) goto error;
  if (H5Sclose(space_id)<0) goto error;
  if (H5Aclose(attr_id)<0) goto error;
 } /* u */
 
  return 0;

error:
 H5E_BEGIN_TRY {
  H5Tclose(ftype_id);
  H5Tclose(mtype_id);
  H5Sclose(space_id);
  H5Aclose(attr_id);
  H5Aclose(attr_out);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function:	close_obj
 *
 * Purpose:	Auxiliary function to close an object
 *
 *-------------------------------------------------------------------------
 */

static void close_obj(H5G_obj_t obj_type, hid_t obj_id)
{
 
 switch (obj_type) {
 case H5G_GROUP:
  H5Gclose(obj_id);
  break;
 case H5G_DATASET:
  H5Dclose(obj_id);
  break;
 case H5G_TYPE:
  H5Tclose(obj_id);
  break;
 default:
  assert(0);
  break;
 }
}

/*-------------------------------------------------------------------------
 * Function:	MapIdToName
 *
 * Purpose:	map an object ID to a name
 *
 *-------------------------------------------------------------------------
 */

const char* MapIdToName(hid_t refobj_id, 
                        int nobjects,         /* number of objects */
                        trav_info_t *travi,   /* array of object names */
                        pack_opt_t  *options) /* repack options */
{
 hid_t id;
 hid_t fid;
 int   i;

 if ((fid = H5Iget_file_id(refobj_id))<0)
 {
  assert(0);
  return NULL;
 }
   

 for ( i=0; i<nobjects; i++)
 {
  switch ( travi[i].type ) 
  {
   
 /*-------------------------------------------------------------------------
  * H5G_GROUP
  *-------------------------------------------------------------------------
  */
   
  case H5G_GROUP:
  
   
   break;
   
  /*-------------------------------------------------------------------------
   * H5G_DATASET
   *-------------------------------------------------------------------------
   */
   
  case H5G_DATASET:
   
   if ((id = H5Dopen(fid,travi[i].name))<0)
   {
    assert(0);
    return NULL;
   }

   if (H5Dclose(id)<0)
   {
    assert(0);
    return NULL;
   }

   if (id==refobj_id)
   {
    H5Fclose(fid);
    return travi[i].name;
   }
 
   
   break;
   
  /*-------------------------------------------------------------------------
   * H5G_TYPE
   *-------------------------------------------------------------------------
   */
   
  case H5G_TYPE:
   
  
   
   break;
   
   
  /*-------------------------------------------------------------------------
   * H5G_LINK
   *-------------------------------------------------------------------------
   */
   
  case H5G_LINK:
   break;
   
  default:
   assert(0);
   break;
   
  } 
  
 
 } /* i */

 if (H5Fclose(fid)<0)
  return NULL;

 return NULL;
}
