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
#include "h5repack_list.h"



/*-------------------------------------------------------------------------
 * Function: get_objlist
 *
 * Purpose: locate all HDF5 objects in the file 
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September, 23, 2003
 *
 *-------------------------------------------------------------------------
 */


int get_objlist(char* fname, packoptions_t *options)
{
 hid_t    fid; 
 int      nobjects, i;
 info_t   *info=NULL;

/*-------------------------------------------------------------------------
 * open the file 
 *-------------------------------------------------------------------------
 */

 /* disable error reporting */
 H5E_BEGIN_TRY {
 
 /* Open the files */
 if ((fid=H5Fopen(fname,H5F_ACC_RDONLY,H5P_DEFAULT))<0 ){
  printf("h5repack: <%s>: No such file or directory\n", fname );
  exit(1);
 }
 /* enable error reporting */
 } H5E_END_TRY;


/*-------------------------------------------------------------------------
 * get the number of objects in the file
 *-------------------------------------------------------------------------
 */

 if ((nobjects = H5get_object_info(fid, NULL ))<0) {
  printf("h5repack: <%s>: Could not obtain object list\n", fname );
  return -1;
 }

/*-------------------------------------------------------------------------
 * get the list of objects in the file
 *-------------------------------------------------------------------------
 */

 if ((info = (info_t*) malloc( nobjects * sizeof(info_t)))==NULL){
  printf("h5repack: <%s>: Could not allocate object list\n", fname );
  return -1;
 }
 if (H5get_object_info(fid, info )<0) {
  printf("h5repack: <%s>: Could not obtain object list\n", fname );
  return -1;
 }

/*-------------------------------------------------------------------------
 * compare with user supplied list
 *-------------------------------------------------------------------------
 */
 
 if (options->verbose)
  printf("Opening file <%s>. Searching for objects to modify...\n",fname);
 
 for ( i = 0; i < options->op_tbl->nelems; i++) 
 {
  char* obj_name=options->op_tbl->objs[i].path;
  if (options->verbose)
   printf(PFORMAT1,"","",obj_name);
  
  /* the input object names are present in the file and are valid */
  if (info_getindex(obj_name,nobjects,info)<0)
  {
   printf("\nError: Could not find <%s> in file <%s>. Exiting...\n",
    obj_name,fname);
   H5Fclose(fid);
   info_free(info,nobjects);
   exit(1);
  }
  if (options->verbose)
   printf("...Found\n");
 }



/*-------------------------------------------------------------------------
 * free
 *-------------------------------------------------------------------------
 */
 H5Fclose(fid);
 info_free(info,nobjects);
 return 0;

}



/*-------------------------------------------------------------------------
 * Function: copy_file
 *
 * Purpose: duplicate all HDF5 objects in the file 
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 23, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_file(char* fnamein, 
              char* fnameout,
              packoptions_t *options)
{
 hid_t    fidin; 
 hid_t    fidout; 
 int      nobjects;
 info_t   *info=NULL;

/*-------------------------------------------------------------------------
 * open the files 
 *-------------------------------------------------------------------------
 */

 /* disable error reporting */
 H5E_BEGIN_TRY {
 
 /* Open the files */
 if ((fidin=H5Fopen(fnamein,H5F_ACC_RDONLY,H5P_DEFAULT))<0 ){
  printf("h5repack: <%s>: No such file or directory\n", fnamein );
  exit(1);
 }
 if ((fidout=H5Fcreate(fnameout,H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0 ){
  printf("h5repack: <%s>: Could not create file\n", fnameout );
  exit(1);
 }
 /* enable error reporting */
 } H5E_END_TRY;


/*-------------------------------------------------------------------------
 * get the number of objects in the file
 *-------------------------------------------------------------------------
 */

 if ((nobjects = H5get_object_info(fidin, NULL ))<0) {
  printf("h5repack: <%s>: Could not obtain object list\n", fnamein );
  return -1;
 }

/*-------------------------------------------------------------------------
 * get the list of objects in the file
 *-------------------------------------------------------------------------
 */

 if ((info = (info_t*) malloc( nobjects * sizeof(info_t)))==NULL){
  printf("h5repack: <%s>: Could not allocate object list\n", fnamein );
  return -1;
 }
 if (H5get_object_info(fidin, info )<0) {
  printf("h5repack: <%s>: Could not obtain object list\n", fnamein );
  return -1;
 }

/*-------------------------------------------------------------------------
 * do the copy
 *-------------------------------------------------------------------------
 */
 
  do_copy_file(fidin,fidout,nobjects,info,options);
  



/*-------------------------------------------------------------------------
 * free
 *-------------------------------------------------------------------------
 */
 H5Fclose(fidin);
 H5Fclose(fidout);
 info_free(info,nobjects);
 return 0;

}



/*-------------------------------------------------------------------------
 * Function: print_objlist
 *
 * Purpose: print list of objects in file
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 23, 2003
 *
 *-------------------------------------------------------------------------
 */
void print_objlist(char *filename, 
                   int nobjects, 
                   info_t *info )
{
 int i;

 printf("File <%s>: # of entries = %d\n", filename, nobjects );
 for ( i = 0; i < nobjects; i++)
 {
  switch ( info[i].type )
  {
  case H5G_GROUP:
   printf("%s %20s\n", info[i].name, "group" );
   break;
  case H5G_DATASET:
   printf("%s %20s\n", info[i].name, "dataset" );
   break;
  case H5G_TYPE:
   printf("%s %20s\n", info[i].name, "datatype" );
   break;
  case H5G_LINK:
   printf("%s %20s\n", info[i].name, "link" );
   break;
  default:
   printf("%s %20s\n", info[i].name, "User defined object" );
   break;
  }
 }

}


/*-------------------------------------------------------------------------
 * Function: do_copy_file
 *
 * Purpose: duplicate all HDF5 objects in the file 
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 23, 2003
 *
 *-------------------------------------------------------------------------
 */

int do_copy_file(hid_t fidin, 
                 hid_t fidout, 
                 int nobjects, 
                 info_t *info,
                 packoptions_t *options)
{
 hid_t     grp_id;       /* group ID */ 
 hid_t     dset_in;      /* read dataset ID */ 
 hid_t     dset_out;     /* write dataset ID */ 
 hid_t     dcpl_id;      /* dataset creation property list ID */ 
 hid_t     space_id;     /* space ID */ 
 hid_t     ftype_id;     /* file data type ID */ 
 hid_t     mtype_id;     /* memory data type ID */
 size_t    msize;        /* memory size of memory type */
 void      *buf=NULL;    /* data buffer */
 hsize_t   nelmts;       /* number of elements in dataaset */
 int       rank;         /* rank of dataset */
 hsize_t   dims[32];     /* dimansions of dataset */
 int       i, j;

 for ( i = 0; i < nobjects; i++)
 {
  switch ( info[i].type )
  {
/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */
  case H5G_GROUP:
   if (options->verbose)
    printf("%s %20s\n", info[i].name, "group" );

   if ((grp_id=H5Gcreate(fidout, info[i].name, 0))<0) 
    goto error;
   if (H5Gclose(grp_id)<0) 
    goto error;

   break;

/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */
  case H5G_DATASET:
   if (options->verbose)
    printf("%s %20s\n", info[i].name, "dataset" );

   if ((dset_in=H5Dopen(fidin,info[i].name))<0) 
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
 
/*-------------------------------------------------------------------------
 * read to memory
 *-------------------------------------------------------------------------
 */
   nelmts=1;
   for (j=0; j<rank; j++) 
    nelmts*=dims[j];
   if ((mtype_id=H5Tget_native_type(ftype_id,H5T_DIR_DEFAULT))<0)
    goto error;
   if ((msize=H5Tget_size(mtype_id))<0)
    goto error;
   buf=(void *) HDmalloc((unsigned)(nelmts*msize));
   if ( buf==NULL){
    printf( "cannot read into memory\n" );
    goto error;
   }
   if (H5Dread(dset_in,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
    goto error;

/*-------------------------------------------------------------------------
 * create/write dataset/close
 *-------------------------------------------------------------------------
 */
   if ((dset_out=H5Dcreate(fidout,info[i].name,ftype_id,space_id,dcpl_id))<0) 
    goto error;
   if (H5Dwrite(dset_out,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
    goto error;

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
   if (options->verbose)
    printf("%s %20s\n", info[i].name, "datatype" );
   break;

  case H5G_LINK:
   if (options->verbose)
    printf("%s %20s\n", info[i].name, "link" );
   break;

  default:
   if (options->verbose)
    printf("%s %20s\n", info[i].name, "User defined object" );
   break;
  }
 }

 return 0;
 
error:
 H5E_BEGIN_TRY {
  H5Gclose(grp_id);
  H5Pclose(dcpl_id);
  H5Sclose(space_id);
  H5Dclose(dset_in);
  H5Dclose(dset_out);
  H5Tclose(ftype_id);
  H5Tclose(mtype_id);
 } H5E_END_TRY;
 return -1;

}


