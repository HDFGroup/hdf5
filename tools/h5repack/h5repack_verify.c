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

#include "hdf5.h"
#include "h5test.h"
#include "h5repack.h"



/*-------------------------------------------------------------------------
 * Function: has_filter
 *
 * Purpose: verify if a filter is present in the property list DCPL_ID
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 19, 2003
 *
 *-------------------------------------------------------------------------
 */

int has_filter(hid_t dcpl_id,
               H5Z_filter_t filtnin)
{

 int          nfilters;       /* number of filters */
 unsigned     filt_flags;     /* filter flags */
 H5Z_filter_t filtn;          /* filter identification number */
 unsigned     cd_values[20];  /* filter client data values */
 size_t       cd_nelmts;      /* filter client number of values */
 char         f_name[256];    /* filter name */
 int          have=0;         /* flag, filter is present */
 int          i;              /* index */

 /* if no information about the input filter is requested return exit */
 if (filtnin==-1)
  return 1;

 /* get information about filters */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0)
  return -1;

 /* if we do not have filters and the requested filter is NONE, return 1 */
 if (!nfilters && filtnin==H5Z_FILTER_NONE)
  return 1;

 for (i=0; i<nfilters; i++)
 {
  cd_nelmts = NELMTS(cd_values);
  filtn = H5Pget_filter(dcpl_id,
   (unsigned)i,
   &filt_flags,
   &cd_nelmts,
   cd_values,
   sizeof(f_name),
   f_name);

  if (filtnin==filtn)
   have=1;

 }

 return have;
}


/*-------------------------------------------------------------------------
 * Function: has_layout
 *
 * Purpose: verify which layout is present in the property list DCPL_ID
 *
 *  H5D_COMPACT	  	= 0
 *  H5D_CONTIGUOUS	= 1
 *  H5D_CHUNKED		  = 2
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */

int has_layout(hid_t dcpl_id,
               pack_info_t *obj)
{
 hsize_t      chsize[64];     /* chunk size in elements */
 H5D_layout_t layout;         /* layout */
 int          nfilters;       /* number of filters */
 int          rank;           /* rank */
 int          i;              /* index */

 /* if no information about the input layout is requested return exit */
 if (obj==NULL)
  return 1;

 /* check if we have filters in the input object */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0)
  return -1;

 /* a non chunked layout was requested on a filtered object; avoid the test */
 if (nfilters && obj->layout!=H5D_CHUNKED)
  return 1;

 /* get layout */
 if ((layout = H5Pget_layout(dcpl_id))<0)
  return -1;

 if (obj->layout != layout)
  return 0;

 if (layout==H5D_CHUNKED)
 {
  if ((rank = H5Pget_chunk(dcpl_id,NELMTS(chsize),chsize/*out*/))<0)
   return -1;
  if (obj->chunk.rank != rank)
   return 0;
  for ( i=0; i<rank; i++)
   if (chsize[i] != obj->chunk.chunk_lengths[i])
    return 0;
 }

 return 1;
}


/*-------------------------------------------------------------------------
 * Function: h5repack_verify
 *
 * Purpose: verify if the filters specified in the options list are
 *  present on the OUTPUT file
 *
 * Return: 1=filter present, 0=filter not present, -1=error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 19, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5repack_verify(const char *fname,
                    pack_opt_t *options)
{
 hid_t  fid;      /* file ID */
 hid_t  dset_id=-1;  /* dataset ID */
 hid_t  dcpl_id=-1;  /* dataset creation property list ID */
 hid_t  space_id=-1; /* space ID */
 int    ret=1, i, j;
 trav_table_t  *travt=NULL;

 /* open the file */
 if ((fid=H5Fopen(fname,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
  return -1;

 for ( i=0; i<options->op_tbl->nelems; i++)
 {
  char* name=options->op_tbl->objs[i].path;
  pack_info_t *obj = &options->op_tbl->objs[i];

/*-------------------------------------------------------------------------
 * open
 *-------------------------------------------------------------------------
 */
  if ((dset_id=H5Dopen(fid,name))<0)
   goto error;
  if ((space_id=H5Dget_space(dset_id))<0)
   goto error;
  if ((dcpl_id=H5Dget_create_plist(dset_id))<0)
   goto error;

/*-------------------------------------------------------------------------
 * filter check
 *-------------------------------------------------------------------------
 */
  for ( j=0; j<obj->nfilters; j++)
  {
   if (has_filter(dcpl_id,obj->filter[j].filtn)==0)
    ret=0;
  }

/*-------------------------------------------------------------------------
 * layout check
 *-------------------------------------------------------------------------
 */
  if ((obj->layout!=-1) && (has_layout(dcpl_id,obj)==0))
   ret=0;

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
  if (H5Pclose(dcpl_id)<0)
   goto error;
  if (H5Sclose(space_id)<0)
   goto error;
  if (H5Dclose(dset_id)<0)
   goto error;

 }


/*-------------------------------------------------------------------------
 * check for the "all" objects option
 *-------------------------------------------------------------------------
 */

 if (options->all_filter==1 || options->all_layout==1)
 {

  /* init table */
  trav_table_init(&travt);

  /* get the list of objects in the file */
  if (h5trav_gettable(fid,travt)<0)
   goto error;

  for ( i=0; i<travt->nobjs; i++)
  {
   char* name=travt->objs[i].name;

   switch ( travt->objs[i].type )
   {
   case H5G_DATASET:

 /*-------------------------------------------------------------------------
  * open
  *-------------------------------------------------------------------------
  */
    if ((dset_id=H5Dopen(fid,name))<0)
     goto error;
    if ((space_id=H5Dget_space(dset_id))<0)
     goto error;
    if ((dcpl_id=H5Dget_create_plist(dset_id))<0)
     goto error;

 /*-------------------------------------------------------------------------
  * filter check
  *-------------------------------------------------------------------------
  */
    if (options->all_filter==1 ){
     if (has_filter(dcpl_id,options->filter_g.filtn)==0)
      ret=0;
    }

 /*-------------------------------------------------------------------------
  * layout check
  *-------------------------------------------------------------------------
  */
    if (options->all_layout==1){
     pack_info_t pack;
     init_packobject(&pack);
     pack.layout=options->layout_g;
     pack.chunk=options->chunk_g;
     if (has_layout(dcpl_id,&pack)==0)
      ret=0;
    }


  /*-------------------------------------------------------------------------
   * close
   *-------------------------------------------------------------------------
   */
    if (H5Pclose(dcpl_id)<0)
     goto error;
    if (H5Sclose(space_id)<0)
     goto error;
    if (H5Dclose(dset_id)<0)
     goto error;

    break;
   default:
    break;
   } /* switch */

  } /* i */

  /* free table */
  trav_table_free(travt);
 }

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

 if (H5Fclose(fid)<0)
  return -1;

 return ret;

error:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl_id);
  H5Sclose(space_id);
  H5Dclose(dset_id);
  H5Fclose(fid);
  if (travt)
   trav_table_free(travt);
 } H5E_END_TRY;
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: h5repack_cmpdcpl
 *
 * Purpose: compare 2 files for identical property lists of all objects
 *
 * Return: 1=identical, 0=not identical, -1=error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 31, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5repack_cmpdcpl(const char *fname1,
                     const char *fname2)
{
 hid_t         fid1=-1;       /* file ID */
 hid_t         fid2=-1;       /* file ID */
 hid_t         dset1=-1;      /* dataset ID */
 hid_t         dset2=-1;      /* dataset ID */
 hid_t         dcpl1=-1;      /* dataset creation property list ID */
 hid_t         dcpl2=-1;      /* dataset creation property list ID */
 trav_table_t  *travt1=NULL;
 trav_table_t  *travt2=NULL;
 int           ret=1, i;

/*-------------------------------------------------------------------------
 * open the files first; if they are not valid, no point in continuing
 *-------------------------------------------------------------------------
 */

 /* disable error reporting */
 H5E_BEGIN_TRY {

 /* Open the files */
 if ((fid1=H5Fopen(fname1,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5repack: <%s>: %s\n", fname1, H5FOPENERROR );
  return -1;
 }
 if ((fid2=H5Fopen(fname2,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5repack: <%s>: %s\n", fname2, H5FOPENERROR );
  H5Fclose(fid1);
  return -1;
 }
 /* enable error reporting */
 } H5E_END_TRY;

/*-------------------------------------------------------------------------
 * get file table list of objects
 *-------------------------------------------------------------------------
 */
 trav_table_init(&travt1);
 trav_table_init(&travt2);
 if (h5trav_gettable(fid1,travt1)<0)
  goto error;
 if (h5trav_gettable(fid2,travt2)<0)
  goto error;


/*-------------------------------------------------------------------------
 * traverse the suppplied object list
 *-------------------------------------------------------------------------
 */

 for ( i=0; i < travt1->nobjs; i++)
 {
  switch ( travt1->objs[i].type )
  {
/*-------------------------------------------------------------------------
 * nothing to do for groups, links and types
 *-------------------------------------------------------------------------
 */
  default:
   break;

/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */
  case H5G_DATASET:
   if ((dset1=H5Dopen(fid1,travt1->objs[i].name))<0)
    goto error;
   if ((dset2=H5Dopen(fid2,travt1->objs[i].name))<0)
    goto error;
   if ((dcpl1=H5Dget_create_plist(dset1))<0)
    goto error;
   if ((dcpl2=H5Dget_create_plist(dset2))<0)
    goto error;

/*-------------------------------------------------------------------------
 * compare the property lists
 *-------------------------------------------------------------------------
 */
   if ((ret=H5Pequal(dcpl1,dcpl2))<0)
    goto error;

   if (ret==0)
   {
    printf("Property lists for <%s> are different\n",travt1->objs[i].name);
    goto error;
   }

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
   if (H5Pclose(dcpl1)<0)
    goto error;
   if (H5Pclose(dcpl2)<0)
    goto error;
   if (H5Dclose(dset1)<0)
    goto error;
   if (H5Dclose(dset2)<0)
    goto error;

   break;

  } /*switch*/
 } /*i*/

/*-------------------------------------------------------------------------
 * free
 *-------------------------------------------------------------------------
 */

 trav_table_free(travt1);
 trav_table_free(travt2);

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

 H5Fclose(fid1);
 H5Fclose(fid2);
 return ret;

/*-------------------------------------------------------------------------
 * error
 *-------------------------------------------------------------------------
 */

error:
 H5E_BEGIN_TRY {
 H5Pclose(dcpl1);
 H5Pclose(dcpl2);
 H5Dclose(dset1);
 H5Dclose(dset2);
 H5Fclose(fid1);
 H5Fclose(fid2);
 trav_table_free(travt1);
 trav_table_free(travt2);
 } H5E_END_TRY;
 return -1;

}
