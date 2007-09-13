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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "H5DSprivate.h"
#include "H5LTprivate.h"
#include "H5IMprivate.h"
#include "H5TBprivate.h"



/*-------------------------------------------------------------------------
 * Function: H5DSset_scale
 *
 * Purpose: The dataset DSID is converted to a Dimension Scale dataset.
	*   Creates the CLASS attribute, set to the value "DIMENSION_SCALE"
	*   and an empty REFERENCE_LIST attribute.
 *   If DIMNAME is specified, then an attribute called NAME is created,
	*   with the value DIMNAME.
 *
 * Return: Success: SUCCEED, Failure: FAIL
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 04, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5DSset_scale(hid_t dsid,
                     const char *dimname)
{
 int has_dimlist;
 H5I_type_t it;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(dsid))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

/*-------------------------------------------------------------------------
 * check if the dataset is a dataset wich has references to dimension scales
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "DIMENSION_LIST"  */
 if ((has_dimlist = H5LT_find_attribute(dsid,DIMENSION_LIST))<0)
  return FAIL;

 if (has_dimlist == 1)
  return FAIL;

/*-------------------------------------------------------------------------
 * write the standard attributes for a Dimension Scale dataset
 *-------------------------------------------------------------------------
 */

 if (H5LT_set_attribute_string(dsid,"CLASS",DIMENSION_SCALE_CLASS)<0)
  return FAIL;

 if (dimname!=NULL)
 {
  if (H5LT_set_attribute_string(dsid,"NAME",dimname)<0)
   return FAIL;
 }

 return SUCCEED;
}



/*-------------------------------------------------------------------------
 * Function: H5DSattach_scale
 *
 * Purpose: Define Dimension Scale DSID to be associated with dimension IDX
	*  of Dataset DID. Entries are created in the DIMENSION_LIST and
	*  REFERENCE_LIST attributes.
 *
 * Return:
 *   Success: SUCCEED
 *   Failure: FAIL
	*
	* Fails if: Bad arguments
 *           If DSID is not a Dimension Scale
 *           If DID is a Dimension Scale (A Dimension Scale cannot have scales)
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: December 20, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5DSattach_scale(hid_t did,
                        hid_t dsid,
                        unsigned int idx)
{
 int        has_dimlist;
 int        has_reflist;
 int        is_ds;
 hssize_t   nelmts;
 hid_t      sid;          /* space ID */
 hid_t      tid = -1;     /* attribute type ID */
 hid_t      aid = -1;     /* attribute ID */
 int        rank;         /* rank of dataset */
 hsize_t    *dims;        /* dimension of the "REFERENCE_LIST" array */
 ds_list_t  dsl;          /* attribute data in the DS pointing to the dataset */
 ds_list_t  *dsbuf;       /* array of attribute data in the DS pointing to the dataset */
 hobj_ref_t ref_to_ds;    /* reference to the DS */
 hobj_ref_t ref_j;        /* iterator reference */
 hvl_t      *buf;         /* VL buffer to store in the attribute */
 hid_t      dsid_j;       /* DS dataset ID in DIMENSION_LIST */
 H5O_info_t oi1, oi2;
 H5I_type_t it1, it2;
 int        i, len;
 int        found_ds=0;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* the dataset cannot be a DS dataset */
 if ((H5DSis_scale(did))==1)
  return FAIL;

 /* get info for the dataset in the parameter list */
 if(H5Oget_info(did, ".", &oi1, H5P_DEFAULT) < 0)
  return FAIL;

 /* get info for the scale in the parameter list */
 if(H5Oget_info(dsid, ".", &oi2, H5P_DEFAULT) < 0)
  return FAIL;

 /* same object, not valid */
 if(oi1.fileno == oi2.fileno && oi1.addr == oi2.addr)
  return FAIL;

 /* get ID type */
 if ((it1 = H5Iget_type(did))<0)
  return FAIL;
 if ((it2 = H5Iget_type(dsid))<0)
  return FAIL;

 if (H5I_DATASET!=it1 || H5I_DATASET!=it2)
  return FAIL;

 /* the DS dataset cannot have dimension scales */
 if (H5LT_find_attribute(dsid,DIMENSION_LIST)==1)
  return FAIL;

 /* check if the dataset is a "reserved" dataset (image, table) */
 if (H5DS_is_reserved(did)==1)
  return FAIL;


/*-------------------------------------------------------------------------
 * The dataset may or may not have the associated DS attribute
 * First we try to open to see if it is already there; if not, it is created.
 * If it exists, the array of references is extended to hold the reference
 * to the new DS
 *-------------------------------------------------------------------------
 */

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* scalar rank */
 if (rank==0)
  rank=1;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  return FAIL;

 /* parameter range checking */
 if (idx>(unsigned)rank-1)
  return FAIL;

/*-------------------------------------------------------------------------
 * two references are created: one to the DS, saved in "DIMENSION_LIST"
 *  and one to the dataset, saved in "REFERENCE_LIST"
 *-------------------------------------------------------------------------
 */
 /* create a reference for the >>DS<< dataset */
 if (H5Rcreate(&ref_to_ds,dsid,".",H5R_OBJECT,-1)<0)
  return FAIL;

 /* create a reference for the >>data<< dataset */
 if (H5Rcreate(&dsl.ref,did,".",H5R_OBJECT,-1)<0)
  return FAIL;

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

/*-------------------------------------------------------------------------
 * it does not exist. we create the attribute and its reference data
 *-------------------------------------------------------------------------
 */
 if (has_dimlist == 0)
 {
  /* create one entry array */
  dims = (hsize_t*) malloc (1 * sizeof (hsize_t));

  if (dims == NULL)
   return FAIL;

  dims[0] = rank;

  /* space for the attribute */
  if ((sid = H5Screate_simple(1,dims,NULL))<0)
   return FAIL;

  /* create the type for the attribute "DIMENSION_LIST" */
  if ((tid = H5Tvlen_create(H5T_STD_REF_OBJ))<0)
   goto out;

  /* create the attribute */
  if ((aid = H5Acreate(did,DIMENSION_LIST,tid,sid,H5P_DEFAULT))<0)
   goto out;

  /* allocate and initialize the VL */
  buf = (hvl_t*)malloc((size_t)rank * sizeof(hvl_t));

  if (buf == NULL)
   goto out;

  for(i=0; i<rank; i++)
  {
   buf[i].len = 0;
   buf[i].p = NULL;
  }

  /* store the REF information in the index of the dataset that has the DS */
  buf[idx].len = 1;
  buf[idx].p = malloc( 1 * sizeof(hobj_ref_t));
  ((hobj_ref_t *)buf[idx].p)[0] = ref_to_ds;

  /* write the attribute with the reference */
  if (H5Awrite(aid,tid,buf)<0)
   goto out;

  /* close */
  if (H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf)<0)
   goto out;
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;

  if (dims)
   free(dims);
  if (buf)
   free(buf);

 }

/*-------------------------------------------------------------------------
 * the attribute already exists, open it, extend the buffer,
 *  and insert the new reference
 *-------------------------------------------------------------------------
 */

 else if ( has_dimlist == 1 )
 {
  if ((aid = H5Aopen_name(did,DIMENSION_LIST))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  if ((sid = H5Aget_space(aid))<0)
   goto out;

  /* allocate and initialize the VL */
  buf = (hvl_t*)malloc((size_t)rank * sizeof(hvl_t));

  if (buf == NULL)
   goto out;

  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  /* check to avoid inserting duplicates. it is not FAIL, just do nothing */
  /* iterate all the REFs in this dimension IDX */
  for (i=0; i<(int)buf[idx].len; i++)
  {
   /* get the reference */
   ref_j = ((hobj_ref_t *)buf[idx].p)[i];

   /* get the scale id for this REF */
   if ((dsid_j = H5Rdereference(did,H5R_OBJECT,&ref_j))<0)
    goto out;

   /* get info for DS in the parameter list */
   if(H5Oget_info(dsid, ".", &oi1, H5P_DEFAULT) < 0)
    goto out;

   /* get info for this DS */
   if(H5Oget_info(dsid_j, ".", &oi2, H5P_DEFAULT) < 0)
    goto out;

   /* same object, so this DS scale is already in this DIM IDX */
   if(oi1.fileno == oi2.fileno && oi1.addr == oi2.addr)
    found_ds = 1;

   /* close the dereferenced dataset */
   if (H5Dclose(dsid_j)<0)
    goto out;
  }

  if (found_ds==0)
  {
  /* we are adding one more DS to this dimension */
   if ( buf[idx].len > 0 )
   {
    buf[idx].len++;
    len = buf[idx].len;
    buf[idx].p = realloc( buf[idx].p, len * sizeof(hobj_ref_t));
    ((hobj_ref_t *)buf[idx].p)[ len-1 ] = ref_to_ds;
   }
   else
   {
    /* store the REF information in the index of the dataset that has the DS */
    buf[idx].len = 1;
    buf[idx].p = malloc( 1 * sizeof(hobj_ref_t));
    ((hobj_ref_t *)buf[idx].p)[0] = ref_to_ds;
   }
  }

  /* write the attribute with the new references */
  if (H5Awrite(aid,tid,buf)<0)
   goto out;

  /* close */
  if (H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf)<0)
   goto out;
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);

 } /* has_dimlist */

/*-------------------------------------------------------------------------
 * save DS info on the >>DS<< dataset
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "REFERENCE_LIST" on the >>DS<< dataset */
 if ((has_reflist = H5LT_find_attribute(dsid,REFERENCE_LIST))<0)
  goto out;

/*-------------------------------------------------------------------------
 * it does not exist. we create the attribute and its reference data
 *-------------------------------------------------------------------------
 */
 if (has_reflist == 0)
 {
  /* create one entry array */
  dims = (hsize_t*) malloc (1 * sizeof (hsize_t));

  if (dims == NULL)
   goto out;

  dims[0] = 1;

  /* space for the attribute */
  if ((sid = H5Screate_simple(1,dims,NULL))<0)
   goto out;

  /* create the compound datatype for the attribute "REFERENCE_LIST" */
  if ((tid = H5Tcreate(H5T_COMPOUND,sizeof(ds_list_t)))<0)
   goto out;

  /* insert reference field */
  if (H5Tinsert(tid,"dataset",HOFFSET(ds_list_t,ref),H5T_STD_REF_OBJ)<0)
   goto out;

  /* insert dimension idx of the dataset field */
  if (H5Tinsert(tid,"dimension",HOFFSET(ds_list_t,dim_idx),H5T_NATIVE_INT)<0)
   goto out;

  /* create the attribute */
  if ((aid = H5Acreate(dsid,REFERENCE_LIST,tid,sid,H5P_DEFAULT))<0)
   goto out;

  /* store the IDX information */
  dsl.dim_idx = idx;

  /* write the attribute with the reference */
  if (H5Awrite(aid,tid,&dsl)<0)
   goto out;

  /* close */
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;

  if (dims)
   free(dims);
 }

/*-------------------------------------------------------------------------
 * the "REFERENCE_LIST" array already exists, open it and extend it
 *-------------------------------------------------------------------------
 */

 else if ( has_reflist ==  1 )
 {
  if ((aid = H5Aopen_name(dsid,REFERENCE_LIST))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  /* get and save the old reference(s) */
  if ((sid = H5Aget_space(aid))<0)
   goto out;

  if ((nelmts = H5Sget_simple_extent_npoints(sid))<0)
   goto out;

  nelmts++;

  dsbuf = malloc((size_t)nelmts * sizeof(ds_list_t));

  if (dsbuf == NULL)
   goto out;

  if (H5Aread(aid,tid,dsbuf)<0)
   goto out;

  /* close */
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;

/*-------------------------------------------------------------------------
 * create a new attribute
 *-------------------------------------------------------------------------
 */

  /* the attribute must be deleted, in order to the new one can reflect the changes*/
  if (H5Adelete(dsid,REFERENCE_LIST)<0)
   goto out;

  /* store the IDX information (index of the dataset that has the DS) */
  dsl.dim_idx = idx;
  dsbuf[nelmts-1] = dsl;

  /* create a new data space for the new references array */
  dims = (hsize_t*) malloc ( (size_t)nelmts * sizeof (hsize_t));
  if (dims == NULL)
   goto out;
  dims[0] = nelmts;

  if ((sid = H5Screate_simple(1,dims,NULL))<0)
   goto out;

  if (dims)
   free(dims);

  /* create the attribute again with the changes of space */
  if ((aid = H5Acreate(dsid,REFERENCE_LIST,tid,sid,H5P_DEFAULT))<0)
   goto out;

  /* write the attribute with the new references */
  if (H5Awrite(aid,tid,dsbuf)<0)
   goto out;

  /* close */
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;

  if (dsbuf)
   free(dsbuf);

 } /* has_reflist */

/*-------------------------------------------------------------------------
 * write the standard attributes for a Dimension Scale dataset
 *-------------------------------------------------------------------------
 */

 if ((is_ds=H5DSis_scale(dsid))<0)
  return FAIL;

 if (is_ds == 0 )
 {
  if (H5LT_set_attribute_string(dsid,"CLASS",DIMENSION_SCALE_CLASS)<0)
   return FAIL;
 }

 return SUCCEED;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DSdetach_scale
 *
 * Purpose: If possible, deletes association of Dimension Scale DSID with
	*     dimension IDX of Dataset DID. This deletes the entries in the
	*     DIMENSION_LIST and REFERENCE_LIST attributes.
 *
 * Return:
 *   Success: SUCCEED
 *   Failure: FAIL
	*
	* Fails if: Bad arguments
 *           The dataset DID or DSID do not exist.
 *           The DSID is not a Dimension Scale
 *           DSID is not attached to DID.
 * Note that a scale may be associated with more than dimension of the same dataset.
	* If so, the detach operation only deletes one of the associations, for DID.
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: December 20, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5DSdetach_scale(hid_t did,
                        hid_t dsid,
                        unsigned int idx)
{
 int        has_dimlist;
 int        has_reflist;
 hssize_t   nelmts;
 hid_t      dsid_j;       /* DS dataset ID in DIMENSION_LIST */
	hid_t      did_i;        /* dataset ID in REFERENCE_LIST */
 hid_t      sid;          /* space ID */
 hid_t      tid = -1;     /* attribute type ID */
 hid_t      aid = -1;     /* attribute ID */
 int        rank;         /* rank of dataset */
 ds_list_t  *dsbuf = NULL;  /* array of attribute data in the DS pointing to the dataset */
 ds_list_t  *dsbufn = NULL; /* array of attribute data in the DS pointing to the dataset */
 hsize_t    *dims = NULL;   /* dimension of the "REFERENCE_LIST" array */
 hobj_ref_t ref;          /* reference to the DS */
 hvl_t      *buf = NULL;  /* VL buffer to store in the attribute */
 unsigned   i, j, jj;
 H5O_info_t oi1, oi2, oi3, oi4;
 int        found_dset = 0, found_ds = 0;
 H5I_type_t it1, it2;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* the dataset cannot be a DS dataset */
 if ((H5DSis_scale(did))==1)
  return FAIL;

 /* get info for the dataset in the parameter list */
 if(H5Oget_info(did, ".", &oi1, H5P_DEFAULT) < 0)
  return FAIL;

 /* get info for the scale in the parameter list */
 if(H5Oget_info(dsid, ".", &oi2, H5P_DEFAULT) < 0)
  return FAIL;

 /* same object, not valid */
 if(oi1.fileno == oi2.fileno && oi1.addr == oi2.addr)
  return FAIL;

 /* get ID type */
 if ((it1 = H5Iget_type(did))<0)
  return FAIL;
 if ((it2 = H5Iget_type(dsid))<0)
  return FAIL;

 if (H5I_DATASET!=it1 || H5I_DATASET!=it2)
  return FAIL;


/*-------------------------------------------------------------------------
 * Find "DIMENSION_LIST"
 *-------------------------------------------------------------------------
 */
 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

 if (has_dimlist == 0)
  return FAIL;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  return FAIL;


/*-------------------------------------------------------------------------
 * find "REFERENCE_LIST"
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "REFERENCE_LIST" on the >>DS<< dataset */
 if ((has_reflist = H5LT_find_attribute(dsid,REFERENCE_LIST))<0)
  return FAIL;

 if (has_reflist == 0)
  return FAIL;

/*-------------------------------------------------------------------------
 * open "DIMENSION_LIST", and delete the reference
 *-------------------------------------------------------------------------
 */

 if ((aid = H5Aopen_name(did,DIMENSION_LIST))<0)
  return FAIL;

 if ((tid = H5Aget_type(aid))<0)
  goto out;

 if ((sid = H5Aget_space(aid))<0)
  goto out;

 /* allocate and initialize the VL */
 buf = (hvl_t*)malloc((size_t)rank * sizeof(hvl_t));

 if (buf == NULL)
  goto out;

 /* read */
 if (H5Aread(aid,tid,buf)<0)
  goto out;

 /* reset */
 if ( buf[idx].len > 0 )
 {
  for (j=0; j<buf[idx].len; j++)
  {
   /* get the reference */
   ref = ((hobj_ref_t *)buf[idx].p)[j];

   /* get the DS id */
   if ((dsid_j = H5Rdereference(did,H5R_OBJECT,&ref))<0)
    goto out;

   /* get info for DS in the parameter list */
   if(H5Oget_info(dsid, ".", &oi1, H5P_DEFAULT) < 0)
    goto out;

   /* get info for this DS */
   if(H5Oget_info(dsid_j, ".", &oi2, H5P_DEFAULT) < 0)
    goto out;

   /* same object, reset */
   if(oi1.fileno == oi2.fileno && oi1.addr == oi2.addr) {
    for(jj=j; jj<buf[idx].len-1; jj++)
     ((hobj_ref_t *)buf[idx].p)[jj] = ((hobj_ref_t *)buf[idx].p)[jj+1];
    buf[idx].len--;

    found_ds = 1;

    /* close the dereferenced dataset and break */
    if (H5Dclose(dsid_j)<0)
     goto out;
    break;
   }

   /* close the dereferenced dataset */
   if (H5Dclose(dsid_j)<0)
    goto out;
  } /* j */
 } /* if */

 /* write the attribute */
 if (H5Awrite(aid,tid,buf)<0)
  goto out;

 /* close */
 if (H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf)<0)
  goto out;
 if (H5Sclose(sid)<0)
  goto out;
 if (H5Tclose(tid)<0)
  goto out;
 if (H5Aclose(aid)<0)
  goto out;
 if (buf)
  free(buf);

 /* the scale must be present */
 if (found_ds == 0)
  goto out;

/*-------------------------------------------------------------------------
 * the "REFERENCE_LIST" array exists, update
 *-------------------------------------------------------------------------
 */

 if ((aid = H5Aopen_name(dsid,REFERENCE_LIST))<0)
  goto out;

 if ((tid = H5Aget_type(aid))<0)
  goto out;

 /* get and save the old reference(s) */
 if ((sid = H5Aget_space(aid))<0)
  goto out;

 if ((nelmts = H5Sget_simple_extent_npoints(sid))<0)
  goto out;

 dsbuf = malloc((size_t)nelmts * sizeof(ds_list_t));
 if (dsbuf == NULL)
  goto out;

 if (H5Aread(aid,tid,dsbuf)<0)
  goto out;

 for(i=0; i<nelmts; i++)
 {
  /* get the reference to the dataset */
  ref = dsbuf[i].ref;

  /* get the dataset id */
  if ((did_i = H5Rdereference(did,H5R_OBJECT,&ref))<0)
   goto out;

  /* get info for dataset in the parameter list */
  if(H5Oget_info(did, ".", &oi3, H5P_DEFAULT) < 0)
   goto out;

  /* get info for this dataset */
  if(H5Oget_info(did_i, ".", &oi4, H5P_DEFAULT) < 0)
   goto out;

  /* same object, reset. we want to detach only for this DIM */
  if(oi3.fileno == oi4.fileno && oi3.addr == oi4.addr && (int)idx == dsbuf[i].dim_idx) {
   for(jj=i; jj<nelmts-1; jj++)
    dsbuf[jj] = dsbuf[jj+1];
   nelmts--;
   found_dset=1;

   /* close the dereferenced dataset and break */
   if (H5Dclose(did_i)<0)
    goto out;
   break;
  } /* if */

  /* close the dereferenced dataset */
  if (H5Dclose(did_i)<0)
   goto out;

 } /* i */

 /* close space and attribute */
 if (H5Sclose(sid)<0)
  goto out;
 if (H5Aclose(aid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * check if we found the pointed dataset
 *-------------------------------------------------------------------------
 */

 /* the pointed dataset must exist */
 if (found_dset == 0)
  goto out;

/*-------------------------------------------------------------------------
 * create a new attribute
 *-------------------------------------------------------------------------
 */

 /* the attribute must be deleted, in order to the new one can reflect the changes*/
 if (H5Adelete(dsid,REFERENCE_LIST)<0)
  goto out;

 /* don't do anything for an empty array */
 if (nelmts)
 {
  /* create a new data space for the new references array */
  dims = (hsize_t*) malloc ( (size_t)nelmts * sizeof (hsize_t));
  if (dims == NULL)
   goto out;
  dims[0] = nelmts;

  dsbufn = malloc((size_t)nelmts * sizeof(ds_list_t));
  if (dsbufn == NULL)
   goto out;

  /* store the new information */
  for(i=0; i<nelmts; i++)
  {
   dsbufn[i] = dsbuf[i];
  }

  if ((sid = H5Screate_simple(1,dims,NULL))<0)
   goto out;

  /* create the attribute again with the changes of space */
  if ((aid = H5Acreate(dsid,REFERENCE_LIST,tid,sid,H5P_DEFAULT))<0)
   goto out;

  /* write the new attribute with the new references */
  if (H5Awrite(aid,tid,dsbufn)<0)
   goto out;

  /* close space and attribute */
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;

 } /* nelmts */

 /* close type */
 if (H5Tclose(tid)<0)
  goto out;

 if (dsbuf) {
  free(dsbuf);
  dsbuf=NULL;
 }
 if (dsbufn) {
  free(dsbufn);
  dsbufn=NULL;
 }
 if (dims) {
  free(dims);
  dims=NULL;
 }


 return SUCCEED;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
  if (dsbuf)
   free(dsbuf);
  if (dsbufn)
   free(dsbufn);
  if (dims)
   free(dims);
 } H5E_END_TRY;
 return FAIL;

}

/*-------------------------------------------------------------------------
 * Function: H5DSis_attached
 *
 * Purpose: Report if dimension scale DSID is currently attached to
 *  dimension IDX of dataset DID by checking if DID has a pointer in the REFERENCE_LIST
 *  attribute and DSID (scale ) has a pointer in the DIMENSION_LIST attribute
 *
 * Return:
 *   1: both the DS and the dataset pointers match
 *   0: one of them or both do not match
 *   FAIL (-1): error
	*
	* Fails if: Bad arguments
 *           If DSID is not a Dimension Scale
 *           If DID is a Dimension Scale (A Dimension Scale cannot have scales)
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: February 18, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

htri_t H5DSis_attached(hid_t did,
                       hid_t dsid,
                       unsigned int idx)
{
 int        has_dimlist;
 int        has_reflist;
 hssize_t   nelmts;
 hid_t      sid;          /* space ID */
 hid_t      tid = -1;     /* attribute type ID */
 hid_t      aid = -1;     /* attribute ID */
 int        rank;         /* rank of dataset */
 ds_list_t  *dsbuf;       /* array of attribute data in the DS pointing to the dataset */
 hobj_ref_t ref;          /* reference to the DS */
 hvl_t      *buf;         /* VL buffer to store in the attribute */
 hid_t      dsid_j;       /* DS dataset ID in DIMENSION_LIST */
	hid_t      did_i;        /* dataset ID in REFERENCE_LIST */
 H5O_info_t oi1, oi2, oi3, oi4;
 H5I_type_t it1, it2;
 int        i;
 int        found_dset=0, found_ds=0;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* the dataset cannot be a DS dataset */
 if ((H5DSis_scale(did))==1)
  return FAIL;

 /* get info for the dataset in the parameter list */
 if(H5Oget_info(did, ".", &oi1, H5P_DEFAULT) < 0)
  return FAIL;

 /* get info for the scale in the parameter list */
 if(H5Oget_info(dsid, ".", &oi2, H5P_DEFAULT) < 0)
  return FAIL;

 /* same object, not valid */
 if(oi1.fileno == oi2.fileno && oi1.addr == oi2.addr)
  return FAIL;

 /* get ID type */
 if ((it1 = H5Iget_type(did))<0)
  return FAIL;
 if ((it2 = H5Iget_type(dsid))<0)
  return FAIL;

 if (H5I_DATASET!=it1 || H5I_DATASET!=it2)
  return FAIL;

/*-------------------------------------------------------------------------
 * get space
 *-------------------------------------------------------------------------
 */

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

 /* parameter range checking */
 if (idx>(unsigned)rank-1)
  goto out;

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

/*-------------------------------------------------------------------------
 * open "DIMENSION_LIST"
 *-------------------------------------------------------------------------
 */

 if ( has_dimlist == 1 )
 {
  if ((aid = H5Aopen_name(did,DIMENSION_LIST))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  if ((sid = H5Aget_space(aid))<0)
   goto out;

  /* allocate and initialize the VL */
  buf = (hvl_t*)malloc((size_t)rank * sizeof(hvl_t));

  if (buf == NULL)
   goto out;

  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  /* iterate all the REFs in this dimension IDX */
  for (i=0; i<(int)buf[idx].len; i++)
  {
   /* get the reference */
   ref = ((hobj_ref_t *)buf[idx].p)[i];

   /* get the scale id for this REF */
   if ((dsid_j = H5Rdereference(did,H5R_OBJECT,&ref))<0)
    goto out;

   /* get info for DS in the parameter list */
   if(H5Oget_info(dsid, ".", &oi1, H5P_DEFAULT) < 0)
    goto out;

   /* get info for this DS */
   if(H5Oget_info(dsid_j, ".", &oi2, H5P_DEFAULT) < 0)
    goto out;

   /* same object */
   if(oi1.fileno == oi2.fileno && oi1.addr == oi2.addr)
    found_ds = 1;

   /* close the dereferenced dataset */
   if (H5Dclose(dsid_j)<0)
    goto out;

  }


  /* close */
  if (H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf)<0)
   goto out;
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);
 } /* has_dimlist */

/*-------------------------------------------------------------------------
 * info on the >>DS<< dataset
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "REFERENCE_LIST" on the >>DS<< dataset */
 if ((has_reflist = H5LT_find_attribute(dsid,REFERENCE_LIST))<0)
  goto out;

/*-------------------------------------------------------------------------
 * open "REFERENCE_LIST"
 *-------------------------------------------------------------------------
 */

 if ( has_reflist ==  1 )
 {
  if ((aid = H5Aopen_name(dsid,REFERENCE_LIST))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  /* get and save the old reference(s) */
  if ((sid = H5Aget_space(aid))<0)
   goto out;

  if ((nelmts = H5Sget_simple_extent_npoints(sid))<0)
   goto out;

  dsbuf = malloc((size_t)nelmts * sizeof(ds_list_t));

  if (dsbuf == NULL)
   goto out;

  if (H5Aread(aid,tid,dsbuf)<0)
   goto out;

/*-------------------------------------------------------------------------
 * iterate
 *-------------------------------------------------------------------------
 */

  for(i=0; i<nelmts; i++)
  {
   /* get the reference */
   ref = dsbuf[i].ref;

   /* the reference was not deleted  */
   if (ref)
   {
    /* get the dataset id */
    if ((did_i = H5Rdereference(did,H5R_OBJECT,&ref))<0)
     goto out;

    /* get info for dataset in the parameter list */
    if(H5Oget_info(did, ".", &oi3, H5P_DEFAULT) < 0)
     goto out;

    /* get info for this dataset */
    if(H5Oget_info(did_i, ".", &oi4, H5P_DEFAULT) < 0)
     goto out;

    /* same object */
    if(oi3.fileno == oi4.fileno && oi3.addr == oi4.addr && (int)idx==dsbuf[i].dim_idx)
     found_dset=1;

    /* close the dereferenced dataset */
    if (H5Dclose(did_i)<0)
     goto out;
   } /* if */
  } /* i */


  /* close */
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (dsbuf)
   free(dsbuf);
 } /* has_reflist */

 if (found_ds && found_dset)
  return 1;
 else
  return 0;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DSiterate_scales
 *
 * Purpose: H5DSiterate_scales iterates over the scales attached to dimension DIM
 *  of dataset DID. For each scale in the list, the visitor_data and some
 *  additional information, specified below, are passed to the visitor function.
 *  The iteration begins with the IDX object in the group and the next element
 *  to be processed by the operator is returned in IDX. If IDX is NULL, then the
 *  iterator starts at zero.
 *
 * Parameters:
 *
 *  hid_t DID;               IN: the dataset
 *  unsigned int dim;        IN: the dimension of the dataset
 *  int *idx;                IN/OUT: input the index to start iterating, output the
	*                             next index to visit. If NULL, start at the first position.
 *  H5DS_iterate_t visitor;  IN: the visitor function
 *  void *visitor_data;      IN: arbitrary data to pass to the visitor function.
 *
 *  Iterate over all scales of DIM, calling an application callback
 *   with the item, key and any operator data.
 *
 *   The operator callback receives a pointer to the item ,
 *   and the pointer to the operator data passed
 *   in to H5SL_iterate ('op_data').  The return values from an operator are:
 *       A. Zero causes the iterator to continue, returning zero when all
 *           nodes of that type have been processed.
 *       B. Positive causes the iterator to immediately return that positive
 *           value, indicating short-circuit success.
 *       C. Negative causes the iterator to immediately return that value,
 *           indicating failure.
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 31, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5DSiterate_scales(hid_t did,
                          unsigned int dim,
                          int *idx,
                          H5DS_iterate_t visitor,
                          void *visitor_data )
{
 hid_t        scale_id;
 int          rank;
 hobj_ref_t   ref;          /* reference to the DS */
 hid_t        sid;          /* space ID */
 hid_t        tid = -1;     /* attribute type ID */
 hid_t        aid = -1;     /* attribute ID */
 hvl_t        *buf=NULL;    /* VL buffer to store in the attribute */
 H5I_type_t   it;           /* ID type */
 herr_t       ret_value=0;
 int          j_idx;
 int          nscales;
 int          has_dimlist;
 int          i;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(did))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

 /* get the number of scales assotiated with this DIM */
 if ((nscales = H5DSget_num_scales(did,dim))<0)
  return FAIL;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

 if (has_dimlist == 0)
  return SUCCEED;

 else if (has_dimlist == 1 )
 {
  if ((aid = H5Aopen_name(did,DIMENSION_LIST))<0)
   goto out;
  if ((tid = H5Aget_type(aid))<0)
   goto out;
  if ((sid = H5Aget_space(aid))<0)
   goto out;

  /* allocate and initialize the VL */
  buf = (hvl_t*)malloc((size_t)rank * sizeof(hvl_t));

  if (buf == NULL)
   goto out;

  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  if ( buf[dim].len > 0 )
  {
   if (idx!=NULL)
    j_idx = *idx;
   else
    j_idx=0;

   /* iterate */
   for(i=j_idx; i<nscales; i++)
   {
    /* get the reference */
    ref = ((hobj_ref_t *)buf[dim].p)[ i ];

    /* disable error reporting, the ID might refer to a deleted dataset */
    H5E_BEGIN_TRY {
     /* get the DS id */
     if ((scale_id = H5Rdereference(did,H5R_OBJECT,&ref))<0)
      goto out;
    } H5E_END_TRY;

    if((ret_value=(visitor)(did,dim,scale_id,visitor_data))!=0)
    {
     /* set the return IDX OUT value at current scale index and break */
     if (idx!=NULL)
     {
      *idx = i;
     }

     /* close the DS id */
     if (H5Dclose(scale_id)<0)
      goto out;

     break;
    }

    /* close the DS id */
    if (H5Dclose(scale_id)<0)
     goto out;

   } /* i */
  } /* if */

  /* close */
  if (H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf)<0)
   goto out;
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);

 } /* if has_dimlist */

 return ret_value;

out:
 H5E_BEGIN_TRY {
  if (buf)
  {
   H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf);
   free(buf);
  }
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DSset_label
 *
 * Purpose: Set label for the dimension IDX of dataset DID to the value LABEL
 *
 * Return: Success: SUCCEED, Failure: FAIL
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 11, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5DSset_label(hid_t did,
                     unsigned int idx,
                     const char *label)
{
 int           has_labels;
 hid_t         sid;                /* space ID */
 hid_t         tid = -1;           /* attribute type ID */
 hid_t         aid = -1;           /* attribute ID */
 int           rank;               /* rank of dataset */
 hsize_t       dims[1];            /* dimensions of dataset */
 const char    **buf=NULL;         /* buffer to store in the attribute */
 H5I_type_t    it;                 /* ID type */
 unsigned int  i;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(did))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

/*-------------------------------------------------------------------------
 * attribute "DIMENSION_LABELS"
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "DIMENSION_LABELS" on the >>data<< dataset */
 if ((has_labels = H5LT_find_attribute(did,DIMENSION_LABELS))<0)
  return FAIL;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * make the attribute and insert label
 *-------------------------------------------------------------------------
 */

 if (has_labels == 0)
 {
  dims[0] = rank;

  /* space for the attribute */
  if ((sid = H5Screate_simple(1,dims,NULL))<0)
   goto out;

  /* create the datatype  */
  if ((tid = H5Tcopy(H5T_C_S1))<0)
   goto out;
  if (H5Tset_size(tid,H5T_VARIABLE)<0)
   goto out;

  /* create the attribute */
  if ((aid = H5Acreate(did,DIMENSION_LABELS,tid,sid,H5P_DEFAULT))<0)
   goto out;

  /* allocate and initialize */
  buf = (const char **)malloc((size_t)rank * sizeof(char *));

  if (buf == NULL)
   goto out;

  for(i=0; i<(unsigned int)rank; i++)
   buf[i] = NULL;

  /* store the label information in the required index */
  buf[idx] = label;

  /* write the attribute with the label */
  if (H5Awrite(aid,tid,buf)<0)
   goto out;

  /* close */
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);
 }

/*-------------------------------------------------------------------------
 * just insert label
 *-------------------------------------------------------------------------
 */

 else
 {
  if ((aid = H5Aopen_name(did,DIMENSION_LABELS))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  /* allocate and initialize */
  buf = (const char **)malloc((size_t)rank * sizeof(char *));

  if (buf == NULL)
   goto out;

  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  /* store the label information in the required index */
  buf[idx] = label;

  /* write the attribute with the new references */
  if (H5Awrite(aid,tid,buf)<0)
   goto out;

  /* close */
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);
 }

 return SUCCEED;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DSget_label
 *
 * Purpose: Read the label LABEL for dimension IDX of dataset DID
 *   Up to 'size' characters are stored in 'label' followed by a '\0' string
 *   terminator.  If the label is longer than 'size'-1,
 *   the string terminator is stored in the last position of the buffer to
 *   properly terminate the string.
 *
 * Return: 0 if no label found, size of label if found, Failure: FAIL
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 11, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t H5DSget_label(hid_t did,
                      unsigned int idx,
                      char *label,
                      size_t size)
{
 int             has_labels;
 hid_t           sid;                /* space ID */
 hid_t           tid = -1;           /* attribute type ID */
 hid_t           aid = -1;           /* attribute ID */
 int             rank;               /* rank of dataset */
 char            **buf=NULL;         /* buffer to store in the attribute */
 H5I_type_t      it;                 /* ID type */
 size_t          nbytes;
 size_t          copy_len;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(did))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

/*-------------------------------------------------------------------------
 * attribute "DIMENSION_LABELS"
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "DIMENSION_LABELS" on the >>data<< dataset */
 if ((has_labels = H5LT_find_attribute(did,DIMENSION_LABELS))<0)
  return FAIL;

 /* return 0 and NULL for label if no label found */
 if (has_labels == 0)
 {
  if (label)
   label=NULL;
  return 0;
 }

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * open the attribute and read label
 *-------------------------------------------------------------------------
 */

 assert (has_labels == 1);
  if ((aid = H5Aopen_name(did,DIMENSION_LABELS))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  /* allocate and initialize */
  buf = (char **)malloc((size_t)rank * sizeof(char *));

  if (buf == NULL)
   goto out;

  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  /* get the real string length */
  nbytes = strlen(buf[idx]);

  /* compute the string length which will fit into the user's buffer */
  copy_len = MIN(size-1, nbytes);

  /* copy all/some of the name */
  if( label ) {
   memcpy(label, buf[idx], copy_len);

   /* terminate the string */
   label[copy_len]='\0';

  /* close */
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);
 }

 return (ssize_t) nbytes;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DSget_scale_name
 *
 * Purpose: Read the name of dataset scale DID into buffer NAME
 *   Up to 'size' characters are stored in 'name' followed by a '\0' string
 *   terminator.  If the name is longer than 'size'-1,
 *   the string terminator is stored in the last position of the buffer to
 *   properly terminate the string.
 *
 * Return: size of name if found, zero if not found,  Failure: FAIL
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 04, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

ssize_t H5DSget_scale_name(hid_t did,
                           char *name,
                           size_t size)
{
 hid_t      aid;      /* attribute ID  */
 hid_t      tid = -1; /* attribute type ID */
 hid_t      sid;      /* space ID  */
 H5I_type_t it;       /* ID type */
 size_t     nbytes;
 size_t     copy_len;
 int        has_name;
 char       *buf=NULL;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(did))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

 if ((H5DSis_scale(did))<=0)
  return FAIL;

/*-------------------------------------------------------------------------
 * check if the DS has a name
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "NAME" on the >>DS<< dataset */
 if ((has_name = H5LT_find_attribute(did,"NAME"))<0)
  return FAIL;

 if (has_name == 0)
  return 0;

/*-------------------------------------------------------------------------
 * open the attribute
 *-------------------------------------------------------------------------
 */

 if ((aid = H5Aopen_name(did,"NAME"))<0)
  return FAIL;

 /* get space */
 if ((sid = H5Aget_space(aid))<0)
  goto out;

 /* get type */
 if ((tid = H5Aget_type(aid))<0)
  goto out;

 /* get the size */
 if ((nbytes = H5Tget_size(tid))==0)
  goto out;

 /* allocate a temporary buffer */
 buf = (char*)malloc(nbytes * sizeof(char));
 if (buf == NULL)
  goto out;

 /* read */
 if (H5Aread(aid,tid,buf)<0)
  goto out;

 /* compute the string length which will fit into the user's buffer */
 copy_len = MIN(size-1, nbytes);

 /* copy all/some of the name */
 if (name) {
  memcpy(name, buf, copy_len);

  /* terminate the string */
  name[copy_len]='\0';
 }

 /* close */
 if (H5Tclose(tid)<0)
  goto out;
 if (H5Aclose(aid)<0)
  goto out;
 if (H5Sclose(sid)<0)
  goto out;
 if (buf)
 {
  free(buf);
  buf=NULL;
 }

 return (ssize_t) nbytes;

 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Aclose(aid);
  H5Tclose(tid);
  H5Sclose(sid);
 } H5E_END_TRY;
 if (buf)
  free(buf);
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DSis_scale
 *
 * Purpose: check if the dataset DID is a dimension scale
 *
 * Return: 1, is, 0, not, FAIL, error
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 04, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

htri_t H5DSis_scale(hid_t did)
{
 hid_t      tid = -1;   /* attribute type ID */
 hid_t      aid;        /* attribute ID */
 herr_t     has_class;  /* has the "CLASS" attribute */
 htri_t     is_ds;      /* boolean return value */
 H5I_type_t it;           /* ID type */
 char       buf[20];

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(did))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

 /* try to find the attribute "CLASS" on the dataset */
 if ((has_class = H5LT_find_attribute(did,"CLASS"))<0)
  return FAIL;

 if (has_class == 0)
  is_ds = 0;

 else
 {
  if ((aid = H5Aopen_name(did,"CLASS"))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  if (H5Aread(aid,tid,buf)<0)
   goto out;

  if( strcmp(buf,DIMENSION_SCALE_CLASS)==0)
   is_ds = 1;
  else
   is_ds = 0;

  if (H5Tclose(tid)<0)
   goto out;

  if (H5Aclose(aid)<0)
   goto out;
 }

 return is_ds;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;

}

/*-------------------------------------------------------------------------
 * Function: H5DSget_num_scales
 *
 * Purpose: get the number of scales linked to the IDX dimension of dataset DID
 *
 * Return:
 *   Success: number of scales
 *   Failure: FAIL
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: January 13, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int H5DSget_num_scales(hid_t did,
                       unsigned int idx)
{
 int        has_dimlist;
 hid_t      sid;          /* space ID */
 hid_t      tid = -1;     /* attribute type ID */
 hid_t      aid = -1;     /* attribute ID */
 int        rank;         /* rank of dataset */
 hvl_t      *buf;         /* VL buffer to store in the attribute */
 H5I_type_t it;           /* ID type */
 int        nscales;

/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* get ID type */
 if ((it = H5Iget_type(did))<0)
  return FAIL;

 if (H5I_DATASET!=it)
  return FAIL;

/*-------------------------------------------------------------------------
 * the attribute "DIMENSION_LIST" on the >>data<< dataset must exist
 *-------------------------------------------------------------------------
 */
 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  return FAIL;

 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

 /* dimemsion index IDX range checking */
 if (idx>=(unsigned int )rank)
  return FAIL;

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

 /* it does not exist */
 if (has_dimlist == 0)
  return 0;

/*-------------------------------------------------------------------------
 * the attribute exists, open it
 *-------------------------------------------------------------------------
 */

 else
 {
  if ((aid = H5Aopen_name(did,DIMENSION_LIST))<0)
   goto out;
  if ((tid = H5Aget_type(aid))<0)
   goto out;
  if ((sid = H5Aget_space(aid))<0)
   goto out;

  /* allocate and initialize the VL */
  buf = (hvl_t*)malloc((size_t)rank * sizeof(hvl_t));

  if (buf == NULL)
   goto out;

  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  nscales=(int)buf[idx].len;

  /* close */
  if (H5Dvlen_reclaim(tid,sid,H5P_DEFAULT,buf)<0)
   goto out;
  if (H5Sclose(sid)<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Aclose(aid)<0)
   goto out;
  if (buf)
   free(buf);

 } /* has_dimlist */

 return nscales;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}

/*-------------------------------------------------------------------------
 * Function: H5DS_is_reserved
 *
 * Purpose: Verify that a dataset's CLASS is either an image, palette or table
 *
 * Return: true, false, fail
 *
 * Programmer: pvn@ncsa.uiuc.edu
 *
 * Date: March 19, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t H5DS_is_reserved(hid_t did)
{
 int    has_class;
 hid_t  tid = -1;
 hid_t  aid = -1;
 char   buf[40];
 herr_t ret;

 /* try to find the attribute "CLASS" on the dataset */
 if ((has_class = H5LT_find_attribute(did,"CLASS"))<0)
  return -1;

 if ( has_class ==  0 )
  return 0;

 assert( has_class ==  1 );
  if ((aid = H5Aopen_name(did,"CLASS"))<0)
   goto out;

  if ((tid = H5Aget_type(aid))<0)
   goto out;

  if (H5Aread(aid,tid,buf)<0)
   goto out;

  if ( strcmp(buf,IMAGE_CLASS)==0 ||
       strcmp(buf,PALETTE_CLASS)==0 ||
       strcmp(buf,TABLE_CLASS)==0 )
   ret = 1;
  else
   ret = 0;

  if (H5Tclose(tid)<0)
   goto out;

  if (H5Aclose(aid)<0)
   goto out;


 return ret;

/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Tclose(tid);
  H5Aclose(aid);
 } H5E_END_TRY;
 return FAIL;
}




