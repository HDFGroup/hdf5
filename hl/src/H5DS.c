/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/


#include "H5DS.h"
#include "H5LT.h"
#include <stdlib.h>

/*-------------------------------------------------------------------------
 * Function: H5DSset_scale
 *
 * Purpose: write the standard attributes for a Dimension Scale dataset;
 *  optionally set its name
 *
 * Return: Success: SUCCESS, Failure: FAIL
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

herr_t H5DSset_scale(hid_t did, 
                     char *dimname) 
{  
 int has_dimlist;
 H5I_type_t it;

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
 * check if the dataset is a dataset wich has references to dimension scales
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

 if (has_dimlist == 1)
  return FAIL;
 
/*-------------------------------------------------------------------------
 * write the standard attributes for a Dimension Scale dataset
 *-------------------------------------------------------------------------
 */
 
 if (H5LT_set_attribute_string(did,"CLASS",DIMENSION_SCALE_CLASS)<0)
  return FAIL;

 if (dimname!=NULL)
 {
  if (H5LT_set_attribute_string(did,"NAME",dimname)<0)
   return FAIL;
 }

 return SUCCESS;
}



/*-------------------------------------------------------------------------
 * Function: H5DSattach_scale
 *
 * Purpose: attach a DS with DSID to the IDX dimension of the existing 
 *   dataset DID
 *
 * Return:  
 *   Success: SUCCESS: both the DS and the dataset exist
 *   Failure: FAIL: if either one of them does not exist
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
 hid_t      tid;          /* attribute type ID */
 hid_t      aid;          /* attribute ID */
 int        rank;         /* rank of dataset */
 hsize_t    *dims;        /* dimension of the "REFERENCE_LIST" array */
 ds_list_t  dsl;          /* attribute data in the DS pointing to the dataset */
 ds_list_t  *dsbuf;       /* array of attribute data in the DS pointing to the dataset */
 hobj_ref_t ref;          /* reference to the DS */
 hvl_t      *buf;         /* VL buffer to store in the attribute */
 H5G_stat_t sb1, sb2;
 H5I_type_t it1, it2;
 int        i, len;
 
/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* the dataset cannot be a DS dataset */
 if ((H5DSis_scale(did))==1) 
  return FAIL;
 
 /* get info for the dataset in the parameter list */
 if (H5Gget_objinfo(did,".",TRUE,&sb1)<0)
  return FAIL;

 /* get info for the scale in the parameter list */
 if (H5Gget_objinfo(dsid,".",TRUE,&sb2)<0)
  return FAIL;

 /* same object, not valid */
 if (sb1.fileno==sb2.fileno && sb1.objno==sb2.objno) 
  return FAIL;

 /* get ID type */
 if ((it1 = H5Iget_type(did))<0)
  return FAIL;
 if ((it2 = H5Iget_type(dsid))<0)
  return FAIL;

 if (H5I_DATASET!=it1 || H5I_DATASET!=it2)
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
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

 /* parameter range checking */
 if (idx>(unsigned)rank-1)
  goto out;

/*-------------------------------------------------------------------------
 * two references are created: one to the DS, saved in "DIMENSION_LIST"
 *  and one to the dataset, saved in "REFERENCE_LIST"
 *-------------------------------------------------------------------------
 */
 /* create a reference for the >>DS<< dataset */
 if (H5Rcreate(&ref,dsid,".",H5R_OBJECT,-1)<0)
  goto out;
 
 /* create a reference for the >>data<< dataset */
 if (H5Rcreate(&dsl.ref,did,".",H5R_OBJECT,-1)<0)
  goto out;
 
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
   goto out;

  dims[0] = rank;

  /* space for the attribute */
  if ((sid = H5Screate_simple(1,dims,NULL))<0)
   goto out;
    
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
  ((hobj_ref_t *)buf[idx].p)[0] = ref;

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

  /* we are adding one more DS to this dimension */
  if ( buf[idx].len > 0 )
  {
   buf[idx].len++;
   len = buf[idx].len;
   buf[idx].p = realloc( buf[idx].p, len * sizeof(hobj_ref_t));
   ((hobj_ref_t *)buf[idx].p)[ len-1 ] = ref;
  }
  else
  {
   /* store the REF information in the index of the dataset that has the DS */
   buf[idx].len = 1;
   buf[idx].p = malloc( 1 * sizeof(hobj_ref_t));
   ((hobj_ref_t *)buf[idx].p)[0] = ref;
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
  if ((sid = H5Screate_simple(1,&nelmts,NULL))<0)
   goto out;
  
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
    
 } /* has_dimlist */ 
 
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

 return SUCCESS;
 
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
 * Purpose: detach a DS with DSID to the IDX dimension of the existing 
 *   dataset DID
 *
 * Return:  
 *   Success: SUCCESS: both the DS and the dataset exist
 *   Failure: FAIL: if either one of them does not exist
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
 hid_t      sid;          /* space ID */
 hid_t      tid;          /* attribute type ID */
 hid_t      aid;          /* attribute ID */
 int        rank;         /* rank of dataset */
 ds_list_t  *dsbuf;       /* array of attribute data in the DS pointing to the dataset */
 hobj_ref_t ref;          /* reference to the DS */
 hvl_t      *buf;         /* VL buffer to store in the attribute */
 unsigned   i, j, jj;
 H5G_stat_t sb1, sb2, sb3, sb4;
 int        found_dset=0, found_ds=0;
 H5I_type_t it1, it2;
 
/*-------------------------------------------------------------------------
 * parameter checking
 *-------------------------------------------------------------------------
 */
 /* the dataset cannot be a DS dataset */
 if ((H5DSis_scale(did))==1) 
  return FAIL;
 
 /* get info for the dataset in the parameter list */
 if (H5Gget_objinfo(did,".",TRUE,&sb1)<0)
  return FAIL;

 /* get info for the scale in the parameter list */
 if (H5Gget_objinfo(dsid,".",TRUE,&sb2)<0)
  return FAIL;

 /* same object, not valid */
 if (sb1.fileno==sb2.fileno && sb1.objno==sb2.objno) 
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
  goto out;

 if (has_dimlist == 0)
  goto out;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;


/*-------------------------------------------------------------------------
 * find "REFERENCE_LIST"
 *-------------------------------------------------------------------------
 */

 /* try to find the attribute "REFERENCE_LIST" on the >>DS<< dataset */
 if ((has_reflist = H5LT_find_attribute(dsid,REFERENCE_LIST))<0)
  goto out;

 if (has_reflist == 0)
  goto out;
 
/*-------------------------------------------------------------------------
 * open "DIMENSION_LIST", and delete the reference
 *-------------------------------------------------------------------------
 */
 
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
 
 /* reset */
 if ( buf[idx].len > 0 )
 {
  for(j=0; j<buf[idx].len; j++)
  {
   /* get the reference */
   ref = ((hobj_ref_t *)buf[idx].p)[j];
   
   /* get the DS id */
   if ((dsid_j = H5Rdereference(did,H5R_OBJECT,&ref))<0)
    goto out;
   
   /* get info for DS in the parameter list */
   if (H5Gget_objinfo(dsid,".",TRUE,&sb1)<0)
    goto out;
   
   /* get info for this DS */
   if (H5Gget_objinfo(dsid_j,".",TRUE,&sb2)<0)
    goto out;
   
   /* same object, reset */
   if (sb1.fileno==sb2.fileno && sb1.objno==sb2.objno) 
   {
    for(jj=j; jj<buf[idx].len-1; jj++)
    {
     ((hobj_ref_t *)buf[idx].p)[jj] = ((hobj_ref_t *)buf[idx].p)[jj+1];
    }
    buf[idx].len--;
    
    found_ds = 1;
   }
   
   /* close the dereferenced dataset */
   if (H5Dclose(dsid_j)<0)
    goto out;
  } /* j */
 } /* if */
 
 if (found_ds == 0)
  goto out;
 
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
  /* get the reference */
  ref = dsbuf[i].ref;
  
  /* get the DS id */
  if ((dsid_j = H5Rdereference(did,H5R_OBJECT,&ref))<0)
   goto out;
  
  /* get info for dataset in the parameter list */
  if (H5Gget_objinfo(did,".",TRUE,&sb3)<0)
   goto out;
  
  /* get info for this DS */
  if (H5Gget_objinfo(dsid_j,".",TRUE,&sb4)<0)
   goto out;
  
  /* same object, reset */
  if (sb3.fileno==sb4.fileno && sb3.objno==sb4.objno) {
   dsbuf[i].ref=0;
   dsbuf[i].dim_idx=-1;
   found_dset=1;
  } /* if */
  
  /* close the dereferenced dataset */
  if (H5Dclose(dsid_j)<0)
   goto out;
 } /* i */
 
 if (found_dset == 0)
  goto out;
 
 /* update on disk */
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
 
 return SUCCESS;
 
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
 * Function: H5DSget_nscales
 *
 * Purpose: get the number of scales linked to the IDX dimension of DNAME 
 *
 * Return:  
 *   Success: SUCCESS: both the DS and the dataset exist
 *   Failure: FAIL: if either one of them does not exist
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

herr_t H5DSget_nscales(hid_t did,
                       unsigned int dim,
                       int *nscales) 
{ 
 int        has_dimlist;
 hid_t      sid;          /* space ID */
 hid_t      tid;          /* attribute type ID */
 hid_t      aid;          /* attribute ID */
 int        rank;         /* rank of dataset */
 hvl_t      *buf;         /* VL buffer to store in the attribute */
 H5I_type_t it;           /* ID type */

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
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

 /* DIM range checking */
 if (dim>=(unsigned int )rank)
  goto out;

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  return FAIL;

 /* it does not exist */
 if (has_dimlist == 0)
  goto out;

/*-------------------------------------------------------------------------
 * the attribute exists, open it 
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

  *nscales=(int)buf[dim].len;

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

 return SUCCESS;
 
/* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Sclose(sid);
  H5Aclose(aid);
  H5Tclose(tid);
 } H5E_END_TRY;
 return FAIL;
}


/*-------------------------------------------------------------------------
 * Function: H5DSset_label
 *
 * Purpose: set a label for dimension IDX 
 *
 * Return: Success: SUCCESS, Failure: FAIL
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
                     char *label,
                     unsigned int idx)
{  
 int           has_labels;
 hid_t         sid;                /* space ID */
 hid_t         tid;                /* attribute type ID */
 hid_t         aid;                /* attribute ID */
 int           rank;               /* rank of dataset */
 hsize_t       dims[1];            /* dimensions of dataset */
 char          **buf=NULL;         /* buffer to store in the attribute */
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
  goto out;
 
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
  buf = (char **)malloc((size_t)rank * sizeof(char *));

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

  if ((sid = H5Aget_space(aid))<0)
   goto out;
 
  /* allocate and initialize */
  buf = (char **)malloc((size_t)rank * sizeof(char *));

  if (buf == NULL)
   goto out;
  
  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  for(i=0; i<(unsigned int)rank; i++)
  {
   if (idx == i )
   {
    /* store the label information in the required index */
    buf[idx] = label;
   }
  }
  
  /* write the attribute with the new references */
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

 return SUCCESS;

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
 * Purpose: get a label for dimension IDX 
 *
 * Return: Success: SUCCESS, Failure: FAIL
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
herr_t H5DSget_label(hid_t did, 
                     char *label,
                     unsigned int idx)
{  
 int             has_labels;
 hid_t           sid;                /* space ID */
 hid_t           tid;                /* attribute type ID */
 hid_t           aid;                /* attribute ID */
 int             rank;               /* rank of dataset */
 char            **buf=NULL;         /* buffer to store in the attribute */
 H5I_type_t      it;                 /* ID type */
 unsigned int    i;

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

 if (has_labels == 0)
  return FAIL;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  goto out;
 
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

 if (has_labels == 1)
 {
  if ((aid = H5Aopen_name(did,DIMENSION_LABELS))<0)
   goto out;
  
  if ((tid = H5Aget_type(aid))<0)
   goto out;

  if ((sid = H5Aget_space(aid))<0)
   goto out;
 
  /* allocate and initialize */
  buf = (char **)malloc((size_t)rank * sizeof(char *));

  if (buf == NULL)
   goto out;
  
  /* read */
  if (H5Aread(aid,tid,buf)<0)
   goto out;

  for(i=0; i<(unsigned int)rank; i++)
  {
   if (idx == i )
   {
    /* store the label information in the required index */
    strcpy(label,buf[idx]);
   }
  }
 
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


 return SUCCESS;

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
 * Purpose: get the name of a DS
 *
 * Return: Success: SUCCESS, Failure: FAIL
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

herr_t H5DSget_scale_name(hid_t did, 
                          char *buf) 
{  
 H5I_type_t it;           /* ID type */
 int has_name;

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

 if (has_name == 1)
 {
  /* get the attribute */
  if (H5LT_get_attribute_disk(did,"NAME",buf)<0)
   return FAIL;
 }

 return SUCCESS;
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

herr_t H5DSis_scale(hid_t did) 
{  
 hid_t      tid;        /* attribute type ID */
 hid_t      aid;        /* attribute ID */
 herr_t     has_class;  /* has the "CLASS" attribute */
 herr_t     is_ds;      /* boolean return value */
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

 else if (has_class == 1 )
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
 * Function: H5DSiterate_scales
 *
 * Purpose: H5DSiterate_scales iterates over the scales attached to dimension dim 
 *  of dataset dset. For each scale in the list, the visitor_data and some 
 *  additional information, specified below, are passed to the visitor function. 
 *  The iteration begins with the idx object in the group and the next element 
 *  to be processed by the operator is returned in idx. If idx is NULL, then the 
 *  iterator starts at zero.
 *
 * Parameters:
 *
 *  hid_t DID;               IN: the dataset 
 *  unsigned int dim;        IN: the dimension of the dataset
 *  int *idx;                IN/OUT: input the index to start iterating, output the next index
 *                             to visit. If NULL, start at the first position.
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
 hid_t        scale_id=0;
 int          rank;
 hobj_ref_t   ref;          /* reference to the DS */
 hid_t        sid;          /* space ID */
 hid_t        tid;          /* attribute type ID */
 hid_t        aid;          /* attribute ID */
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
 if (H5DSget_nscales(did,dim,&nscales)<0)
  goto out;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* close dataset space */
 if (H5Sclose(sid)<0)
  goto out;

 /* try to find the attribute "DIMENSION_LIST" on the >>data<< dataset */
 if ((has_dimlist = H5LT_find_attribute(did,DIMENSION_LIST))<0)
  goto out;

 if (has_dimlist == 0)
  return SUCCESS;

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

    /* get the DS id */
    if ((scale_id = H5Rdereference(did,H5R_OBJECT,&ref))<0)
     goto out;
    
    if((ret_value=(visitor)(did,dim,scale_id,visitor_data))!=0)
    {
     /* set the return IDX OUT value at current scale index and break */
     if (idx!=NULL) *idx = i;
     break;
    }
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
  if (scale_id && H5Dclose(scale_id)<0)
   goto out;
  if (buf)
   free(buf);
    
 } /* if has_dimlist */

 return ret_value;
      
out:
 return FAIL;
} 

