/*-------------------------------------------------------------------------
 *
 * Copyright (C) 2000   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 */

/******************************************************************************

  Description: 

1. converter

See HDF4 to HDF5 mapping specification at
(http://hdf.ncsa.uiuc.edu/HDF5/papers/h4toh5) for the default mapping 
from HDF4 object to HDF5 object.
 
The whole converter includes 10 files, h4toh5util.h, h4toh5main.h, h4toh5util.c, h4toh5main.c, h4toh5sds.c, h4toh5image.c,h4toh5vdata.c,h4toh5vgroup.c,h4toh5pal.c and h4toh5anno.c.

2. this file 

Converting an hdf4 sds object into an hdf5 dataset.

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/

#include "h4toh5main.h"

/*-------------------------------------------------------------------------
 * Function:	Sds_h4_to_h5
 *
 * Purpose:     translate SDS object into hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                sds_id: SDS identifier
		h5_group: hdf5 group id
		h5_dimgroup: hdf5 dimension group id
		dim_pathname: dimensional path name

 *-------------------------------------------------------------------------
 */	 
   
int Sds_h4_to_h5(int32 file_id,int32 sds_id,hid_t h5_group,hid_t h5_dimgroup){

  int32   sds_dtype;
  int32   sds_rank;
  int32   sds_dimsizes[MAX_VAR_DIMS];
  int32*  sds_start;
  int32*  sds_edge;
  int32*  sds_stride;
  int32   count_sdsdata;
  int32   sds_ref;
  intn   sds_empty;
  int32   istat;
  int     i;
  int32   num_sdsattrs;
  void*   sds_data;

  int     check_sdsname;
  int     check_gloattr;
  
  char    sdsname[MAX_NC_NAME];
  char    sdslabel[MAX_NC_NAME];
  size_t  h4size;
  size_t  h4memsize;
  HDF_CHUNK_DEF c_def_out;
  hsize_t*  chunk_dims;
  int32   c_flags;

  /* define varibles for hdf5. */

  hid_t   h5dset;
  hid_t   h5d_sid;
  hid_t   h5ty_id;
  hid_t   h5_memtype;
  hid_t   create_plist;
  hsize_t h5dims[MAX_VAR_DIMS];
  hsize_t max_h5dims[MAX_VAR_DIMS];
  
  char*   h5csds_name;

  herr_t  ret;
  /* zeroing out the memory for sdsname and sdslabel.*/

  h4toh5_ZeroMemory(sdsname,MAX_NC_NAME);
  h4toh5_ZeroMemory(sdslabel,MAX_NC_NAME);
  /* check whether the sds is empty. */

  if(SDcheckempty(sds_id,&sds_empty)== FAIL) {
      printf("error in running SDcheckempty routine. \n");
      return FAIL;
    }

  if(sds_empty != 0) return SUCCEED;


  /*check whether the sds is created with unlimited dimension. */
  
  if(SDgetchunkinfo(sds_id,&c_def_out, &c_flags)== FAIL) {
    printf("error in getting chunking information. \n");
    return FAIL;
  }

  /*obtain name,rank,dimsizes,datatype and num of attributes of sds */
    if (SDgetinfo(sds_id,sdsname,&sds_rank,sds_dimsizes,&sds_dtype,
		  &num_sdsattrs)==FAIL) {
      printf("unable to get information of sds h5dset.\n"); 
      return FAIL;
    }

  /* obtain start,edge, stride and number of sds data. */

  sds_start     = malloc(sizeof(int32)*sds_rank);
  if(sds_start == NULL) {
    printf("error in allocating memory for sds start.\n");
    return FAIL;
  }

  sds_edge      = malloc(sizeof(int32)*sds_rank);
  if(sds_edge == NULL) {
    printf("error in allocating memory for sds edge.\n");
    free(sds_start);
    return FAIL;
  }

  sds_stride    = malloc(sizeof(int32)*sds_rank);
  if(sds_stride == NULL) {
    printf("error in allocating memory for sds stride. \n");
    free(sds_start);
    free(sds_edge);
    return FAIL;
  }

  count_sdsdata = 1;
  for (i=0;i<sds_rank;i++){
      sds_stride[i] = 1;
      sds_start[i]  = 0;
      sds_edge[i]   = sds_dimsizes[i];
      count_sdsdata = count_sdsdata*sds_dimsizes[i];

  }

  for (i=0;i<sds_rank;i++) {
      h5dims[i] = sds_edge[i]-sds_start[i];
      max_h5dims[i] = h5dims[i];
  }
  if(SDisrecord(sds_id)) max_h5dims[0] = H5S_UNLIMITED;
  
  /* convert hdf4 data type to hdf5 data type. */
  if  (h4type_to_h5type(sds_dtype,&h5_memtype,&h4memsize,&h4size,
		       &h5ty_id) == FAIL) {
      printf("failed to translate datatype. \n");
      free(sds_start);
      free(sds_edge);
      free(sds_stride);
      return FAIL;
  }

   /* check whether the datatype is string, if we find string format,
    we will change them back into integer format.*/

  if (h5ty_id == H5T_STRING) {
    /* rechange string datatype into numerical datatype.*/
    if(h5string_to_int(sds_dtype,&h5_memtype,h4memsize,
		       &h5ty_id)== FAIL) {
      printf("error in translating H5T_STRING to int.\n");
      free(sds_start);
      free(sds_edge);
      free(sds_stride);
      return FAIL;
    }
  }
 
  sds_data = malloc(h4memsize*count_sdsdata);
  if(sds_data == NULL) {
    printf("error in allocating memory. \n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    return FAIL;
  }

  istat    = SDreaddata(sds_id, sds_start, sds_stride, sds_edge, 
			(VOIDP)sds_data);
  if (istat == FAIL)  { 
     printf("unable to read data from h5dset. \n");
     free(sds_start);
     free(sds_edge);
     free(sds_stride);
     free(sds_data);
     return FAIL;
  }

  /* obtaining reference number and name of h5 dataset 
     corresponding to sds. */

  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("error in obtaining sds reference number. \n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    return FAIL;
  }
  
  h5csds_name = get_name(sds_ref,2*num_sds,sds_hashtab,&check_sdsname);
  if (h5csds_name == NULL && check_sdsname == 0 ) {
     free(sds_start);
     free(sds_edge);
     free(sds_stride);
     free(sds_data);
     printf("error,cannot find sds name \n");
     return FAIL;
  }

  if (h5csds_name == NULL && check_sdsname == -1) {
     free(sds_start);
     free(sds_edge);
     free(sds_stride);
     free(sds_data);
     printf("error,sds name is not defined.\n");
     return FAIL;
  }

  if (h5csds_name == NULL && check_sdsname == -2) {
     free(sds_start);
     free(sds_edge);
     free(sds_stride);
     free(sds_data);
     printf("error,not enough memory for allocating sds name.\n");
     return FAIL;
  }
 
  h5d_sid = H5Screate_simple(sds_rank,h5dims,max_h5dims);	
									      
  if (h5d_sid < 0) {							      
    printf("failed to create hdf5 data space converted from SDS. \n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data); 		      
    return FAIL;							      
  }									      
	
  /* create property list. */

  create_plist = H5Pcreate(H5P_DATASET_CREATE);
  chunk_dims   = malloc(sizeof(hsize_t)*sds_rank);

  /* if the sds is not chunked, but with unlimited dimension, we have to
     provide a chunk size for the corresponding hdf5 dataset. we will choose
     1/2 dimension size right now. */

  if(c_flags == HDF_NONE && SDisrecord(sds_id))
 {
   for(i=0;i<sds_rank;i++){
      chunk_dims[i] = (hsize_t)(h5dims[i]/2);
   }
    if(H5Pset_chunk(create_plist, sds_rank, chunk_dims)<0) {
       printf("failed to set up chunking information for ");
       printf("property list.\n");
       free(sds_start);
       free(sds_edge);
       free(sds_stride);
       free(sds_data);
       free(chunk_dims);
       H5Sclose(h5d_sid);
       H5Pclose(create_plist);
       return FAIL;	
    }

 }
  if(c_flags == HDF_CHUNK || c_flags == (HDF_CHUNK | HDF_COMP)
     || c_flags == (HDF_CHUNK | HDF_NBIT)  ){
     
    for(i=0;i<sds_rank;i++) 
      chunk_dims[i] = (hsize_t)c_def_out.chunk_lengths[i]; 
       
    if(H5Pset_chunk(create_plist, sds_rank, chunk_dims)<0) {
       printf("failed to set up chunking information for ");
       printf("property list.\n");
       free(sds_start);
       free(sds_edge);
       free(sds_stride);
       free(sds_data);
       free(chunk_dims);
       H5Sclose(h5d_sid);
       H5Pclose(create_plist);
       return FAIL;	
     }
  }


  h5dset = H5Dcreate(h5_group,h5csds_name,h5ty_id,h5d_sid,create_plist);    

  if (h5dset < 0) {							      
    printf("failed to create hdf5 dataset converted from SDS. \n");	
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Pclose(create_plist);
    return FAIL;							      
  }									      
									      
  if (H5Dwrite(h5dset,h5_memtype,h5d_sid,h5d_sid,H5P_DEFAULT,	    
	       (void *)sds_data)<0) {				      
    printf("failed to write data into hdf5 dataset");	
    printf(" converted from SDS.\n");
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);	
    free(chunk_dims);
    return FAIL;							      
  }							      
									      

  /* convert sds annotation into attribute of sds dataset.
     Since there is no routines to find the exact tag of sds object,
     we will check three possible object tags of sds objects, that is:
     DFTAG_SD,DFTAG_SDG,DFTAG_NDG. If the object tag of sds object is 
     falling out of this scope, we will not convert annotations into
     hdf5 attributes; it is user's responsibility to make sure object tags
     for sds objects are only one of the above three tags.*/

  if(Annoobj_h4_to_h5(file_id,sds_ref,DFTAG_SD,h5dset)== FAIL){
    printf("failed to convert sds annotation into hdf5 attribute.\n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    return FAIL;
  }

  if(Annoobj_h4_to_h5(file_id,sds_ref,DFTAG_SDG,h5dset)== FAIL){
    printf("failed to convert sds annotation into hdf5 attribute.\n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    return FAIL;
  }

  if(Annoobj_h4_to_h5(file_id,sds_ref,DFTAG_NDG,h5dset)== FAIL){
    printf("failed to convert sds annotation into hdf5 attribute.\n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    return FAIL;
  }
  
  /* convert sds dimensional scale dataset into hdf5 dataset. */
  if(sdsdim_to_h5dataset(sds_id,sds_rank,h5dset,h5_dimgroup,sds_dimsizes[0]) == FAIL) {
    printf("failed to convert dimensional scale to hdf5 dataset. \n");
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    return FAIL;
  }
  check_gloattr = 0;
  if (sds_transattrs(sds_id,h5dset,num_sdsattrs,check_gloattr)==FAIL) {
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    printf(" Error in obtaining sds attributes. \n");
    return FAIL;
  }

  /********************************************/
  /*  handle extra attributes of sds : sds label, object type 
      and reference num */

  strcpy(sdslabel,SDSLABEL);

  if(h4_transpredattrs(h5dset,HDF4_OBJECT_TYPE,sdslabel)==FAIL) {
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    printf("unable to transfer sds label to HDF4 OBJECT TYPE.\n");
    return FAIL;
  }

  if(sdsname[0] != '\0') {
    if(h4_transpredattrs(h5dset,HDF4_OBJECT_NAME,sdsname)==FAIL){
      free(sds_start);
      free(sds_edge);
      free(sds_stride);
      free(sds_data);
      free(chunk_dims);
      H5Sclose(h5d_sid);
      H5Dclose(h5dset);
      H5Pclose(create_plist);
      printf("unable to transfer sds name to HDF5 dataset attribute.\n");
      return FAIL;
    }
  }

  if(h4_transnumattr(h5dset,HDF4_REF_NUM,sds_ref)==FAIL){
    free(sds_start);
    free(sds_edge);
    free(sds_stride);
    free(sds_data);
    free(chunk_dims);
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    H5Pclose(create_plist);
    printf("unable to transfer sds ref. to HDF5 dataset attribute.\n");
    return FAIL;
  }

  istat = SDendaccess(sds_id);
  ret   = H5Pclose(create_plist);
  ret   = H5Sclose(h5d_sid);
  ret   = H5Dclose(h5dset);
  free(sds_data);
  free(sds_start);
  free(sds_edge);
  free(sds_stride);
  free(chunk_dims);
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	sds_transattrs
 *
 * Purpose:     translate attribute of HDF4 SDS object into 
                hdf5 dataset attribute
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                ssds_id: SDS identifier
		sh5_dset: hdf5 dataset
		snum_sdsattrs: number of sds attribute
		check_gloflag:  a flag that check whether the attribute is
		a file attribute or a sds id or a dimensional scale id.

 *-------------------------------------------------------------------------
 */	

int sds_transattrs(int32 ssds_id, hid_t sh5_dset,int snum_sdsattrs,
		   int check_gloflag) {
    
  char      ssdsatrr_name[2*MAX_NC_NAME];
  char      sdsglo[MAX_NC_NAME];
  char*      sdsrepattr_name;
  int32     count_ssdsadata;
  int32     ssds_atype;
  size_t    sh4_amemsize;
  size_t    sh4_asize;
  hid_t     sh5a_sid;
  hid_t     sh5a_id;
  hid_t     sh5_atype;
  hid_t     sh5_amemtype;
  hid_t     sh5str_type;
  hid_t     sh5str_memtype;
  hsize_t   sh5dims[MAX_VAR_DIMS];
  void*     ssds_adata;
  herr_t    sret;
  int       i;

  for (i = 0;i < snum_sdsattrs; i++) {

     if (SDattrinfo(ssds_id,i,ssdsatrr_name,&ssds_atype,
		    &count_ssdsadata)==FAIL){  
        printf("unable to obtain SDS attribute information. \n"); 
        return FAIL;
     }

     /* make a table for the attribute type, to do the corresponding type. */

     if(h4type_to_h5type(ssds_atype,&sh5_amemtype,&sh4_amemsize,
		      &sh4_asize,&sh5_atype)== FAIL) {
       printf("fail to translate sds attribute data type from H4 to H5. \n");
       return FAIL;
     }
    
     ssds_adata = malloc(sh4_amemsize * count_ssdsadata);
     if(ssds_adata == NULL) {
       printf("error, cannot allocate memory for sds attribute data. \n");
       return FAIL;
     }

     if(SDreadattr(ssds_id,i,(VOIDP)ssds_adata)== FAIL) {
       printf("error in reading attributes of sds object. \n");
       free(ssds_adata);
       return FAIL;
     }
	
      /* if attribute doesn't have name, a default name is set. */
     if(ssdsatrr_name[0] == '\0') {
       sdsrepattr_name = trans_obj_name(DFTAG_NDG,i);
       strcpy(ssdsatrr_name,sdsrepattr_name);
       free(sdsrepattr_name);
     }

     /* if the sds attribute is a file attribute. */
     if(check_gloflag == 1){
       strcpy(sdsglo,GLOSDS);
       strcat(ssdsatrr_name,"_");
       strcat(ssdsatrr_name,sdsglo);
     }
       
     /* now do attribute-transferring.
       1. deal with string data type
       2. set attribute space.
       3. get attribute name, set property list. */
           
     if (sh5_atype == H5T_STRING) {

	sh5a_sid = H5Screate(H5S_SCALAR);

	if (sh5a_sid < 0) {
	   printf("failed to create attribute space for"); 
           printf(" HDF4_OBJECT_TYPE SDS. \n"); 
	   free(ssds_adata);
	   return FAIL;
	}

	if ((sh5str_type = mkstr(count_ssdsadata,
				 H5T_STR_SPACEPAD))<0) {
           printf("error in making string. \n");
	   H5Sclose(sh5a_sid);
	   free(ssds_adata);
           return FAIL;
	}

        /* check this line later. */
       	if ((sh5str_memtype = mkstr(count_ssdsadata*sh4_amemsize,
				    H5T_STR_SPACEPAD))<0) {
	  printf("error in making memory string. \n");
	  H5Sclose(sh5a_sid);
	  free(ssds_adata);
	  return FAIL;
	}

        sh5a_id = H5Acreate(sh5_dset,ssdsatrr_name,sh5str_type,
			    sh5a_sid,H5P_DEFAULT);

        if (sh5a_id <0) {
	   printf("failed to obtain attribute id for"); 
           printf(" HDF4_OBJECT_TYPE SDS. \n");
	   H5Sclose(sh5a_sid);
	   free(ssds_adata);
	   return FAIL;
	}

        sret = H5Awrite(sh5a_id,sh5str_memtype,(void *)ssds_adata);
        
	if (sret <0) {
	  printf("failed to write attribute data for"); 
          printf(" HDF4_OBJECT_TYPE SDS. \n");
	  H5Sclose(sh5a_sid);
	  H5Aclose(sh5a_id);
	  free(ssds_adata);
	  return FAIL;
	}

        sret = H5Sclose(sh5a_sid);
        sret = H5Aclose(sh5a_id);
     }
	 
     else {
      
       if(count_ssdsadata == 1) {

	 sh5a_sid = H5Screate(H5S_SCALAR);
	 if (sh5a_sid < 0) {
	   printf("failed to create space id. \n");
	   free(ssds_adata);
	   return FAIL;
	 }
       }
       else {
          sh5dims[0] = count_ssdsadata;
	  sh5a_sid =  H5Screate_simple(1,sh5dims,NULL);
          
	  if (sh5a_sid < 0)  {
	    printf("failed to create attribute space. \n");
	    free(ssds_adata);
	    return FAIL;
	  }
       }
       sh5a_id = H5Acreate(sh5_dset,ssdsatrr_name,sh5_atype,
			   sh5a_sid,H5P_DEFAULT);
    
       if(sh5a_id <0) {
	 printf("failed to obtain attribute id. \n");
	 H5Sclose(sh5a_sid);
	 free(ssds_adata);
         return FAIL;
       }

       sret = H5Awrite(sh5a_id,sh5_amemtype,(void *)ssds_adata);

       if(sret <0) {
	 printf("failed to write attribute data.\n ");
	 H5Sclose(sh5a_sid);
	 H5Aclose(sh5a_id);
	 free(ssds_adata);
	 return FAIL;
       }
       sret = H5Sclose(sh5a_sid);
       sret = H5Aclose(sh5a_id);
     }
     free(ssds_adata);
  }
  return SUCCEED;
}
/****************sdsdim_to_h5dataset*******************

 * Purpose:     translate dimensional scale dataset into 
                hdf5 dataset 
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                sds_id: SDS identifier
                sds_rank: number of sds dimensions
   Out:
   Modification:

 *-------------------------------------------------------------------------
 */	

int sdsdim_to_h5dataset(int32 sds_id,int32 sds_rank,hid_t sh5dset,
			hid_t sh5_dimgroup,int32 firstdimsize) {

  int32   sdsdim_id;
  int32   sdsdim_type = 0;
  int32   sds_dimscasize[1];
  int32   istat;
  int     i;
  int     count_h5objref;/* this counter updates the number of h5 object reference. */
  int     count_h5attrname;/*this counter updates the number of h5 dimensional name attribute.*/

  int     check_gloattr;
  int32   num_sdsdimattrs;
  int     check_sdsdim;
  void*   dim_scadata; 
  
  char    sdsdim_name[MAX_NC_NAME+1];
  char*   cor_sdsdimname;
  size_t  h4dim_memsize;
  size_t  h4dim_size;

  HDF_CHUNK_DEF c_def_out;
  int32   c_flags;

  /* define varibles for hdf5. */

  hid_t   h5dim_dset;
  hid_t   h5dim_sid;

  hid_t   h5dim_tid;
  hid_t   h5dim_memtype;

  hid_t   h5dim_nameaid;
  hid_t   h5dim_namesid;

  hid_t   h5str_dimntype;

  hid_t   attr_refSpace;
  hid_t   attr_refType;
  hid_t   attribID;
  hid_t   create_plist;

  hsize_t h5dimscas[1];
  hsize_t max_h5dimscas[1];
  hsize_t h5dim_dims[1];
  hsize_t attr_refDims[1];
  hsize_t h5dim_chunkdim[1];
  hobj_ref_t  dim_refdat;

  hobj_ref_t* alldim_refdat;
  
  char*   h5sdsdim_name;
  char    h5sdsdim_allname[MAX_VAR_DIMS * MAX_DIM_NAME];
  char    h5newsdsdim_name[MAX_DIM_NAME];
  char    h5dimpath_name[MAX_DIM_NAME];
  herr_t  ret;

  
  /*zero out memory for h5sdsdim_allname and h5dimpath_name */
  h4toh5_ZeroMemory(h5sdsdim_allname,(MAX_VAR_DIMS*MAX_DIM_NAME)*sizeof(char));
  h4toh5_ZeroMemory(h5dimpath_name,MAX_DIM_NAME*sizeof(char));

  /*check whether the sds is created with unlimited dimension. */

  if(SDgetchunkinfo(sds_id,&c_def_out, &c_flags)== FAIL) {
    printf("error in getting chunking information. \n");
    return FAIL;
  }

  /* initialize the dimensional number of sds dimensions, h5dim_dims
   is used for grabbing hdf5 dimensional name list and object reference
  list. */
  h5dim_dims[0] = (hsize_t)sds_rank;
  count_h5objref = 0;
  count_h5attrname = 0;

  for (i = 0; i<sds_rank;i++) {						      
								      
    sdsdim_id    = SDgetdimid(sds_id,i);	
      
    if(sdsdim_id == FAIL) {
      printf("error in obtaining sds dimension id. \n");
      return FAIL;
    }
    
    istat = SDdiminfo(sdsdim_id,sdsdim_name,sds_dimscasize,
		      &sdsdim_type,&num_sdsdimattrs);  

    if (istat == FAIL) {						      
      printf("sds get dim. information failed. \n");		
      SDendaccess(sdsdim_id);		
      return FAIL;							      
    }	

    /* for unlimited sds dimension, grab the current dimensional size. */
    if(sds_dimscasize[0] == 0) sds_dimscasize[0] = firstdimsize;
 
    /* check whether this dimensional scale dataset is looked up. */	
    check_sdsdim = lookup_name(sdsdim_name,DIM_HASHSIZE,dim_hashtab);	      
      									      
    strcpy(h5dimpath_name,HDF4_DIMG);

    /* checking whether sds dimension scale name contains ORI_SLASH, changing into CHA_SLASH.*/

    cor_sdsdimname = correct_name(sdsdim_name);
    if(cor_sdsdimname == NULL) {
      printf("error in generating corrected sds dimensional scale name.\n");
      SDendaccess(sdsdim_id);
      return FAIL;
    }

    /* generating hdf5 dimensional scale name. */
    h5sdsdim_name = get_obj_aboname(cor_sdsdimname,NULL,h5dimpath_name,NULL);
    if (h5sdsdim_name == NULL) {		      
      printf("error in getting hdf5 sds dimension name.\n");
      SDendaccess(sdsdim_id);		
      free(cor_sdsdimname);
      return FAIL;							      
    }
    free(cor_sdsdimname);

    strcpy(&h5sdsdim_allname[count_h5attrname*MAX_DIM_NAME],h5sdsdim_name);

    /* here we should add some comments for fakedim0--name. It seems that
       hdf4(netcdf) will use unique fake dimension name, fakedim + unique 
       number, so check_sdsdim will never be 1 if the dimension name is fake
       name. Under this case, count_h5objref and count_h5attrname
        will not increase if this dimension doesnot
       have dimensional scale data. That assures the object reference of sds is
       correct. */
      
    /*if this dimension is not touched, get name of the dimensional scale data. */     
    if (check_sdsdim == 1){/* the dimension is touched, skip this one.*/
      free(h5sdsdim_name);
      SDendaccess(sdsdim_id);	
      count_h5objref = count_h5objref + 1;
      count_h5attrname = count_h5attrname + 1;
      continue;
    }   
   									      
    if (check_sdsdim != 0) {						      
      printf("error in checking sds dimensions.\n");	
      SDendaccess(sdsdim_id);		
      free(h5sdsdim_name);
      return FAIL;							      
    }									      

     /* if this sds dimension has no dimensional scale data. skip it.*/
    if(sdsdim_type == 0) 
      continue;
    

    /* get h5 dimensional scale data type. */
    if(h4type_to_h5type(sdsdim_type,&h5dim_memtype,&h4dim_memsize,	      
			&h4dim_size,&h5dim_tid)== FAIL) {
      printf("error in transferring sds dimension data type.\n");
      SDendaccess(sdsdim_id);		
      free(h5sdsdim_name);
      return FAIL;
    }

    /* dimensional scale dataset cannot be H5T_STRING data type.
	 So transferring back to int8 */

    if (h5dim_tid == H5T_STRING) {					      
      if(h5string_to_int(sdsdim_type,&h5dim_memtype,h4dim_memsize,
			 &h5dim_tid)==FAIL){		
	printf("error in translating from string to int. \n");
	SDendaccess(sdsdim_id);		
	free(h5sdsdim_name);
	return FAIL;							      
      }								      
    }									      

    /* get the dimensional scale data. */
    dim_scadata = malloc(h4dim_memsize*sds_dimscasize[0]);		      
    istat       = SDgetdimscale(sdsdim_id,(VOIDP)dim_scadata);	      
									      
    if (istat == FAIL) {						      
      printf("sds get dim. scale failed. \n");	
      SDendaccess(sdsdim_id);		
      free(h5sdsdim_name);
      free(dim_scadata);
      return FAIL;							      
    }									      
    
    /* set dimensional scale size properly. */
    h5dimscas[0] = sds_dimscasize[0];	

    /* only set for the first dimension if SDS is unlimited dimension. */	
    if(SDisrecord(sds_id) && i == 0)
      max_h5dimscas[0] = H5S_UNLIMITED;
    else
      max_h5dimscas[0] = h5dimscas[0];

    h5dim_sid    = H5Screate_simple(1,h5dimscas,max_h5dimscas);		      
	
    if(h5dim_sid <0) {
      printf("error in creating space. \n");
      SDendaccess(sdsdim_id);		
      free(h5sdsdim_name);
      free(dim_scadata);
      return FAIL;
    }

    /* create property list, for chunked sds or unlimited dimension cases */

  create_plist = H5Pcreate(H5P_DATASET_CREATE);

  if(create_plist == -1) {
    printf("failed to create property list. \n");
    SDendaccess(sdsdim_id);		
    free(h5sdsdim_name);
    free(dim_scadata);
    H5Sclose(h5dim_sid);
  }

    
  if(c_flags == HDF_NONE && SDisrecord(sds_id) && i == 0)
 {
      h5dim_chunkdim[0] = (hsize_t)(h5dimscas[0]/2);
   
    if(H5Pset_chunk(create_plist,1, h5dim_chunkdim)<0) {
       printf("failed to set up chunking information for ");
       printf("dimensional scale property list.\n");
       SDendaccess(sdsdim_id);		
       free(h5sdsdim_name);
       free(dim_scadata);
       H5Sclose(h5dim_sid);
       H5Pclose(create_plist);
       return FAIL;	
    }

 }

  if(c_flags == HDF_CHUNK || c_flags == (HDF_CHUNK | HDF_COMP)
     || c_flags == (HDF_CHUNK | HDF_NBIT)  ){
     
      h5dim_chunkdim[0] = (hsize_t)c_def_out.chunk_lengths[0]; 
       
    if(H5Pset_chunk(create_plist,1, h5dim_chunkdim)<0) {
       printf("failed to set up chunking information for ");
       printf("property list.\n");
       SDendaccess(sdsdim_id);		
       free(h5sdsdim_name);
       free(dim_scadata);
       H5Sclose(h5dim_sid);
       H5Pclose(create_plist);
       return FAIL;	
     }
  }
  
    /* create h5 dataset under group HDF4_DIMG*/
    h5dim_dset   = H5Dcreate(sh5_dimgroup,h5sdsdim_name,h5dim_tid,	      
			     h5dim_sid,create_plist);			      
    	
    if(h5dim_dset <0) {
      printf("error in creating dataset. \n");
      free(h5sdsdim_name);
      free(dim_scadata);
      SDendaccess(sdsdim_id);		
      H5Sclose(h5dim_sid);
      H5Pclose(create_plist);
      return FAIL;
    }

    if (H5Dwrite(h5dim_dset,h5dim_memtype,h5dim_sid,h5dim_sid,	      
		 H5P_DEFAULT,(void *)dim_scadata)<0) {		      
      printf("error writing data\n");
      free(h5sdsdim_name);
      free(dim_scadata);
      SDendaccess(sdsdim_id);	
      H5Sclose(h5dim_sid);
      H5Pclose(create_plist);
      H5Dclose(h5dim_dset);
      return FAIL;							      
    }									      
	
    check_gloattr = 0;
    if(sds_transattrs(sdsdim_id,h5dim_dset,num_sdsdimattrs,check_gloattr)
       == FAIL){
      printf("error in transferring attributes. \n");
      free(h5sdsdim_name);
      free(dim_scadata);
      SDendaccess(sdsdim_id);	
      H5Sclose(h5dim_sid);
      H5Dclose(h5dim_dset);
      H5Pclose(create_plist);
      return FAIL;	
    }	
    SDendaccess(sdsdim_id);						      
    free(dim_scadata);	
    free(h5sdsdim_name);
    ret = H5Sclose(h5dim_sid);					      
    ret = H5Dclose(h5dim_dset);	
    ret = H5Pclose(create_plist);
    count_h5objref = count_h5objref + 1;
    count_h5attrname =count_h5attrname  + 1;
  }								      
  
  /*1. create object reference number to dimensional scale dataset.
    2. store absolute name of dimensional name into 
     dimensional list. */
  
  if ( count_h5objref != 0) {

     h5dim_dims[0]   = count_h5objref;
     attr_refDims[0] = count_h5objref;
     attr_refSpace   = H5Screate_simple(1,attr_refDims,NULL);
     attr_refType    = H5Tcopy(H5T_STD_REF_OBJ);
     alldim_refdat   = calloc((size_t)count_h5objref,sizeof(hobj_ref_t));

     if(alldim_refdat == NULL) {
       printf("error in allocating memory. \n");
       H5Sclose(attr_refSpace);
       H5Tclose(attr_refType);
       return FAIL;
     }

     for(i=0;i<count_h5objref;i++){
       h4toh5_ZeroMemory(h5newsdsdim_name,MAX_DIM_NAME);
       strcpy(h5newsdsdim_name,&h5sdsdim_allname[i*MAX_DIM_NAME]);
       
       ret              = H5Rcreate(&dim_refdat,sh5_dimgroup,h5newsdsdim_name,
				    H5R_OBJECT,-1);
       if(ret <0) {
	 free(alldim_refdat);
	 H5Sclose(attr_refSpace);
	 H5Tclose(attr_refType);	 
	 printf("error in generating H5 reference. \n");
	 return FAIL;
       }
       alldim_refdat[i] = dim_refdat;
       
     }

     attribID      = H5Acreate(sh5dset,DIMSCALE,attr_refType,attr_refSpace,
				H5P_DEFAULT);
     if(attribID < 0) {
       free(alldim_refdat);
       H5Sclose(attr_refSpace);
       H5Tclose(attr_refType);
       H5Aclose(attribID);
       printf("error in generating H5 attribute ID. \n");
       return FAIL;
     }

     ret           = H5Awrite(attribID,attr_refType,(void *)alldim_refdat);
     
     H5Sclose(attr_refSpace);
     H5Tclose(attr_refType);
     H5Aclose(attribID);
     free(alldim_refdat);
  }

  if(count_h5attrname!= 0) {

    h5dim_namesid    = H5Screate_simple(1,h5dim_dims,NULL);	

    if(h5dim_namesid <0) {
      printf("error in creating sds dimensionlist space.\n");
      return FAIL;
    }

    h5str_dimntype   = mkstr(MAX_DIM_NAME,H5T_STR_SPACEPAD);	
    if(h5str_dimntype < 0) {
      H5Sclose(h5dim_namesid);
      printf("error in generating H5T_STRING type.\n");
      return FAIL;
    }

    h5dim_nameaid    = H5Acreate(sh5dset,HDF4_DIMENSION_LIST,h5str_dimntype,
				 h5dim_namesid,H5P_DEFAULT);		      

    if(h5dim_nameaid <0) {
      H5Sclose(h5dim_namesid);
      printf("error in creating sds dimensionlist id.\n");
      return FAIL;
    }

    ret = H5Awrite(h5dim_nameaid,h5str_dimntype,h5sdsdim_allname);

    if(ret < 0) {
      H5Sclose(h5dim_namesid);
      H5Aclose(h5dim_nameaid);
      printf("error in writing sds dimensionlist. \n");
      return FAIL;
    }
    
    ret = H5Sclose(h5dim_namesid);
    ret = H5Aclose(h5dim_nameaid);

  }
  return SUCCEED;
}




































