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

Converting an hdf4 independent vdata object into an hdf5 dataset of compound dataset.

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/

#include "h4toh5main.h"
#include <assert.h>

/*-------------------------------------------------------------------------
 * Function:	Vdata_h4_to_h5
 *
 * Purpose:     translate Vdata object into hdf5 dataset 
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                vdata_id: RI identifier
		group_id: hdf5 group id
   Out:

   Modification:
 *-------------------------------------------------------------------------
 */	

int Vdata_h4_to_h5(int32 file_id,int32 vdata_id, hid_t group_id) {

  /* define variables for hdf4. */

  int32     istat;
  int32     n_records;

  int32     vdata_ref;
  int32     vdata_tag;

  int32     interlace_mode;
  
  int32     vdata_size;
  int32     vdatamem_size;

  int32     field_index;
  int32     fieldorder;
  int32     fieldtype;

  int       i;
  int32       nfields;
  int       num_vd_attrs;
  int       num_vd_field_attrs;

  VOIDP     vd_data;

  char      vdlabel[10];
  char      vdata_name[MAX_NC_NAME];
  char      fieldname[MAX_NC_NAME];
  char      vdata_class[VSNAMELENMAX];
  char      field_name_list[VSFIELDMAX*FIELDNAMELENMAX];

  /* define varibles for hdf5. */

  hid_t     h5d_sid;
  hid_t     h5dset;

  hid_t     h5_ctype;
  hid_t     h5_cmemtype;

  hid_t*     h5memtype = NULL;
  hid_t*     h5type = NULL;

  size_t*    h4memsize = NULL;
  size_t*    h4size = NULL;
  hsize_t   h5_vddims[1];
  char*     h5cvdata_name;

  int       check_vdname;

   /* Zeroing out memory for vdlabel,vdata_class,vdata_name */

    h4toh5_ZeroMemory(vdata_name,MAX_NC_NAME);
     h4toh5_ZeroMemory(fieldname,MAX_NC_NAME);
     h4toh5_ZeroMemory(vdata_class,VSNAMELENMAX);
     h4toh5_ZeroMemory(field_name_list,VSFIELDMAX*FIELDNAMELENMAX);
    h4toh5_ZeroMemory(vdlabel,10);

  /* get absolute path of vdata name. */

  vdata_ref =  VSQueryref(vdata_id);
  if (vdata_ref == FAIL) {
    printf("error in getting reference number. \n");
    return FAIL;
  }

  vdata_tag = VSQuerytag(vdata_id);
  if (vdata_tag == FAIL) {
    printf("error in getting object tag number. \n");
    return FAIL;
  }

  /* get the class name */

  if(VSgetclass(vdata_id,vdata_class) == FAIL) {
    printf("error in obtaining class name. \n");
    return FAIL;
  }

  /* get number of record,field_name,Size of a record and
     Name of the vdata*/ 

  if(VSQueryvsize(vdata_id,&vdata_size)==FAIL) {
    printf("error in getting size of vdata. \n");
    return FAIL;
  }

  if(vdata_size == 0) {/* empty vdata set. */
    return SUCCEED;
  }
  
  /* obtain number of records, field name list, vdata name. */
  if(VSinquire(vdata_id,&n_records,&interlace_mode,
	       field_name_list,&vdata_size,vdata_name) == FAIL) {
    printf("error in inquiring vdata. \n");
    return FAIL;
  }
 
  vdatamem_size = 0;
  vdata_size = 0;
  nfields = VFnfields(vdata_id);

  if (nfields == FAIL) {
    printf("error in obtaining number of vdata fields. \n");
    return FAIL;
  }

  assert(nfields>0);
  h5memtype = calloc((size_t)nfields,sizeof(hid_t));
  h5type    = calloc((size_t)nfields,sizeof(hid_t));
  h4memsize = calloc((size_t)nfields,sizeof(size_t));
  h4size    = calloc((size_t)nfields,sizeof(size_t));

  for (i=0;i<nfields;i++) {
     
    /* obtain field type. */
     fieldtype = VFfieldtype(vdata_id,i);
     if(fieldtype == FAIL){
       printf("error in obtaining field type. \n");
       free(h5memtype);
       free(h5type);
       free(h4memsize);
       free(h4size);
       return FAIL;
     }

     /* obtain field order.*/
     fieldorder = VFfieldorder(vdata_id,i);  
     /*     printf("fieldorder %d\n",fieldorder);*/
     if(fieldorder == FAIL){
       printf("error in obtaining field order. \n");
       free(h5memtype);
       free(h5type);
       free(h4memsize);
       free(h4size);
       return FAIL;
     }

     /* datatype conversion from hdf4 to hdf5.
	the corresponding memory data type is also converted.*/
     if(h4type_to_h5type(fieldtype,&h5memtype[i],&h4memsize[i],
			 &h4size[i],&h5type[i])== FAIL){
       printf("error in doing datatype conversion at vdata routine. \n");
       free(h5memtype);
       free(h5type);
       free(h4memsize);
       free(h4size);
       return FAIL;
     }

     vdatamem_size +=fieldorder*h4memsize[i];
     vdata_size +=fieldorder*h4size[i];
     
  }
 
  vd_data = malloc((size_t)(vdatamem_size*n_records));   

  istat   = VSsetfields(vdata_id,field_name_list); 
  
  if(istat == FAIL) {
    printf("error setting fields of vdata.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  istat = VSread(vdata_id,(uint8*)vd_data,n_records,FULL_INTERLACE);

  if(istat == FAIL) {
    printf("error in obtaining vdata. \n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  for (i=0;i<nfields;i++) {
    /* obtain field order.*/
     fieldorder = VFfieldorder(vdata_id,i);  

     if(fieldorder == FAIL){
       printf("error in obtaining field order. \n");
       free(h5memtype);
       free(h5type);
       free(h4memsize);
       free(h4size);
       return FAIL;
     }
  }
  /*  create hdf5 compound datatype for both memory and file.*/

  h5_ctype       = H5Tcreate(H5T_COMPOUND, (size_t)vdata_size);
  h5_cmemtype    = H5Tcreate(H5T_COMPOUND,(size_t)vdatamem_size);

  if(gen_h5comptype(vdata_id,nfields,h4size,h4memsize,h5type,h5memtype,
		    h5_ctype,h5_cmemtype)==FAIL){
    printf("error in generating h5 compound data type.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  h5_vddims[0] = n_records;
  h5d_sid = H5Screate_simple(1,h5_vddims,NULL);

  if(h5d_sid <0){
    printf("error in obtaining space id.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  /* choose a number that is not returned from the func.*/
  check_vdname  =  -3;

  /* obtain hdf5 vdata name. */
  h5cvdata_name =  get_name(vdata_ref,estnum_vd,vd_hashtab,&check_vdname);
  
  if (h5cvdata_name == NULL && check_vdname == 0 ) {
    printf("error,cannot find vdata \n");
    return FAIL;
  }

  if (h5cvdata_name == NULL && check_vdname == -1) {
    printf("error,group name is not defined.\n");
    return FAIL;
  }

  if (h5cvdata_name == NULL && check_vdname == -2 ) {
    printf("cannot allocate memory for vdata.\n");
    return FAIL;
  }

  h5dset = H5Dcreate(group_id,h5cvdata_name,h5_ctype,h5d_sid,H5P_DEFAULT);
  if(h5dset <0) {
    printf("error in obtaining dataset.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);       
    free(h5cvdata_name);
    return FAIL;
  }
  free(h5cvdata_name);

  if(H5Dwrite(h5dset,h5_cmemtype,H5S_ALL,H5S_ALL,H5P_DEFAULT,vd_data)<0){
    printf("error in writing dataset converted from vdata.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  /* handle vdata attributes and vdata field attributes. */

  num_vd_attrs = VSfnattrs(vdata_id,_HDF_VDATA);

  if (num_vd_attrs == FAIL) {
    printf("error in obtaining attributes of vdata.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }
    
  /* when field_index = -1, only transfer vdata attribute.*/

  field_index = -1;
  if(vdata_transattrs(vdata_id,h5dset,num_vd_attrs,field_index,NULL)==FAIL){
    printf("error in translating vdata attibutes.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  for (i =0;i< nfields;i++) {

    if(VFfieldname(vdata_id,i)== NULL) {
      printf("error in obtaining field name. \n");
      free(h5memtype);
      free(h5type);
      free(h4memsize);
      free(h4size);
      free(vd_data);
      return FAIL;
    }

    strcpy(fieldname,VFfieldname(vdata_id,i));
    num_vd_field_attrs = VSfnattrs(vdata_id,i);
    if(num_vd_field_attrs == FAIL){
      printf("error in number of vd field attribute \n");
      free(h5memtype);
      free(h5type);
      free(h4memsize);
      free(h4size);
      free(vd_data);
      return FAIL;
    }
       
    if(vdata_transattrs(vdata_id,h5dset,num_vd_field_attrs,i,fieldname)
       ==FAIL){
      printf("error in transfering vdata attributes.\n");
      free(h5memtype);
      free(h5type);
      free(h4memsize);
      free(h4size);
      free(vd_data);
      return FAIL;
    }
  }
  /* converting annotations of vdata into corresponding hdf5 attribute.*/
  if( Annoobj_h4_to_h5(file_id,vdata_ref,vdata_tag,h5dset)== FAIL){
    printf("fail to convert HDF4 VDATA annotation into hdf5 attributes.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  /* converting predefined attributes. */
  strcpy(vdlabel,VDATALABEL);
  if(h4_transpredattrs(h5dset,HDF4_OBJECT_TYPE,vdlabel)==FAIL){
    printf("error in transfering vdata attributes.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }

  if(vdata_name[0] != '\0') {
    if(h4_transpredattrs(h5dset,HDF4_OBJECT_NAME,vdata_name)==FAIL){
      printf("error in transfering vdata attributes.\n");
      free(h5memtype);
      free(h5type);
      free(h4memsize);
      free(h4size);
      free(vd_data);
      return FAIL;
    }
  }
        
  if(h4_transnumattr(h5dset,HDF4_REF_NUM,vdata_ref)==FAIL){
    printf("error in transfering vdata attributes.\n");
    free(h5memtype);
    free(h5type);
    free(h4memsize);
    free(h4size);
    free(vd_data);
    return FAIL;
  }
 
  H5Sclose(h5d_sid);
  H5Dclose(h5dset);
  VSdetach(vdata_id);
  free(h5memtype);
  free(h5type);
  free(h4memsize);
  free(h4size);
  free(vd_data);
  return SUCCEED;
}
          
/*-------------------------------------------------------------------------
 * Function:	vdata_transattrs
 *
 * Purpose:     translate Vdata attributes into attributes of the
                corresponding hdf5 dataset 
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                vdata_id: vdata identifier
		h5dset: hdf5 dataset
		snum_vdattrs: number of vd attributes
		field_index:  index of vdata fields
		attr_name: vdata(or vdata field) attribute name
   Out:
   Modifications:

 *-------------------------------------------------------------------------
 */	

int  vdata_transattrs(int32 vdata_id,hid_t h5dset,int snum_vdattrs,
		      int field_index,char* attr_name){
   
  char      svdattr_name[2*MAX_NC_NAME];
  char*     svdrepattr_name;
  char      refstr[MAXREF_LENGTH];

  int32     count_svdadata;
  int32     svd_atype;

  size_t    sh4_amemsize;
  size_t    sh4_asize;

  hid_t     sh5a_sid;
  hid_t     sh5a_id;
  hid_t     sh5_atype;
  hid_t     sh5_amemtype;
  hid_t     sh5str_type;
  hid_t     sh5str_memtype;

  hsize_t   sh5dims[1];
  void*     svd_adata;
  herr_t    sret;
  int       i;

      /* zeroing out memory for svdattr_name and refstr */                                  
     h4toh5_ZeroMemory(svdattr_name,2*MAX_NC_NAME); 
     h4toh5_ZeroMemory(refstr,MAXREF_LENGTH);          

  /* separate vdata attribute from vdata field attributes. */

  if (field_index < -1) {
    printf("error: check_field should be either -1(vdata) or ");
    printf(">=0(vdata field).\n"); 
    return FAIL;
  }

  for (i = 0;i < snum_vdattrs; i++) {

    /* if the field_index is 0, no field attribute exists, only
       VDATA attributes are converted.*/

    if (VSattrinfo(vdata_id,field_index,i,svdattr_name,&svd_atype,
        	     &count_svdadata,NULL)== FAIL){  
       printf("unable to obtain attribute information. \n"); 
       return FAIL;
      }
         
    if(svdattr_name[0] == '\0') {
       svdrepattr_name = trans_obj_name(DFTAG_VG,i);
       strcpy(svdattr_name,svdrepattr_name);
       free(svdrepattr_name);
     }

    if (field_index == -1);

    else if (field_index != -1 && attr_name != NULL) {

       strcat(svdattr_name,":");
       strcat(svdattr_name,attr_name);
    }

    else {

       strcat(svdattr_name,":");
       strcat(svdattr_name,"HDF4_VDATA_ATTR_");
       if(conv_int_str(field_index,refstr)==FAIL) {
	 printf("error in converting vdata field index to string.\n");
	 return FAIL;
       }
       strcat(svdattr_name,refstr);
      
    }

     /* converting attribute data type into the corresponding hdf5 data type */

   if(h4type_to_h5type(svd_atype,&sh5_amemtype,&sh4_amemsize,
		       &sh4_asize,&sh5_atype)==FAIL){
     printf("fail to translate vdata attribute datatype from H4 to H5.\n");
     return FAIL;
   }

   svd_adata = malloc(sh4_amemsize * count_svdadata);

   if(svd_adata == NULL) {
     printf("fail to allocate memory for vdata attribute data.\n");
     return FAIL;
   }

   if(VSgetattr(vdata_id,field_index,i,(VOIDP)svd_adata)==FAIL){
     printf("error in getting attributes of vdata. \n");
     free(svd_adata);
     return FAIL;
   }
	
     /* now do attribute-transferring:
       1. deal with string data type
       2. set attribute space
       3. get attribute name  */
           
   if (sh5_atype == H5T_STRING) {

      if ((sh5str_type = mkstr(count_svdadata,
			       H5T_STR_SPACEPAD))<0) {
         printf("error in making string for vdata attribute. \n");
	 free(svd_adata);
         return FAIL;
      }

      if ((sh5str_memtype = mkstr(count_svdadata*sh4_amemsize,
				    H5T_STR_SPACEPAD))<0) {
	  printf("error in making memory string for vdata attribute. \n");
	  free(svd_adata);
	  return FAIL;
      }

      sh5a_sid = H5Screate(H5S_SCALAR);

      if (sh5a_sid < 0) {
         printf("failed to create attribute space for ");
         printf("HDF4_OBJECT_TYPE VDATA. \n"); 
	 free(svd_adata);
         return FAIL;
      }


      sh5a_id = H5Acreate(h5dset,svdattr_name,sh5str_type,
			    sh5a_sid,H5P_DEFAULT);

      if (sh5a_id <0) {
	 printf("failed to obtain attribute id for"); 
	 printf(" HDF4_OBJECT_TYPE VDATA. \n");
	 H5Sclose(sh5a_sid);
	 free(svd_adata);
	 return FAIL;
      }

      sret = H5Awrite(sh5a_id,sh5str_memtype,(void *)svd_adata);
        
      if (sret <0) {
	 printf("fail to write vdata attr into hdf5 dataset attr\n ");
	 H5Sclose(sh5a_sid);
	 H5Aclose(sh5a_id);
	 free(svd_adata);
	 return FAIL;
      }

      free(svd_adata);
      sret = H5Sclose(sh5a_sid);
      sret = H5Aclose(sh5a_id);
   }
	 
   else {
      
      if(count_svdadata == 1) {
        sh5a_sid = H5Screate(H5S_SCALAR);

        if (sh5a_sid < 0) {
	  printf("failed to create scalar space id for hdf5 attribute ");
	  printf("of dataset converted from attribute of VDATA.\n");
	  free(svd_adata);
	  return FAIL;
	}
      }
      else {
        sh5dims[0] = count_svdadata;
        sh5a_sid =  H5Screate_simple(1,sh5dims,NULL);
          
        if (sh5a_sid < 0)  {
	  printf("failed to create simple space id for hdf5 attribute ");
	  printf("of dataset converted from attribute of VDATA.\n");
	  free(svd_adata);
	  return FAIL;
	}
      }

      sh5a_id = H5Acreate(h5dset,svdattr_name,sh5_atype,
			  sh5a_sid,H5P_DEFAULT);
    
      if(sh5a_id <0) {
	printf("failed to create attribute id for hdf5 attribute ");
        printf("of dataset converted from attribute of VDATA.\n");
	H5Sclose(sh5a_sid);
	free(svd_adata);
	return FAIL;
      }

      sret = H5Awrite(sh5a_id,sh5_amemtype,(void *)svd_adata);

      if(sret <0) {
	printf("failed to write attribute data for hdf5 attribute ");
        printf("of dataset converted from attribute of VDATA.\n");
	H5Sclose(sh5a_sid);
	H5Aclose(sh5a_id);
	free(svd_adata);
	return FAIL;
      }

      sret = H5Aclose(sh5a_id);
      sret = H5Sclose(sh5a_sid);
      free(svd_adata);
   }
  }
  return SUCCEED;
}
/*-------------------------------------------------------------------------
 * Function:	gen_h5comptype
 *
 * Purpose:     generate hdf5 compound data type
                 
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                vdata_id: vdata identifier
		nfields: number of fields
		sh4size: pointer to datatype size in memory
		sh4memsize: pointer to datatype size in memory
		sh5type: pointer to hdf5 datatype
		sh5memtype: pointer to actual hdf5 datatype in memory
		h5_ctype:  hdf5 compound datatype
		h5_cmemtype: hdf5 compound datatype in memory
   Out:
   Modifications:

 *-------------------------------------------------------------------------
 */	

int gen_h5comptype(int32 vdata_id,int32 nfields,
		   size_t* sh4size,size_t* sh4memsize,
		  hid_t* sh5type,hid_t* sh5memtype,
		  hid_t h5_ctype,hid_t h5_cmemtype) {

  char* fieldname;
  int32   fieldorder;
  size_t  fil_offset;
  size_t  mem_offset;
  hsize_t  fielddim[1];
  hid_t   h5str_type;
  int     check_ifstr;/* flag to check if the h5 type is string.*/
  int     i;


  check_ifstr    = 0;
  fil_offset     = 0;
  mem_offset     = 0;
  

 for (i =0;i< nfields;i++) {
   
   fieldname = NULL;
   fieldorder = VFfieldorder(vdata_id,i);
   
   if(fieldorder == FAIL){
     printf("error in obtaining fieldorder.\n");
     return FAIL;
   }

   fieldname = VFfieldname(vdata_id,i);
   if(fieldname == NULL){
     printf("fail to obtain Vdata field name. \n");
     return FAIL;
   }
   

   /* when vdata is a character array, we will write the whole
      array as one hdf5 type string. */

   if(sh5type[i] == H5T_STRING) {

    if ((h5str_type = mkstr(sh4size[i]*fieldorder,H5T_STR_SPACEPAD))<0) {
       printf("error in making string of hdf5 string. \n");
       return FAIL;
     }
     sh5type[i] = h5str_type;
     check_ifstr  = 1;
   }

   if (sh5memtype[i] == H5T_STRING) {

     if((h5str_type = mkstr(sh4memsize[i]*fieldorder,H5T_STR_SPACEPAD))<0){ 
       printf("error in making string for VDATA in memory. \n");
       return FAIL;
     }
     sh5memtype[i] = h5str_type;

   }

   fielddim[0] = fieldorder;
        
     /* if field type is an array, use H5Tinsert_array. 
      When the data type is H5T_STRING, 
      we will treat the the vdata  as a HDF5 scalar type.*/

   if (fielddim[0] == 1 || check_ifstr == 1) {

     if(H5Tinsert(h5_ctype,fieldname,fil_offset,sh5type[i])<0) {
       printf("error inserting hdf5 compound datatype while ");
       printf("converting vdata.\n");
       return FAIL;
     }

     if(H5Tinsert(h5_cmemtype,fieldname,mem_offset,sh5memtype[i])<0){
       printf("error inserting hdf5 compound datatype of memory");
       printf(" while converting vdata.\n");
       return FAIL;
     }
   }
      
   else {
     hid_t arr_type;    /* Array datatype for inserting fields */

     /* Create array datatype */
     if((arr_type=H5Tarray_create(sh5type[i],1,fielddim,NULL))<0) {
       printf("error creating array datatype.\n");
       return FAIL;
     }

     if(H5Tinsert(h5_ctype,fieldname,fil_offset,arr_type)<0) {
       printf("error inserting array into hdf5 compound datatype. \n");
       return FAIL;
     }

     /* Close array datatype */
     if(H5Tclose(arr_type)<0) {
       printf("error closing array datatype.\n");
       return FAIL;
     }

     /* Create array datatype */
     if((arr_type=H5Tarray_create(sh5memtype[i],1,fielddim,NULL))<0) {
       printf("error creating array datatype.\n");
       return FAIL;
     }

     if(H5Tinsert(h5_cmemtype,fieldname,mem_offset,arr_type)<0) {
       printf("error inserting array into hdf5 compound datatype for memory. \n");
       return FAIL;
     }

     /* Close array datatype */
     if(H5Tclose(arr_type)<0) {
       printf("error closing array datatype.\n");
       return FAIL;
     }

     
   }

   if( check_ifstr == 1) {
     fil_offset = fil_offset + sh4size[i]*fieldorder;
     mem_offset = mem_offset + sh4memsize[i]*fieldorder;
     check_ifstr = 0;
   }
   else { 
   
     fil_offset = fil_offset + sh4size[i]*fieldorder;
     mem_offset = mem_offset + sh4memsize[i]*fieldorder;
   }

 }

 return SUCCEED;
}
   
   











 
