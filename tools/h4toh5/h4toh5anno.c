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

converting an hdf4 annotation into an hdf5 attribute of the corresponding object.


Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/

#include "h4toh5main.h"


/*-------------------------------------------------------------------------
 * Function:	Annofil_h4_to_h5
 *
 * Purpose:     translate file annotation object into hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: file identifier
		h5_group: hdf5 group id
	
  *-------------------------------------------------------------------------
 */		

int Annofil_h4_to_h5(int32 file_id,hid_t h5group){

  int32    an_id;
  int32    ann_id;
  int32    i;

  int32    ann_length;

  int32    n_file_label = 0;
  int32    n_file_desc  = 0;
  int32    n_data_label = 0;
  int32    n_data_desc  = 0;

  int32    istat;

  char*    ann_buf;
  char     anno_labelname[30];
  char     anno_descname[30];
  char     index_str[5];

  hid_t    h5_sid;
  hid_t    h5_aid;
  hid_t    sh5str_type; 
  hid_t    sh5str1_type;
  hid_t    ret;

  an_id    = ANstart(file_id);

  if(an_id < 0) {
    printf("error in obtaining an_id. \n");
    return FAIL;
  }

  istat    = ANfileinfo(an_id,&n_file_label,&n_file_desc,
			&n_data_label,&n_data_desc);

  if(istat == FAIL) {
    printf("error getting file information.\n");
    ANend(file_id);
    return FAIL;
  }

  for (i = 0; i < n_file_label; i++) {

    ann_id     = ANselect(an_id,i,AN_FILE_LABEL);
    if(ann_id == FAIL) {
      printf("error in obtaining annotation id. \n");
      ANend(file_id);
      return FAIL;
    }
      
    ann_length = ANannlen(ann_id);
    if(ann_length == FAIL) {
      printf("error in obtaining annotation length. \n");
      ANend(file_id);
      ANendaccess(ann_id);
      return FAIL;
    }

    ann_buf    = malloc((size_t)ann_length + 1);
    if(ann_buf == NULL) {
      printf("error in allocating memory. \n");
      return FAIL;
    }
    h4toh5_ZeroMemory(ann_buf,(ann_length+1)*sizeof(char));
    istat      = ANreadann(ann_id,ann_buf,ann_length+1);

    if(istat==FAIL) {
      printf("fail to read file information. \n");
      ANend(file_id);
      ANendaccess(ann_id);
      free(ann_buf);
      return FAIL;
    }
 
    h5_sid     = H5Screate(H5S_SCALAR);

    if (h5_sid < 0) {
       printf("failed to create attribute space for"); 
       printf(" HDF4 FILE ANNOTATION. \n"); 
       ANend(file_id);
       ANendaccess(ann_id);
       free(ann_buf);
       return FAIL;
    }

    if ((sh5str_type = mkstr(ann_length+1,H5T_STR_SPACEPAD))<0) {
       printf("error in making string at FILE lABEL ANNO. \n");
       ANend(file_id);
       ANendaccess(ann_id);
       free(ann_buf);
       return FAIL;
    }

    if(conv_int_str(i,index_str)== FAIL) {
      printf("fail to convert integer into character format.\n");
      ANend(file_id);
      ANendaccess(ann_id);
      free(ann_buf);
      return FAIL;          
    }

    strcpy(anno_labelname,HDF4_FILE_LABEL);
    strcat(anno_labelname,"_");
    strcat(anno_labelname,index_str);


    h5_aid = H5Acreate(h5group,anno_labelname,sh5str_type,
		       h5_sid,H5P_DEFAULT);

    if (h5_aid <0) {
       printf("failed to obtain attribute id for"); 
       printf(" File annotation. \n");
       ANend(file_id);
       ANendaccess(ann_id);
       free(ann_buf);
       return FAIL;
    }

    ret   = H5Awrite(h5_aid,sh5str_type,(void *)ann_buf);

    if (ret <0) {
       printf("failed to obtain attribute.\n ");
       ANend(file_id);
       ANendaccess(ann_id);
       free(ann_buf);
       return FAIL;
    }

    ret   = H5Sclose(h5_sid);
    ret   = H5Aclose(h5_aid);
    free(ann_buf);
    ANendaccess(ann_id);

  }

  for (i = 0; i < n_file_desc; i++) {

    ann_id     = ANselect(an_id,i,AN_FILE_DESC);
    if(ann_id == FAIL) {
      printf("error in obtaining annotation id. \n");
      ANend(an_id);
      return FAIL;
    }

    ann_length = ANannlen(ann_id);
    
    if(ann_length == FAIL) {
      printf("error in obtaining annotation length. \n");
      ANend(an_id);
      ANendaccess(ann_id);
      return FAIL;
    }

    ann_buf    = malloc((size_t)ann_length+1);
    if(ann_buf == NULL) {
      printf("error in allocating memory. \n");
      ANend(an_id);
      ANendaccess(ann_id);
      return FAIL;
    }
    h4toh5_ZeroMemory(ann_buf,ann_length+1);

    istat      = ANreadann(ann_id,ann_buf,ann_length+1);

    if(istat == FAIL) {
      printf("error reading file information. \n");
      ANend(an_id);
      ANendaccess(ann_id);
      free(ann_buf);
      return FAIL;
    }

    if ((sh5str1_type = mkstr(ann_length+1,H5T_STR_SPACEPAD))<0) {
       printf("error in making string at FILE DESC. \n");
       ANend(an_id);
       ANendaccess(ann_id);
       free(ann_buf);
       return FAIL;
    }

    if(conv_int_str(i,index_str)==FAIL) {
      printf("fail to convert integer into character format.\n");
      ANend(an_id);
      ANendaccess(ann_id);
      free(ann_buf);
      return FAIL;
    }

    strcpy(anno_descname,HDF4_FILE_DESC);
    strcat(anno_descname,"_");
    strcat(anno_descname,index_str);
    
    h5_sid     = H5Screate(H5S_SCALAR);

    if (h5_sid < 0) {
       printf("failed to create attribute space for"); 
       printf(" HDF4 FILE ANNOTATION. \n"); 
       ANend(an_id);
       ANendaccess(ann_id);
       free(ann_buf);
       return FAIL;
    }

    h5_aid = H5Acreate(h5group,anno_descname,sh5str1_type,
		       h5_sid,H5P_DEFAULT);
    
    if (h5_aid <0) {
       
       printf("failed to obtain attribute id for"); 
       printf(" File annotation. \n");
       ANend(an_id);
       ANendaccess(ann_id);
       H5Sclose(h5_sid);
       free(ann_buf);
       return FAIL;
    }

    ret   = H5Awrite(h5_aid,sh5str1_type,(void *)ann_buf);
        
    if (ret <0) {
       printf("failed to obtain attribute.\n ");
       ANend(an_id);
       ANendaccess(ann_id);
       H5Sclose(h5_sid);
       H5Aclose(h5_aid);
       free(ann_buf);
       return FAIL;
    }

    ret   = H5Sclose(h5_sid);
    ret   = H5Aclose(h5_aid);
    free(ann_buf);
    ANendaccess(ann_id);
  }
  ANend(an_id);
  return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	Annoobj_h4_to_h5
 *
 * Purpose:     translate annotation object into attribute of hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: file identifier
		obj_ref: object reference
		obj_tag: object tag
		h5group: hdf5 group
	
  *-------------------------------------------------------------------------
 */
int Annoobj_h4_to_h5(int32 file_id,int32 obj_ref, int32 obj_tag,
		     hid_t h5group){

  int32    an_id;
  int32    ann_id;
  int32    i;
  int32    status;
  int32    ann_length;

  int32    n_file_label =-1;
  int32    n_file_desc  =-1;
  int32    n_data_label =-1;
  int32    n_data_desc  =-1;

  int      num_lab_anno;
  int      num_des_anno;
  int32    istat;
  int32*   des_anno_list=NULL;
  int32*   lab_anno_list=NULL;

  char*    ann_buf;
  char*    ann_obj_name;
  char     ann_labelname[30];
  
  char     index_str[5];

  hid_t    h5_sid;
  hid_t    h5_aid;
  hid_t    sh5str_type; 
  hid_t    ret;

  an_id    = ANstart(file_id);
  if(an_id == FAIL) {
    printf("fail to start annotation interface.\n");
    return FAIL;
  }

  istat    = ANfileinfo(an_id,&n_file_label,&n_file_desc,
			&n_data_label,&n_data_desc);

  if(istat == FAIL ) {
    printf("error getting file information.\n");
    ANend(an_id);
    return FAIL;
  }

  num_lab_anno   = ANnumann(an_id,AN_DATA_LABEL,obj_tag,obj_ref);
  num_des_anno   = ANnumann(an_id,AN_DATA_DESC,obj_tag,obj_ref); 
  
  if (num_lab_anno == FAIL) {
    printf("error getting number of annotation data label.\n");
    ANend(an_id);
    return FAIL;
  }

  if (num_des_anno == FAIL) {
    printf("error getting number of annotation object label.\n");
    ANend(an_id);
    return FAIL;
  }
  
  if(num_lab_anno > 0) {

    for(i=0; i<num_lab_anno;i++) {
      ann_id = ANselect(an_id,i,AN_DATA_LABEL);

      if(ann_id == FAIL) {
	printf("error in obtaining annotation id.\n");
	ANend(an_id);
	return FAIL;
      }

      ann_length = ANannlen(ann_id);
      if(ann_length == FAIL) {
	printf("error in getting annotation length. \n");
	ANendaccess(ann_id);
	ANend(an_id);
	return FAIL;
      }

      ann_buf    = malloc((size_t)ann_length+1);
      if(ann_buf == NULL) {
	printf("error in allocating annotation memory.\n");
	ANendaccess(ann_id);
	ANend(an_id);
	return FAIL;
      }
      h4toh5_ZeroMemory(ann_buf,(ann_length+1)*sizeof(char));
      status = ANreadann(ann_id,ann_buf,ann_length+1);
      if(status == FAIL) {
	printf("error in reading data.\n");
	ANendaccess(ann_id);
	ANend(an_id);
	free(ann_buf);
	return FAIL;
      }

      status = ANendaccess(ann_id);

      h5_sid     = H5Screate(H5S_SCALAR);

      if (h5_sid < 0) {
	printf("failed to create attribute space for"); 
	printf(" HDF4 FILE ANNOTATION. \n");
	ANend(an_id);
	free(lab_anno_list);
	free(ann_buf);
	return FAIL;
      }

      if ((sh5str_type = mkstr(ann_length+1,H5T_STR_SPACEPAD))<0) {
	printf("error in making string at OBJ LABEL. \n");
	ANend(an_id);
	free(lab_anno_list);
	free(ann_buf);
	return FAIL;
      }

      if(conv_int_str(i,index_str)== FAIL) {
	printf("fail to convert annotation index into character format.\n");
	ANend(an_id);
	free(lab_anno_list);
	free(ann_buf);
	return FAIL;
      }

      /* obtain annotation object name. The name is defined based on object tag
     */
      ann_obj_name = trans_tag_name(obj_tag,AN_DATA_LABEL);

      if(ann_obj_name != NULL) 
	strcpy(ann_labelname,ann_obj_name);
    
      strcat(ann_labelname,"_");
      strcat(ann_labelname,index_str);
    
      h5_aid = H5Acreate(h5group,ann_labelname,sh5str_type,
			 h5_sid,H5P_DEFAULT);
    
      if (h5_aid <0) {

	printf("failed to obtain attribute id for"); 
	printf(" file annotation. \n");
	ANend(an_id);
	free(lab_anno_list);
	free(ann_buf);
	free(ann_obj_name);
	return FAIL;
      }

      ret   = H5Awrite(h5_aid,sh5str_type,(void *)ann_buf);
        
      if (ret <0) {
	printf("failed to obtain attribute.\n ");
	ANend(an_id);
	free(lab_anno_list);
	free(ann_buf);
	free(ann_obj_name);
	return FAIL;
      }

      ret   = H5Sclose(h5_sid);
      ret   = H5Aclose(h5_aid);
      free(ann_obj_name);
      free(ann_buf);
    }
  }

  if(num_des_anno > 0) {


    for (i = 0; i< num_des_anno;i++) {
    
      ann_id  = ANselect(an_id,i,AN_DATA_DESC);
      if(ann_id == FAIL) {
	printf("error in obtaining annotation id.\n");
	ANend(an_id);
	return FAIL;
      }
      ann_length = ANannlen(ann_id);
      if(ann_length == FAIL) {
	printf("error in getting annotation length. \n");
	ANendaccess(ann_id);
	ANend(an_id);
	return FAIL;
      }

      ann_buf = malloc((size_t)ann_length+1);

      if(ann_buf == NULL) {
	printf("error in allocating annotation memory.\n");
	ANendaccess(ann_id);
	ANend(an_id);
	return FAIL;
      }
      
      h4toh5_ZeroMemory(ann_buf,(ann_length+1)*sizeof(char));
      ANreadann(ann_id,ann_buf,ann_length+1);

      if ((sh5str_type = mkstr(ann_length+1,H5T_STR_SPACEPAD))<0) {
	printf("error in making string at OBJECT DESC. \n");
	ANend(an_id);
	free(des_anno_list);
	free(ann_buf);
	return FAIL;
      }

      if(conv_int_str(i,index_str)== FAIL) {
	printf("fail to convert annotation index into character format.\n");
	ANend(an_id);
	free(ann_buf);
	free(des_anno_list);
	return FAIL;
      }
      ann_obj_name = trans_tag_name(obj_tag,AN_DATA_DESC);
      if(ann_obj_name == NULL) {
	printf("error in obtaining tag name. \n");
	ANend(an_id);
	free(ann_buf);
	free(des_anno_list);
	return FAIL;
      }

      strcpy(ann_labelname,ann_obj_name);
      strcat(ann_labelname,"_");
      strcat(ann_labelname,index_str);
    
      h5_sid     = H5Screate(H5S_SCALAR);

      if (h5_sid < 0) {
	printf("failed to create attribute space for"); 
	printf(" HDF4 OBJECT ANNOTATION. \n");
	ANend(an_id);
	free(des_anno_list);
	free(ann_buf);
	free(ann_obj_name);
	return FAIL;
      }

      h5_aid = H5Acreate(h5group,ann_labelname,sh5str_type,
			 h5_sid,H5P_DEFAULT);
    
      if (h5_aid <0) {

	ANend(an_id);
	free(ann_buf);
	free(des_anno_list);
	free(ann_obj_name);
	printf("failed to obtain attribute id for ");
	printf("File annotation. \n");
	return FAIL;
      }

      ret   = H5Awrite(h5_aid,sh5str_type,(void *)ann_buf);
        
      if (ret <0) {
	printf("failed to obtain attribute.\n ");
	ANend(an_id);
	free(ann_buf);
	free(des_anno_list);
	free(ann_obj_name);
	return FAIL;
      }
      ret   = H5Sclose(h5_sid);
      ret   = H5Aclose(h5_aid);
      free(ann_obj_name);

      free(ann_buf);
    }

  }
   ANend(an_id);
   return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:	trans_tag_name
 *
 * Purpose:     in annotation routine,
                translate annotation object tag into corresponding HDF5 object
		name.
	   
 *              
 * Return:	NULL  if failed, HDF5 object name if successful.
 *
 * In :	        
		obj_tag: hdf4 object tag
		annot_type: hdf4 annotation type
	
  *-------------------------------------------------------------------------
 */
char* trans_tag_name(int32 obj_tag,ann_type annot_type){

  char* obj_name;

  obj_name = malloc(strlen(HDF4_VGROUP_LABEL)+1);

  if(obj_name == NULL) {
    printf("error in obtaining tag name. \n");
    return NULL;
  }

  if (obj_tag == DFTAG_NDG || obj_tag == DFTAG_SDG || obj_tag == DFTAG_SD) {
     
    if(annot_type == AN_DATA_LABEL) 
      strcpy(obj_name,HDF4_SDS_LABEL);
    
    else if(annot_type == AN_DATA_DESC)
      strcpy(obj_name,HDF4_SDS_DESC);
    else
      return NULL;
  }

  else if(obj_tag == DFTAG_RIG || obj_tag == DFTAG_RI || obj_tag == DFTAG_RI8)
 {
    if(annot_type == AN_DATA_LABEL) 
      strcpy(obj_name,HDF4_IMAGE_LABEL);
    else if(annot_type == AN_DATA_DESC)
      strcpy(obj_name,HDF4_IMAGE_DESC);
    else
      return NULL;
  }

  else if(obj_tag == DFTAG_VG) {
    if(annot_type == AN_DATA_LABEL) 
      strcpy(obj_name,HDF4_VGROUP_LABEL);
    else if(annot_type == AN_DATA_DESC)
      strcpy(obj_name,HDF4_VGROUP_DESC);
    else
      return NULL;
  }
  
  else if(obj_tag == DFTAG_VS || obj_tag == DFTAG_VH) {
    if(annot_type == AN_DATA_LABEL) 
      strcpy(obj_name,HDF4_VDATA_LABEL);
    else if(annot_type == AN_DATA_DESC)
      strcpy(obj_name,HDF4_VDATA_DESC);
    else
      return NULL;
  }

  else if(obj_tag == DFTAG_LUT) {
    if(annot_type == AN_DATA_LABEL) 
      strcpy(obj_name,HDF4_PAL_LABEL);
    else if(annot_type == AN_DATA_DESC)
      strcpy(obj_name,HDF4_PAL_DESC);
    else
      return NULL;
  }
  return obj_name;
}











