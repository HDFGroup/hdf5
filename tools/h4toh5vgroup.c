
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

converting an hdf4 vgroup object into a hdf5 group.

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/


#include "h4toh5main.h"

 
/*-------------------------------------------------------------------------
 * Function:	Vgroup_h4_to_h5
 *
 * Purpose:     translate different Vgroup objects: vgroup,vdata,sds,image 
                into hdf5 datasets and recursively call the routine
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: hdf4 file identifier
		vgroup_id: hdf4 vgroup id
		sd_id: sd interface id
		h5_group: hdf5 group id
		h5_dimgroup: hdf5 dimensional scale group id
		h5_palgroup: hdf5 palette group id
   Out:

   Modification:
 *-------------------------------------------------------------------------
 */	


int Vgroup_h4_to_h5(int32 file_id,int32 vgroup_id,int32 sd_id,hid_t h5_group,hid_t h5_dimgroup,hid_t h5_palgroup) 

{

   int32   vgroup_tag;
   int32   vgroup_ref;
   int32   obj_tag;
   int32   obj_ref;
   int32   num_gobjects;
   int     i;

   char    refstr[MAXREF_LENGTH];
   char    vgroup_class[VGNAMELENMAX];
   char    vgroup_name[VGNAMELENMAX];

   char*   h5pgroup_name;

   int     check_vgname;
   hid_t   h5_pgroup;

   /*zeroing out memory for vgroup_class and vgroup_name */
   h4toh5_ZeroMemory(vgroup_class,VGNAMELENMAX);
   h4toh5_ZeroMemory(vgroup_name,VGNAMELENMAX);

   vgroup_tag = VQuerytag(vgroup_id);
   if(vgroup_tag == FAIL) {
     printf("error in obtaining vgroup tag.\n");
     return FAIL;
   }

   vgroup_ref = VQueryref(vgroup_id);
   if(vgroup_ref == FAIL) {
     printf("error in obtaining vgroup reference.\n");
     return FAIL;
   }

   if(Vgetname(vgroup_id,vgroup_name) == FAIL) {
     printf("error in obtaining vgroup name.\n");
     return FAIL;
   }

   if(Vgetclass(vgroup_id,vgroup_class) == FAIL) {
     printf("error in obtaining vgroup class name. \n");
     return FAIL;
   }

   /*** ignore reserved HDF group ***/

   if(vgroup_class != NULL) {
     if(strcmp(vgroup_class,_HDF_ATTRIBUTE)==0) return SUCCEED;
     if(strcmp(vgroup_class,_HDF_VARIABLE)==0) return SUCCEED;
     if(strcmp(vgroup_class,_HDF_DIMENSION)==0) return SUCCEED;
     if(strcmp(vgroup_class,_HDF_UDIMENSION)==0) return SUCCEED;
     if(strcmp(vgroup_class,_HDF_CDF)==0) return SUCCEED;
     if(strcmp(vgroup_class,GR_NAME)==0) return SUCCEED;
     if(strcmp(vgroup_class,RI_NAME)==0) return SUCCEED;
   }
   
   if(vgroup_name != NULL) 
     if(strcmp(vgroup_name,GR_NAME)==0) return SUCCEED;

   h5pgroup_name = get_name(vgroup_ref,estnum_vg,vg_hashtab,&check_vgname);

   if(h5pgroup_name == NULL && check_vgname == 0 ) {
     printf("error,cannot find group\n");
     return FAIL;
   }

   if(h5pgroup_name == NULL && check_vgname ==-1 ) {
     printf("error,group name is not defined.\n");
     return FAIL;
   }

   /* create a hdf5 group under h5_group.*/

   h5_pgroup = H5Gcreate(h5_group,h5pgroup_name,0);

   if(h5_pgroup < 0) {
     printf("error in creating group. \n");
     free(h5pgroup_name);
     return FAIL;
   }

   /* vgroup attributes into corresponding hdf5 group attributes.*/
   if(vg_transattrs(vgroup_id,h5_pgroup)==FAIL) {
     printf("error in translating vgroup attributes into hdf5 group attr.\n");
     H5Gclose(h5_pgroup);
     free(h5pgroup_name);
     return FAIL;
   }
    
   num_gobjects = Vntagrefs(vgroup_id);

   if(num_gobjects == FAIL) {
     printf("error in obtaining number of objects in the vgroup. \n");
     H5Gclose(h5_pgroup);
     free(h5pgroup_name);
     return FAIL;
   }

   if(Annoobj_h4_to_h5(file_id,vgroup_ref,vgroup_tag,h5_pgroup)==FAIL) {
     printf("error in obtaining annotation of the vgroup.\n");
     H5Gclose(h5_pgroup);
     free(h5pgroup_name);
     return FAIL;
   }

   for( i = 0;i<num_gobjects;i++) { 
      
     if(Vgettagref(vgroup_id,i,&obj_tag,&obj_ref)==FAIL) {
       printf("failed to get object tag and ref of the current");
       printf(" object in this vgroup.\n");
       H5Gclose(h5_pgroup);
       free(h5pgroup_name);
       return FAIL;
     }

     if(conv_int_str(obj_ref,refstr)== FAIL) {
       printf("failed to convert object reference number ");
       printf("into string format at vgroup_h4_to_h5 routine.\n");
       H5Gclose(h5_pgroup);
       free(h5pgroup_name);
       return FAIL;
     }

     if (Visvg(vgroup_id,obj_ref)) {

	if(convert_vgroup(file_id,sd_id,obj_ref,h5pgroup_name,h5_pgroup,
			  h5_dimgroup,h5_palgroup)== FAIL) {
	  printf("convert_vgroup routine failed,");
	  printf("cannot convert vgroup into hdf5 group successfully.\n");
	  free(h5pgroup_name);
	  H5Gclose(h5_pgroup);
	  return FAIL;
	}

      }
      /* the object is independent vdata. */
     else if(Visvs(vgroup_id,obj_ref)) {
       if(convert_vdata(file_id,obj_ref,h5pgroup_name,h5_pgroup)==FAIL){
	 printf("fail to convert vdata into hdf5 dataset.\n");
	 free(h5pgroup_name);
	 H5Gclose(h5_pgroup);
	 return FAIL;
       }
     }
     else if(obj_tag == DFTAG_NDG || obj_tag == DFTAG_SDG) {
       if(convert_sds(file_id,sd_id,obj_ref,h5pgroup_name,h5_pgroup,
		      h5_dimgroup)==FAIL){
	 printf("fail to convert sds into hdf5 dataset.\n");
	 H5Gclose(h5_pgroup);
	 free(h5pgroup_name);
	 return FAIL;
       }
     }
    else if(obj_tag == DFTAG_RIG) {
      if(convert_image(file_id,obj_ref,h5pgroup_name,
		       h5_pgroup,h5_palgroup)==FAIL){
	 printf("fail to convert image into hdf5 dataset.\n");
	 H5Gclose(h5_pgroup);
	 free(h5pgroup_name);
	 return FAIL;
      }
    }
   }

   H5Gclose(h5_pgroup);
   free(h5pgroup_name);
   return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	convert_vgroup 
 *
 * Purpose:     subroutine interface for better modularity of vgroup_h4_to_h5
 *              In this routine, 1) h5 vgroup name is obtained;
                                 2) vgroup_h4_to_h5 is called again for 
				 unvisited vgroups
				 3) HardLink is created for visited vgroups

 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: hdf4 file identifier
		sd_id: sd interface id
		obj_ref: object reference number
		h5pgroup_name: h5 group name
		h5_pgroup: hdf5 group id
		h5_dimgroup: hdf5 dimensional scale group id
		h5_palgroup: hdf5 palette group id

 *-------------------------------------------------------------------------
 */	

int convert_vgroup(int32 file_id,int32 sd_id, int32 obj_ref,
		   char* h5pgroup_name,hid_t h5_pgroup,hid_t h5_dimgroup,
		   hid_t h5_palgroup) {
  
  int32   vgroup_cid; 
  int32   istat;
  int     check_vgname;
  char    refstr[MAXREF_LENGTH];
  char    cvgroup_name[VGNAMELENMAX];
  char*   cor_cvgroupname;
  char*   h5cgroup_name;
  char*   h5lgroup_name;
  int     check_vgroup;

  if(conv_int_str(obj_ref,refstr)== FAIL) {
    printf("converting integer into string format.\n");
    return FAIL;
  }

  vgroup_cid = Vattach(file_id,obj_ref,"r");
  if(vgroup_cid == FAIL) {
    printf("error in getting vgroup id.\n");
    return FAIL;
  }

  /* recursively obtain information from the group*/
  /* check whether it is looked up, if yes, create a hard link.*/

  istat = Vgetname(vgroup_cid,cvgroup_name);
  if(istat == FAIL) {
    printf("failed to get the name of vgroup.\n");
    Vdetach(vgroup_cid);
    return FAIL;
  }

  /* look up vg hashtable and see whether this object is touched.*/
  check_vgroup = lookup(obj_ref,estnum_vg,vg_hashtab);
         
  /* if this vgroup has not been touched, convert it into hdf5 group.
	   else create a hard link to the existing group.*/

   cor_cvgroupname = correct_name(cvgroup_name);
    if(cor_cvgroupname == NULL) {
      printf("error in generating corrected vgroup name. \n");
      Vdetach(vgroup_cid);
      return FAIL;
    }
  if(check_vgroup == 0) {

     /* checking whether vgroup name contains ORI_SLASH, changing into CHA_SLASH.*/
   
    h5cgroup_name = get_obj_aboname(cor_cvgroupname,refstr,h5pgroup_name,
				    HDF4_VGROUP);
    if(h5cgroup_name == NULL) {
      printf("error in getting the group name.\n");
      Vdetach(vgroup_cid);
      free(cor_cvgroupname);
      return FAIL;
    }

    free(cor_cvgroupname);
    if(set_name(obj_ref,estnum_vg,vg_hashtab,h5cgroup_name)== FAIL) {
      printf("error in setting  group name.\n");
      Vdetach(vgroup_cid);
      free(h5cgroup_name);
      return FAIL;
    }
    if(Vgroup_h4_to_h5(file_id,vgroup_cid,sd_id,h5_pgroup,
		       h5_dimgroup,h5_palgroup)== FAIL) {
      printf("error in transferring vgroup into hdf5 group.\n");
      Vdetach(vgroup_cid);
      free(h5cgroup_name);
      return FAIL;
    }
    free(h5cgroup_name);

  }

  else {

    h5cgroup_name = get_name(obj_ref,estnum_vg,vg_hashtab,&check_vgname);
    if(h5cgroup_name == NULL && check_vgname ==0 ) {
      printf("error,cannot find group\n");
      Vdetach(vgroup_cid);
      return FAIL;
    }

    if(h5cgroup_name == NULL && check_vgname == -1 ) {
      printf("error,group name is not defined.\n");
      Vdetach(vgroup_cid);
      return FAIL;
    }

    /* create HL  */

 
    h5lgroup_name = get_obj_aboname(cor_cvgroupname,refstr,h5pgroup_name,
				    HDF4_VGROUP);
    if(h5lgroup_name == NULL) {
      printf("failed to obtain group name.\n");
      Vdetach(vgroup_cid);
      free(h5cgroup_name);
      free(cor_cvgroupname);
      return FAIL;
    }
    free(cor_cvgroupname);
    if(H5Glink(h5_pgroup,H5G_LINK_HARD,h5cgroup_name,h5lgroup_name)<0) {
      printf("cannot make hard link for two groups.\n");
      Vdetach(vgroup_cid);
      free(h5cgroup_name);
      free(h5lgroup_name);
      return FAIL;
    }
    free(h5cgroup_name);
    free(h5lgroup_name);
  }

  Vdetach(vgroup_cid);
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	convert_vdata 
 *
 * Purpose:     subroutine interface for better modularity of vgroup_h4_to_h5
 *              In this routine, 1) h5 vdata name is obtained;
                                 2) vdata_h4_to_h5 is called for unvisited
				 vdatas
				 3) HardLink is created for visited vdatas

 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: hdf4 file identifier
		obj_ref: object reference number
		h5pgroup_name: h5 group name
		h5_pgroup: hdf5 group id

 *-------------------------------------------------------------------------
 */	
  
int convert_vdata(int32 file_id,int32 obj_ref,char * h5pgroup_name,
		   hid_t h5_pgroup) {
  
  int32 vdata_id;
  int   check_vdata;
  int   check_vdname;
  int32 istat;
  char  refstr[MAXREF_LENGTH];
  char  cvdata_name[VGNAMELENMAX];
  char* cor_cvdataname;
  char* h5cvdata_name;
  char* h5lvdata_name;
  
  vdata_id = VSattach(file_id,obj_ref,"r");
  if(vdata_id == FAIL) {
    printf("error in attaching vdata. \n");
    return FAIL;
  }

  if(conv_int_str(obj_ref,refstr)== FAIL) {
    printf("converting integer into string format.\n");
    VSdetach(vdata_id);
    return FAIL;
  }

  istat = VSisattr(vdata_id);
  if (istat == FAIL) {
    printf("error in checking vdata attribute. \n");
    VSdetach(vdata_id);
    return FAIL;
  }

  if(istat); /*ignore, dependent vdata(attributes, etc.)can be retrieved later.*/

  else { /* independent vdata, read in */

    check_vdata = lookup(obj_ref,estnum_vd,vd_hashtab);

    if(check_vdata < 0) {
      printf("failed to look up the object.\n");
      VSdetach(vdata_id);
      return FAIL;
    }

    if(VSQueryname(vdata_id,cvdata_name)==FAIL) {
      printf("error in querying name. \n");
      VSdetach(vdata_id);
      return FAIL;
    }

    cor_cvdataname = correct_name(cvdata_name);
    if(cor_cvdataname == NULL) {
      printf("error in generating corrected vdata name. \n");
      VSdetach(vdata_id);
      return FAIL;
    }
    if(check_vdata ==0) {
      h5cvdata_name = get_obj_aboname(cor_cvdataname,refstr,h5pgroup_name,
				      HDF4_VDATA);
      if(h5cvdata_name == NULL) {
	printf("cannot obtain the converted hdf5 dataset name from vdata.\n");
	VSdetach(vdata_id);
	free(cor_cvdataname);
	return FAIL;
      }
      free(cor_cvdataname);
      if(set_name(obj_ref,estnum_vd,vd_hashtab,h5cvdata_name)== FAIL){
	printf("failed to obtain vdata name.\n");
	VSdetach(vdata_id);
	free(h5cvdata_name);
	return FAIL;
      }

      if(Vdata_h4_to_h5(file_id,vdata_id,h5_pgroup)==FAIL){
	printf("failed to transfer vdata into hdf5 dataset.\n");
	VSdetach(vdata_id);
	free(h5cvdata_name);
	return FAIL;
      }
      free(h5cvdata_name);
    }

    else {

      h5cvdata_name = get_name(obj_ref,estnum_vd,vd_hashtab,
			       &check_vdname);

      if(h5cvdata_name == NULL && check_vdname ==0 ){
	printf("error,cannot find vdata\n");
	VSdetach(vdata_id);
	return FAIL;
      }
      if(h5cvdata_name == NULL && check_vdname ==-1 ){
	printf("error,vdata name is not defined.\n");
	VSdetach(vdata_id);
	return FAIL;
      }
      /*create HL, 
	for the time being, we will use absolute path. */

      h5lvdata_name = get_obj_aboname(cor_cvdataname,refstr,h5pgroup_name,
				      HDF4_VDATA);
      if(h5lvdata_name == NULL) {
	printf("error in obtaining vdata name.\n");
	VSdetach(vdata_id);
	free(h5cvdata_name);
	free(cor_cvdataname);
	return FAIL;
      }
      free(cor_cvdataname);
      if(H5Glink(h5_pgroup,H5G_LINK_HARD,h5cvdata_name,h5lvdata_name)){
	printf("error in creating hardlink for hdf5 dataset");
	printf(" converted from vdata.\n");
	VSdetach(vdata_id);
	free(h5cvdata_name);
	free(h5lvdata_name);
	return FAIL;
      }
      free(h5cvdata_name);
      free(h5lvdata_name);
    }
    VSdetach(vdata_id);
  }

  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	convert_sds
 *
 * Purpose:     subroutine interface for better modularity of vgroup_h4_to_h5
 *              In this routine, 1) h5 sds name is obtained;
                                 2) sds_h4_to_h5 is called for unvisited
				 sds objects
				 3) HardLink is created for visited sds

 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                sd_id: hdf4 sds identifier
		obj_ref: object reference number
		h5_dimgroup: h5 dimensional scale group id
		h5_pgroup: hdf5 group id

 *-------------------------------------------------------------------------
 */	
int convert_sds(int32 file_id,int32 sd_id,int32 obj_ref,char * h5pgroup_name,
		   hid_t h5_pgroup,hid_t h5_dimgroup) {

  int32 sd_index;
  int32 sds_id;
  int32 sds_rank;
  int32 sds_dimsizes[DIM_HASHSIZE];
  int32 sds_dtype;
  int32 num_sdsattrs;
  char  sds_name[MAX_NC_NAME];
  char* cor_sdsname;
  int   check_sds;
  int   check_sdsname;
  char  refstr[MAXREF_LENGTH];
  char* h5csds_name;
  char* h5lsds_name;

  sd_index = SDreftoindex(sd_id,obj_ref);
  if(sd_index == FAIL){
    printf("error in obtaining reference number of sds.\n");
    return FAIL;
  }

  if(conv_int_str(obj_ref,refstr)== FAIL) {
    printf("error in converting reference number into string type.\n");
    return FAIL;
  }

  sds_id = SDselect(sd_id,sd_index);

  if(sds_id == FAIL){
    printf("error in obtaining sd id.\n");
    return FAIL;
  }

  if(SDgetinfo(sds_id,sds_name,&sds_rank,sds_dimsizes,
	       &sds_dtype,&num_sdsattrs)==FAIL) {
    printf("error in obtaining SD info.\n");
    SDendaccess(sds_id);
    return FAIL;
  }

  /* check whether this sds is touched. */
  check_sds = lookup(obj_ref,2*num_sds,sds_hashtab);

  cor_sdsname = correct_name(sds_name);
  if(cor_sdsname == NULL) {
    printf("error in generating corrected sds name. \n");
    SDendaccess(sds_id);
    return FAIL;
  }

  if(check_sds == 0) {

    /* obtain the absolute name of sds object, deal with the name clashing by
       looking up things in the "name hashing table".*/
       
    h5csds_name = get_obj_aboname(cor_sdsname,refstr,h5pgroup_name,HDF4_SDS);
    if(h5csds_name == NULL) {
      printf("error in obtaining sds name.\n");
      SDendaccess(sds_id);
      free(cor_sdsname);
      return FAIL;
    }
    free(cor_sdsname);

    /* put the absolute path of sds into "hashing table".*/
    if(set_name(obj_ref,2*num_sds,sds_hashtab,h5csds_name)==FAIL) {
      printf("error in setting object name.\n");
      SDendaccess(sds_id);
      free(h5csds_name);
      return FAIL;
    }
    /* convert the sds object into hdf5 dataset.*/
    if(Sds_h4_to_h5(file_id,sds_id,h5_pgroup,h5_dimgroup)==FAIL){
      printf("error in translating sds into hdf5 dataset.\n");
      SDendaccess(sds_id);
      free(h5csds_name);
      return FAIL;
    }
    free(h5csds_name);
  }
  else {
    /* if the object has been touched, create a hard link instead.*/
    h5csds_name = get_name(obj_ref,2*num_sds,sds_hashtab,&check_sdsname);
    if(h5csds_name == NULL) {
      printf("error in getting sds name \n");
      SDendaccess(sds_id);
      return FAIL;
    }
    /*... ADD in the code. create HL,
      for the time being, we will use absolute path. */
    h5lsds_name = get_obj_aboname(cor_sdsname,refstr,h5pgroup_name,
				  HDF4_SDS);
    if(h5lsds_name == NULL) {
      printf("error in getting sds link name.\n");
      SDendaccess(sds_id);
      free(h5csds_name);
      free(cor_sdsname);
      return FAIL;
    }
    free(cor_sdsname);
    if(H5Glink(h5_pgroup,H5G_LINK_HARD,h5csds_name,h5lsds_name) <0) {
      printf("error in getting hard link \n");
      SDendaccess(sds_id);
      free(h5csds_name);
      free(h5lsds_name);
      return FAIL;
    }

    free(h5csds_name);
    free(h5lsds_name);
  }
  SDendaccess(sds_id);
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	convert_image 
 *
 * Purpose:     subroutine interface for better modularity of vgroup_h4_to_h5
 *              In this routine, 1) h5 vdata name is obtained;
                                 2) image_h4_to_h5 is called for unvisited
				 images
				 3) Hard Link is created for visited images

 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: hdf4 file identifier
		obj_ref: object reference number
		h5pgroup_name: h5 group name
		h5_pgroup: hdf5 group id
		h5_palgroup: hdf5 palette group id

 *-------------------------------------------------------------------------
 */	
int convert_image(int32 file_id,int32 obj_ref,char * h5pgroup_name,
		   hid_t h5_pgroup,hid_t h5_palgroup) {

  int32   gr_id;
  int32   gr_index;
  int32   ri_id;
  int32   istat;
  char*   h5cimage_name;
  char*   h5limage_name;
  char    refstr[MAXREF_LENGTH];
  char    image_name[MAX_GR_NAME];
  char*   cor_imagename;

  int     check_imagename;
  int     check_image;

  
  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("error in obtaining gr id. \n");
    return FAIL;
  }

  if(conv_int_str(obj_ref,refstr)== FAIL) {
    printf("converting integer into string format.\n");
    return FAIL;
  }

  gr_index= GRreftoindex(gr_id,obj_ref);
  if(gr_index == FAIL) {
    printf("error in getting gr index.\n");
    return FAIL;
  }

  ri_id = GRselect(gr_id,gr_index);
  if(ri_id == FAIL) {
    printf("error in selecting gr interface.\n");
    return FAIL;
  }

  istat = GRgetiminfo(ri_id, image_name, NULL, NULL, NULL, NULL, NULL);
        
  if(istat == FAIL) {
    GRendaccess(ri_id);
    printf("error in getting GR images.\n");
    return FAIL;
  }
  
   /* checking whether image name contains ORI_SLASH, 
      changing into CHA_SLASH.*/

   cor_imagename = correct_name(image_name);
   if(cor_imagename == NULL) {
     printf("error in generating corrected image name. \n");
     GRendaccess(ri_id);
     return FAIL;
   }

  /* check whether this image is touched. */
  check_image = lookup(obj_ref,2*num_images,gr_hashtab);

  if(check_image == 0) {

    /* obtain the absolute name of image object, deal with the name clashing by
       looking up things in the "name hashing table".*/

    h5cimage_name = get_obj_aboname(cor_imagename,refstr,h5pgroup_name,
				    HDF4_IMAGE);
    if(h5cimage_name == NULL) {
      printf("error in getting image name.\n");
      GRendaccess(ri_id);
      free(cor_imagename);
      return FAIL;
    }
    free(cor_imagename);

    if(set_name(obj_ref,2*num_images,gr_hashtab,h5cimage_name)==FAIL) {
      printf("error setting image name.\n");
      GRendaccess(ri_id);
      free(h5cimage_name);
      return FAIL;
    }
    if(Image_h4_to_h5(file_id,ri_id,h5_pgroup,h5_palgroup)==FAIL) {
      printf("error in transferring image name into hdf5 dataset.\n");
      GRendaccess(ri_id);
      free(h5cimage_name);
      return FAIL;
    }
    free(h5cimage_name);
  }

  else{

    /*if the object is visited, create HL. */
        
    h5cimage_name = get_name(obj_ref,2*num_images,gr_hashtab,
			     &check_imagename);

    if(h5cimage_name == NULL) {
      printf("error in getting image name into hdf5 dataset.\n");
      GRendaccess(ri_id);
      free(h5cimage_name);
      return FAIL;
    }
    h5limage_name = get_obj_aboname(cor_imagename,refstr,h5pgroup_name,
				    HDF4_IMAGE);

    if(h5limage_name == NULL) {
      printf("error in getting link image name into hdf5 dataset.\n");
      GRendaccess(ri_id);
      free(h5cimage_name);
      free(cor_imagename);
      return FAIL;
    }
    free(cor_imagename);

    if(H5Glink(h5_pgroup,H5G_LINK_HARD,h5cimage_name,h5limage_name)<0){
      printf("error in linking two groups.\n");
      GRendaccess(ri_id);
      free(h5cimage_name);
      free(h5limage_name);
      return FAIL;
    }
    free(h5cimage_name);
    free(h5limage_name);
  }  
 
    GRendaccess(ri_id);
    /* this is for efficient reason, we will comment out GRend.
       GRend(gr_id);*/

    return SUCCEED;
}





