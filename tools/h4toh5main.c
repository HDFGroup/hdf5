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

This file describes the main driver of hdf to hdf5 converter. It checks
the inputting parameters, initializes the global tables, sets up the root level
hdf5 structure and also check the special case fof vgroup loops at HDF file.


Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/


#include "h4toh5main.h"

int32 estnum_vg;
int32 estnum_vd;
int32 num_sds;
int32 num_images;
int   num_objects;
int32 num_glsdsattrs;
int32 num_glgrattrs;
struct table* sds_hashtab;
struct table* gr_hashtab;
struct table* vg_hashtab;
struct table* vd_hashtab;
struct table* pal_hashtab;
struct name_table* name_hashtab;
struct name_table* dim_hashtab;

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:     driver routine to handle all objects of hdf4 file.
 *             
 * Return:	FAIL if failed, SUCCEED if successful.
 
   Modfication: 
 *-------------------------------------------------------------------------
 */	



int main(int argc, char ** argv) {

        char *h5_filename=NULL;
	char *h4_filename=NULL;
	char *h5_extension;
	int status = 0;

	argc--;
	argv++;

	if (argc == 0) {
		fprintf(stderr,"\nError: Invalid Arguments\n");
		PrintOptions_h4toh5();
		return FAIL;
	}

	/* take care -h (help) option first */
	{   int i;
	    for (i=0; i < argc; i++)
		if ( HDstrcmp(argv[i],"-h") == 0 ) {
			PrintOptions_h4toh5();
			return SUCCEED;
		}
	}


	switch(argc) {

	case 0:

		PrintOptions_h4toh5();
		break;

	case 1:	/* h4toh5 file1 */
		h4_filename = argv[0];
#ifndef WIN32
		if (test_file(h4_filename,O_EXCL,292) != 0 ) { 
		  /* 292 Decimal - 0444 Octal, a+r */
		  printf("the current hdf4 file name is not set properly.\n");
		  status = -1;
		  break;
		}
		if (test_dir(h4_filename) != 0 ) {
		   fprintf(stderr,"%s: Is a directory\n",h4_filename);
		   status = -1;
		   break;
		}
#endif
		/*0. check whether this file is an hdf file. */

		if(!Hishdf(h4_filename)){
		  printf("error: not an hdf file. \n");
		  printf("the file will not be converted. \n");
		  status = -1;
		  break;
		}
		h5_extension = HDstrdup("h5");
		h5_filename = BuildFilename(h4_filename,h5_extension);
		if (h5_filename == NULL) {
		  printf("error in creating hdf5 file name.\n");
		  status = -1;
		  break;
		}
#ifndef WIN32
		if (test_file(h5_filename,O_CREAT|O_EXCL,436) != 0) {
		  /* 436 Decimal - 0664 Octal, ug+rw,o+r */
		  printf("permission of hdf5 file is not set properly.\n");
		  status = -1;
		  break;
		}
#endif
		status = h4toh5(h4_filename, h5_filename);

		if ( status == FAIL ) {
		  printf("error in converting %s into %s\n",h4_filename,h5_filename);
		  break;
		}
		if (h5_filename != NULL) {
		   HDfree(h5_filename);
		}

		break;
	
	case 2:	/* h4toh5 file_in file_out */

		h4_filename = argv[0];
		h5_filename = argv[1];

#ifndef WIN32
		if (test_file(h4_filename,O_EXCL,292) != 0 ) { 
		  /* 292 Decimal - 0444 Octal, a+r */
		  printf("permission of hdf4 file is not set properly.\n");    
		  status = -1;
		  break;
		}

		if (test_dir(h4_filename) != 0 ) {
		   fprintf(stderr,"%s: Is a directory\n",h4_filename);
		   status = -1;
		   break;
		}

#endif
		/*0. check whether this file is a hdf file. */

		if(!Hishdf(h4_filename)){
		  printf("error: not an hdf file. \n");
		  printf("the file will not be converted. \n");
		  status = -1;
		  break;
		}

#ifndef WIN32
		if (test_file(h5_filename,O_CREAT|O_RDWR,436) != 0) { /* 436 Decimal - 0664 Octal, ug+rw,o+r */
		  printf("permission of hdf5 file is not set properly.\n");
		  status = -1;
		  break;
		}

		if (test_dir(h4_filename) != 0 ) {
		  fprintf(stderr,"%s: Is a directory\n",h4_filename);
		  status = -1;
		  break;
		}

#endif
		status = h4toh5(h4_filename, h5_filename);
		if ( status == FAIL ) {
		   printf("error in converting %sinto %s\n",h4_filename,h5_filename);
		  break;
		}
		break;

	default:	
		break;
	}

	return status;

}

/*-------------------------------------------------------------------------
 * Function:	h4toh5
 *
 * Purpose:    This routine checks out arguments sent, makes sure that hdf4 
               file is valid, makes sure filename for hdf5 file is correct,
	       and then call h4toh5().
	     
 *-------------------------------------------------------------------------
 */	   
int h4toh5(char*filename4, char*filename5) {
  
  /* define variables for hdf4. */
  int32  istat ; /* hdf4 library routine return value. */
  int32  file_id;/* file identfier of hdf file.*/
  int32  sd_id;/* sd interface identifer*/
  int32  gr_id;/* gr interface identifer*/
  int    check_glo;

  /* define variables for hdf5. */
  hid_t  file5_id;/* hdf5 file identifier. */
  hid_t  h5_root;/* new hdf5 root group identifier.*/
  
  hid_t  h5_dimg;/* hdf5 dimensional scale group identifier. */
  hid_t  h5_palg;/* hdf5 palette group identifier. */

  /*1. open the current hdf4 file. */

  file_id = Hopen(filename4, DFACC_READ, 0);
  if(file_id == FAIL) {
    printf("error: no such hdf4 files. \n");
    return FAIL;
  }

  /* open sd interface.*/
  sd_id = SDstart(filename4,DFACC_READ);
  if(sd_id == FAIL) {
    printf("error: cannot start SD interface. \n");
    Hclose(file_id);
    return FAIL;
  }

  /* open gr interface.*/
  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("error in obtaining gr id. \n");
    SDend(sd_id);
    Hclose(file_id);
    return FAIL;
  }

  /* open V interface. */
  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("error in starting V interface. \n");
    SDend(sd_id);
    GRend(gr_id);
    Hclose(file_id);
    return FAIL;
  }

  /* 2. obtain number of hdf4 objects(sds,image,vdata,vgroup,palette) 
     in this hdf4 file.  */

  if(get_numof_hdf4obj(filename4,file_id) == FAIL) {
    printf("error in obtaining number of hdf4 objects.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    return FAIL;
  }
  
  /* set up global hash tables for hdf4 objects. */
  if(set_hashtables() == FAIL){
    printf("error in setting hashtables. \n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    return FAIL;
  }

  /* create hdf5 file. */
  file5_id = H5Fcreate(filename5,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT);

  if (file5_id < 0) {
     fprintf(stderr, "unable to create hdf5 file \n");
     SDend(sd_id);
     GRend(gr_id);
     Vend(file_id);
     Hclose(file_id);
     free_allhashmemory();
     return FAIL;
  }

  /* Initialize hdf5 group interface. */
  h5_root = H5Gopen(file5_id,"/");

  if(h5_root < 0) {
    printf("error in opening hdf5 root group. \n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

   /**** build up helper groups(dimensional scale and palette) ****/
  if(set_helpgroups(h5_root,&h5_dimg,&h5_palg)==FAIL) {
    printf("error setting up dimensional scale and palette groups.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }
  
  /* convert global sds attributes into global attributes under root group.*/
  check_glo = 1;

  if(sds_transattrs(sd_id, h5_root,num_glsdsattrs,check_glo)==FAIL) {
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
   }
 

  /* convert global image attributes into global attributes under root group.*/
  check_glo = 1;

  if(gr_tranattrs(gr_id, h5_root,num_glgrattrs,check_glo)==FAIL) {
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
   }

 /* convert all objects in lone vgroups into corresponding hdf5 objects. */  
  if(h4toh5lonevgs(file_id,sd_id,h5_root,h5_dimg,h5_palg)== FAIL) {
    printf("error in translating lone vgroup into hdf5 objects.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    if(num_sds >0) H5Gclose(h5_dimg);
    if(num_images >0) H5Gclose(h5_palg);
    H5Gclose(h5_root);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

/*convert all objects in group rings into corresponding hdf5 objects. */
  if(h4toh5vgrings(file_id,sd_id,h5_root,h5_dimg,h5_palg) == FAIL){
    printf("error in translating vgroup rings into hdf5 objects.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    if(num_sds >0) H5Gclose(h5_dimg);
    if(num_images >0) H5Gclose(h5_palg);
    H5Gclose(h5_root);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

  /*convert all independent lone vdata into corresponding hdf5 datasets with 
    compound data type. */
  if(h4toh5lonevds(file_id,h5_root) == FAIL){
    printf("error in translating lone independent vdata into hdf5 objects.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    if(num_sds >0) H5Gclose(h5_dimg);
    if(num_images >0) H5Gclose(h5_palg);
    H5Gclose(h5_root);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

  /*** convert hdf file annotations into hdf5 attributes under the root.***/
  if(Annofil_h4_to_h5(file_id,h5_root) == FAIL) {
    printf("error in translating file annotations into root attributes.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    if(num_sds >0) H5Gclose(h5_dimg);
    if(num_images >0) H5Gclose(h5_palg);
    H5Gclose(h5_root);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

  /*** deal with untouched sds objects.convert them into hdf5 datasets under root group.***/

  if(h4toh5unvisitedsds(file_id,sd_id,h5_root,h5_dimg) == FAIL) {
    printf("error in converting unvisited sds objects into hdf5 file.\n");  
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    if(num_sds >0) H5Gclose(h5_dimg);
    if(num_images >0) H5Gclose(h5_palg);
    H5Gclose(h5_root);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

  /*** deal with untouched image objects. convert them into hdf5 datasets under root group. ***/

  if(h4toh5unvisitedimages(file_id,h5_root,h5_palg) == FAIL) {
    printf("error in converting unvisited image objects into hdf5 file.\n");
    SDend(sd_id);
    GRend(gr_id);
    Vend(file_id);
    Hclose(file_id);
    if(num_sds >0) H5Gclose(h5_dimg);
    if(num_images >0) H5Gclose(h5_palg);
    H5Gclose(h5_root);
    H5Fclose(file5_id);
    free_allhashmemory();
    return FAIL;
  }

  free_allhashmemory();
  SDend(sd_id);
  GRend(gr_id);
  Vend(file_id);
  Hclose(file_id); 
  if(num_sds >0) H5Gclose(h5_dimg);
  if(num_images >0) H5Gclose(h5_palg);
  H5Gclose(h5_root);
  H5Fclose(file5_id);
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:	get_numof_hdf4obj
 *
 * Purpose:     get number or estimated number of hdf4 objects
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: hdf file identifier
		filename: hdf file name
   Out: 
   Modification: 
 *-------------------------------------------------------------------------
 */	 
int get_numof_hdf4obj(char*filename,int32 file_id) {

  int32  sd_id;/* sd interface identifer*/
  int32  gr_id;/* gr interface identifer*/
  int    num_lonevd;/* number of lone vdata*/
  int    num_lonevg;/* number of lone vgroup.*/
  int32  istat;

  estnum_vg     = 0;
  estnum_vd     = 0;
  num_sds       = 0;
  num_images    = 0;
  num_objects   = 0;

  /* obtain number of sds and number of global sds attribute. */ 

  sd_id = SDstart(filename,DFACC_READ);
  if(sd_id == FAIL) {
    printf("error: cannot start SD interface. \n");
    return FAIL;
  }

  if(SDfileinfo(sd_id,&num_sds,&num_glsdsattrs) == FAIL) {
    printf("error in obtaining SDS information from the file.\n");
    return FAIL;
  }
  
  /* obtain number of images and number of global image attributes.*/

  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("error in obtaining gr id. \n");
    return FAIL;
  }

  if(GRfileinfo(gr_id,&num_images,&num_glgrattrs) == FAIL) {
    printf("error in obtaining GR information from the file. \n");
    return FAIL;
  }

  /* obtain number of lone vgroup and lone vdata. */
  
  istat = Vstart(file_id);
  if (istat == FAIL) { 
     fprintf(stderr, "unable to start hdf4 V interface.\n");
     return FAIL;
  }

  num_lonevd = VSlone(file_id,NULL,0);
  if(num_lonevd == FAIL) {
    printf("error in obtaining lone vdata number. \n");
    return FAIL;
  }

  num_lonevg = Vlone(file_id,NULL,0);
  if(num_lonevg == FAIL) {
    printf("error in obtaining lone vgroup number. \n");
    return FAIL;
  }

  /* intelligent guess of the total number of vgroups,total number of 
     independent vdata. */

  estnum_vg = 6* num_lonevg;
  estnum_vd = 4* num_lonevd;
 
  /* set the size of name hashtable to num_objects. */
  num_objects = estnum_vg + estnum_vd + num_sds + num_images;
  
  return SUCCEED;
}



/*-------------------------------------------------------------------------
 * Function:	set_helpgroups
 *
 * Purpose:     get number or estimated number of hdf4 objects
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                h5root: hdf5 group identifier
		h5dimgptr: h5 dimensional group pointer
		h5palgptr: h5 palette group pointer
   Modification:
 *-------------------------------------------------------------------------
 */	 

int set_helpgroups(hid_t h5root,hid_t* h5dimgptr,hid_t* h5palgptr){

  hid_t  h5_dimg=(-1);/* hdf5 dimensional scale group identifier. */
  hid_t  h5_palg;/* hdf5 palette group identifier. */
  
  /*1. dimensional scale group.*/

  if(num_sds > 0) {
    h5_dimg = H5Gcreate(h5root,HDF4_DIMG,0);
    if (h5_dimg <0) {
      printf("error in creating hdf5 dimensional scale group. \n");
      return FAIL;
    }

    *h5dimgptr = h5_dimg;
  }

  /*2. palette group.*/
  
  if(num_images >0) {
    h5_palg = H5Gcreate(h5root,HDF4_PALG,0);
    if(h5_palg <0) {
      printf("error in creating hdf5 palette group. \n");
      if(h5_dimg>0) H5Gclose(h5_dimg);
      return FAIL;
    }

    *h5palgptr = h5_palg;
  }

  return SUCCEED;

}


/*-------------------------------------------------------------------------
 * Function:	set_hashtables
 *
 * Purpose:     set up hashtables
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
               
   Out:  
   Modification:       
 *-------------------------------------------------------------------------
 */	
int set_hashtables(void) {
 
  if(num_sds > 0) {
    sds_hashtab = malloc(sizeof(struct table)*2*num_sds);
    if(init_tab(2*num_sds,sds_hashtab)== FAIL){
      printf("cannot initialize sds hashing table. \n");
      return FAIL;
    }
  }

  if(num_images > 0) {
    gr_hashtab = malloc(sizeof(struct table)*2*num_images);
    if(init_tab(2*num_images,gr_hashtab) == FAIL){
      printf("cannot initialize image hashing table. \n");
      return FAIL;
    }
  }

  /*hashtable is made to be fixed for dimensional scale and palette.*/

  if(num_sds > 0) {
    dim_hashtab = malloc(sizeof(struct name_table)*DIM_HASHSIZE);
    if(init_nametab(DIM_HASHSIZE,dim_hashtab) == FAIL) {
      printf("can not initialize dimension hashing table.\n");
      return FAIL;
    }
  }

  /* initialize the palette table */
  if(num_images > 0){
    pal_hashtab = malloc(sizeof(struct table)*PAL_HASHSIZE);
    if(init_tab(PAL_HASHSIZE,pal_hashtab) == FAIL) {
      printf("can not initialize palette hashing table.\n");
      return FAIL;
    }
  }

  /* initialize the vgroup table */
  if(estnum_vg > 0) { 
    vg_hashtab = malloc(sizeof(struct table)*estnum_vg);
  }
  else {
    estnum_vg = VG_DEFHASHSIZE;
    vg_hashtab = malloc(sizeof(struct table)*estnum_vg);
  }
  if(init_tab(estnum_vg,vg_hashtab) == FAIL) {
      printf("error in allocating memory for vgroup hashing table.\n");
      return FAIL;
  }

  /* initialize the vdata table.*/
  if(estnum_vd > 0) {
    vd_hashtab = malloc(sizeof(struct table)*estnum_vd);
  }
  else {
    estnum_vd = VD_DEFHASHSIZE;
    vd_hashtab = malloc(sizeof(struct table)*estnum_vd);
  }

  if(init_tab(estnum_vd,vd_hashtab)== FAIL) {
     printf("cannot initialize vdata hashing table.\n");
     return FAIL;
  }
  
  /* The name hashtable is only for dealing with name clashing,
     num_objects is the size of the hash table. */

  if(num_objects != 0){
    name_hashtab = malloc(sizeof(struct name_table)*num_objects);
    if(init_nametab(num_objects,name_hashtab)== FAIL) {
      printf("cannot initialize name hashing table. \n");
      return FAIL;
    }
  }

  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    h4toh5lonevgs
 *
 * Purpose:     Recursively convert hdf4 objects in lone vgroups into
                corresponding hdf5 datasets
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        file_id: hdf file id
                sd_id:   hdf sd interface id
		h5group: hdf5 group id
		h5_dimg: hdf5 dimensional scale group id
		h5_palg: hdf5 palette group id
               
   Out:  
   Modification:
 *-------------------------------------------------------------------------
 */	
int h4toh5lonevgs(int32 file_id,int32 sd_id,hid_t h5group,hid_t h5_dimg,hid_t h5_palg) {
		   
  int32  vgroup_id;
  int    num_lonevg; /* number of lone vgroup.*/
  int32  *ref_array;
  int32  istat;
  char   vgroup_name[VGNAMELENMAX];
  char*  cor_vgroupname;
  char   vgroup_class[VGNAMELENMAX];
  char   refstr[MAXREF_LENGTH];
  int    check_vgroup;
  int    check_tabst;
  int    lone_vg_number;
  char   *h5cgroup_name;

  istat = Vstart(file_id);
  if (istat == FAIL) { 
     fprintf(stderr, "unable to start hdf4 V interface.\n");
     return FAIL;
  }

  num_lonevg = Vlone(file_id,NULL,0);
  
  if (num_lonevg == FAIL) {
    printf("error in obtaining lone vgroup number. \n");
    return FAIL;
  }

  /* obtain object reference array. */

  /* if no lone vgroup, quit from this function. */
  if(num_lonevg == 0) 
    return SUCCEED;

  ref_array = (int32 *)malloc(sizeof(int32) *num_lonevg);

  if(ref_array == NULL) {
     printf("error in allocating memory for ref_array.\n");
     return FAIL;
  }

  num_lonevg = Vlone(file_id,ref_array,num_lonevg);

  /* walk through every lone group in the file */

  for(lone_vg_number = 0; lone_vg_number < num_lonevg;
      lone_vg_number++) {

     vgroup_id = Vattach(file_id,ref_array[lone_vg_number],"r");

     if(vgroup_id ==FAIL) {
       printf("error in attaching lone vgroup.\n");
       free(ref_array);
       return FAIL;
     }
	
     /*obtain group name and class name.*/
     h4toh5_ZeroMemory(vgroup_class,VGNAMELENMAX);
     istat = Vgetclass(vgroup_id,vgroup_class);
     if(istat == FAIL) {
       printf("error in getting vgroup class.\n");
       free(ref_array);
       Vdetach(vgroup_id);
       return FAIL;
     }

     h4toh5_ZeroMemory(vgroup_name,VGNAMELENMAX);
     istat = Vgetname(vgroup_id,vgroup_name);
     if(istat == FAIL ) {
       printf("error in getting vgroup name. \n");
       Vdetach(vgroup_id);
       free(ref_array);
       return FAIL;
     }

     /* check for CDF0.0 and RIG0.0, if yes
        don't go into this group.*/
         
      if(strcmp(vgroup_class,_HDF_CDF)==0) {
	Vdetach(vgroup_id);
	continue;
      }
      if(strcmp(vgroup_class,GR_NAME)==0) {
	Vdetach(vgroup_id);
	continue;
      }

     /* converting integer number into string format. */
     if(conv_int_str(ref_array[lone_vg_number],refstr) == FAIL) {
       printf("ref. is negative, error in converting\n");
       Vdetach(vgroup_id);
       free(ref_array);
       return FAIL;
     }

     /* checking whether vgroup name contains ORI_SLASH, changing into CHA_SLASH.*/
     cor_vgroupname = correct_name(vgroup_name);
     if(cor_vgroupname == NULL) {
       printf("error in generating corrected vgroup name. \n");
       Vdetach(vgroup_id);
       free(ref_array);
       return FAIL;
     }
     
     /* obtaining group name of the converted lone vgroup. In this call,
      we will deal with cases such as name clashing and no available vgroup
     name. */

     h5cgroup_name = get_obj_aboname(cor_vgroupname,refstr,NULL,HDF4_VGROUP);
	
     if(h5cgroup_name == NULL) {
       printf("error in getting group name.\n");
       Vdetach(vgroup_id);
       free(ref_array);
       free(cor_vgroupname);
       return FAIL;
     }
    
     /* free memory of corrected name. */
     free(cor_vgroupname);

     /* updating lookup table for vgroups.*/

     check_vgroup = lookup(ref_array[lone_vg_number],estnum_vg,vg_hashtab);

     if(check_vgroup == 0) { /* adding this vgroup into the list. */

       check_tabst = set_name(ref_array[lone_vg_number],estnum_vg,
			      vg_hashtab,h5cgroup_name);
       if(check_tabst == FAIL) {
	 printf("not enough memory to be allocated for vgroup name. \n");
	 Vdetach(vgroup_id);
	 free(h5cgroup_name);
	 free(ref_array);
	 return FAIL;
       }
     }

     /* this line should never fail, if failed, something is wrong with converter or hdf library. */

     if(check_vgroup == 1){
       fprintf(stderr,"this vgroup should not be touched. \n");
       Vdetach(vgroup_id);
       free(h5cgroup_name);
       free(ref_array);
       return FAIL;
     }
     
     if(Vgroup_h4_to_h5(file_id,vgroup_id,sd_id,h5group,h5_dimg,h5_palg)==FAIL){
       printf("error in translating vgroup into hdf5 objects.\n");
       Vdetach(vgroup_id);
       free(h5cgroup_name);
       free(ref_array);
       return FAIL;
     }
      
     Vdetach(vgroup_id);
     free(h5cgroup_name);
  }
  free(ref_array);
  return SUCCEED;
}



/*-------------------------------------------------------------------------
 * Function:    h4toh5vgrings
 *
 * Purpose:     Recursively convert objects at special hdf4 vgroups
                (vgroup rings) 
                into objects of corresponding hdf5 groups. The strategy here 
		is to arbitrily grab any vgroup in the group ring and put it 
		under hdf5 root.
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        file_id: hdf file id
                sd_id:   hdf sds  id
		h5group: hdf5 group id
		h5_dimg: hdf5 dimensional scale group id
		h5_palg: hdf5 palette group id
               
   Out:   
   Modification:
 *-------------------------------------------------------------------------
 */	

int h4toh5vgrings(int32 file_id,int32 sd_id,hid_t h5group,hid_t h5_dimg,hid_t h5_palg){

  int32  vgroup_id;
  int32  ref_num;
  char   vgroup_name[VGNAMELENMAX];
  char*  cor_vgroupname;
  char   vgroup_class[VGNAMELENMAX];
  char   refstr[MAXREF_LENGTH];
  int    check_vgroup;
  int32  istat;
  char   *h5cgroup_name;

  ref_num = Vgetid(file_id,-1);

  while (ref_num != -1) {
  
  /* if we find a group that is not touched, grab it under root group.*/

    check_vgroup = lookup(ref_num,estnum_vg,vg_hashtab);

    if (check_vgroup == 0){

      vgroup_id = Vattach(file_id,ref_num,"r");
      if(vgroup_id ==FAIL) {
	printf("error in attaching group in a group ring. \n");
	return FAIL;
      }

      h4toh5_ZeroMemory(vgroup_name,VGNAMELENMAX);
      istat = Vgetname(vgroup_id,vgroup_name);
      if(istat ==FAIL) {
	printf("error in obtaining vgroup names. \n");
	Vdetach(vgroup_id);
	return FAIL;
      }

      h4toh5_ZeroMemory(vgroup_class,VGNAMELENMAX);
      if(Vgetclass(vgroup_id,vgroup_class) == FAIL) {
        printf("error in obtaining vgroup class name. \n");
	Vdetach(vgroup_id);
        return FAIL;
      }

       /* do nothing for those predefined attribute.*/

      if(vgroup_class[0] != '\0') {

	 if(strcmp(vgroup_class,_HDF_ATTRIBUTE)==0) {
	   ref_num = Vgetid(file_id,ref_num); 
	   Vdetach(vgroup_id);
	   continue;
	 }

	 if(strcmp(vgroup_class,_HDF_VARIABLE)==0) {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }

	 if(strcmp(vgroup_class,_HDF_DIMENSION)==0) {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }

	 if(strcmp(vgroup_class,_HDF_UDIMENSION)==0) {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }

	 if(strcmp(vgroup_class,_HDF_CDF)==0) {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }

         if(strcmp(vgroup_class,GR_NAME)==0)  {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }

	 if(strcmp(vgroup_class,RI_NAME)==0) {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }
       }

       if(vgroup_name[0] != '\0') {
	 if(strcmp(vgroup_name,GR_NAME)==0) {
	   ref_num = Vgetid(file_id,ref_num);
	   Vdetach(vgroup_id);
	   continue;
	 }
       }

       /* convert reference number into string format. */
       if(conv_int_str(ref_num,refstr) == FAIL) {
	 printf("ref. is negative, error in converting\n");
	 Vdetach(vgroup_id);
	 return FAIL;
       }

        /* checking whether vgroup name contains ORI_SLASH, changing into CHA_SLASH.*/
       cor_vgroupname = correct_name(vgroup_name);
       if(cor_vgroupname == NULL) {
	 printf("error in generating corrected vgroup name. \n");
	 Vdetach(vgroup_id);
	 return FAIL;
       }
       /* obtain the hdf5 group name. */
       h5cgroup_name = get_obj_aboname(cor_vgroupname,refstr,NULL,HDF4_VGROUP);

       if(h5cgroup_name == NULL) {
	 printf("error in getting vgroup name.\n");
	 Vdetach(vgroup_id);
	 free(cor_vgroupname);
	 return FAIL;
       }

       free(cor_vgroupname);
       if(set_name(ref_num,estnum_vg,vg_hashtab,h5cgroup_name)==FAIL) {
	 printf("error in setting h5 group name.\n");
	 Vdetach(vgroup_id);
	 free(h5cgroup_name);
	 return FAIL;
       }

       if(Vgroup_h4_to_h5(file_id,vgroup_id,sd_id,h5group,h5_dimg,h5_palg)
	  ==FAIL){
			  
	printf("error in translating vgroup into hdf5 group\n");
	Vdetach(vgroup_id);
	free(h5cgroup_name);
	return FAIL;
       }
      
      Vdetach(vgroup_id);
      free(h5cgroup_name);
    }
    ref_num = Vgetid(file_id,ref_num); 
  }
  return SUCCEED;

}

 
/*-------------------------------------------------------------------------
 * Function:    h4toh5lonevds
 *
 * Purpose:     convert hdf4 lone vdata into
                the corresponding hdf5 datasets
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        file_id: hdf file id
		h5group: hdf5 group id
               
   Out:   
   Modification:
 *-------------------------------------------------------------------------
 */	
int h4toh5lonevds(int32 file_id, hid_t h5group){

  int32  vdata_id;
  int32  *ref_vdata_array;
  int32   vdata_tag;
  int32   vdata_ref;
  int32  istat;
  char   vdata_name[VGNAMELENMAX];
  char*  cor_vdataname;
  char   vdata_class[VGNAMELENMAX];
  char   refstr[MAXREF_LENGTH];
  int    check_vdata;
  int    lone_vd_number;
  int    num_lonevd;
  char   *h5cvdata_name;

  num_lonevd = VSlone(file_id,NULL,0);
  
  if (num_lonevd == FAIL) {
    printf("error in obtaining lone vgroup number. \n");
    return FAIL;
  }

  if (num_lonevd > 0) {

     ref_vdata_array = (int32 *)malloc(sizeof(int32) *(num_lonevd));

     num_lonevd = VSlone(file_id,ref_vdata_array,num_lonevd);

     if(num_lonevd == FAIL) {
       printf("error in obtaining lone vdata number the second time.\n");
       free(ref_vdata_array);
     }
     /* walk through all lone vdatas. */

     for(lone_vd_number = 0; lone_vd_number < num_lonevd;lone_vd_number++) 
       {
	 vdata_id = VSattach(file_id,ref_vdata_array[lone_vd_number],"r");
	
	 if(vdata_id == FAIL) {
	   printf("error in obtaining vdata id for lone vdata.\n");
	   free(ref_vdata_array);
	 }

	 /* Make sure this vdata is not an attribute of other hdf4 objects.*/

	 if(!VSisattr(vdata_id)) {
	   vdata_ref = VSQueryref(vdata_id);
	   if(vdata_ref == FAIL) {
	     printf("error in getting vdata reference number.\n");
	     free(ref_vdata_array);
	     VSdetach(vdata_id);
	     return FAIL;
	   }

	   vdata_tag = VSQuerytag(vdata_id);
	   if(vdata_tag == FAIL){
	     printf("error in getting vdata tag.\n");
	     free(ref_vdata_array);
	     VSdetach(vdata_id);
	     return FAIL;
	   }
	   
           h4toh5_ZeroMemory(vdata_class,VGNAMELENMAX);
	   istat = VSgetclass(vdata_id,vdata_class);
	   if(istat == FAIL) {
	     printf("error in getting vdata class name.\n");
	     free(ref_vdata_array);
	     VSdetach(vdata_id);
	     return FAIL;
	   }
	
	    h4toh5_ZeroMemory(vdata_name,VGNAMELENMAX);
	   istat = VSQueryname(vdata_id,vdata_name);
	   if(istat == FAIL) {
	     printf("error in getting vdata name. \n");
	     free(ref_vdata_array);
             VSdetach(vdata_id);
	     return FAIL;
	   }

	   /* converting reference number into string format.*/
	   if(conv_int_str(ref_vdata_array[lone_vd_number],refstr)==FAIL) {
	     printf("error in converting int to string.\n");
	     free(ref_vdata_array);
	     VSdetach(vdata_id);
	     return FAIL;
	   }  
	   /* checking whether vdata name contains ORI_SLASH, changing into CHA_SLASH.*/
	   cor_vdataname = correct_name(vdata_name);
	   if(cor_vdataname == NULL) {
	     printf("error in generating corrected vgroup name. \n");
	     VSdetach(vdata_id);
	     free(ref_vdata_array);
	     return FAIL;
	   }
	   /* obtaining hdf5 dataset name that is converted from hdf4 vdata.*/
           h5cvdata_name = get_obj_aboname(cor_vdataname,refstr,NULL,HDF4_VDATA);
	   if(h5cvdata_name == NULL) {
	     printf("error in getting vdata name.\n");
	     free(ref_vdata_array);
	     VSdetach(vdata_id);
	     free(cor_vdataname);
	     return FAIL;
	   }

	   free(cor_vdataname);
	   check_vdata = lookup(ref_vdata_array[lone_vd_number],estnum_vd,
				vd_hashtab);

           /* check_vdata should be 1, if it is 1, either converter or hdf lib has bugs. */
	   if(check_vdata == 1){
	     printf("lone vdata should not be checked before.\n");
	     free(h5cvdata_name);
	     free(ref_vdata_array);
             VSdetach(vdata_id);
	     return FAIL;
	   }

           if(set_name(ref_vdata_array[lone_vd_number],estnum_vd,vd_hashtab,
		       h5cvdata_name)==FAIL) {
	     printf("error in setting lone vdata name. \n");
	     free(ref_vdata_array);
	     free(h5cvdata_name);
	     VSdetach(vdata_id);
	     return FAIL;
	   }

	   if(Vdata_h4_to_h5(file_id,vdata_id,h5group)== FAIL) {
	     printf("error in translating independent vdata into");
	     printf(" hdf5 datasets.\n");
	     free(h5cvdata_name);
	     free(ref_vdata_array);
	     VSdetach(vdata_id);
	     return FAIL;
	   }
           free(h5cvdata_name);
	 }
       
       VSdetach(vdata_id);
       }
     free(ref_vdata_array);
  }
  return SUCCEED;   
}


/*-------------------------------------------------------------------------
 * Function:    h4toh5unvisitedsds
 *
 * Purpose:     convert unvisited sds objects into hdf5 datasets and put these
                datasets under hdf5 root group
                This routine will cover old hdf file that doesn't have vgroups.
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                sd_id:   hdf sds  id
		h5root: hdf5 root id
		h5_dimg: hdf5 dimensional scale group id
               
   Out:   
   Modification:
 *-------------------------------------------------------------------------
 */	


int h4toh5unvisitedsds(int32 file_id,int32 sd_id,hid_t h5root,hid_t h5_dimg) {

  int     i;
  int32   sds_id;/* sd dataset identifer*/
  int32   sds_rank;/* sds dataset dimension rank.*/
  int32   sds_dimsizes[DIM_HASHSIZE];/* array that contains the size of the each dimension in sds dataset.*/ 
  int32   sds_dtype;/*sds dataset datatype.*/
  int32   num_sdsattrs;/* number of sds attributes. */
  char    sds_name[MAX_NC_NAME];/* sds name.*/
  char*   cor_sdsname;
  int32   obj_ref; /* obj reference number assigned to sds and images.*/
  char   refstr[MAXREF_LENGTH];/*object reference number in character string format.*/
  int    check_sds;/* flag to check whether this sds is visited. 1 for visited  and 0 for non-visited.*/
  char   *h5csds_name;/* absolute path name of hdf5 dataset transferred from old sds.*/

  if(sd_id == FAIL) {
    printf("error: cannot start SD interface. \n");
    return FAIL;
  }

  /* check all sds objects. */
  for(i=0;i<num_sds;i++){
       
     sds_id    = SDselect(sd_id,i);

     if (sds_id == FAIL) {
	 printf("error in obtaining sd id.\n");
 	 return FAIL;
     }
       
     /* if this sds is dimensional scale, the converting should be ignored. dimensional scale will be converted separately. */
     if(SDiscoordvar(sds_id)) continue;

     /* obtain sds information. */
     if(SDgetinfo(sds_id,sds_name,&sds_rank,sds_dimsizes,
		  &sds_dtype,&num_sdsattrs)== FAIL) {
       printf("error in obtaining SD info at ");
       printf("the unvisited sds routine.\n");
       SDendaccess(sds_id);
       return FAIL;
     }

     /* obtain object reference number of the current sds dataset.*/
     obj_ref = SDidtoref(sds_id);
     if(obj_ref == FAIL) {
       printf("error in obtaining sds object reference at ");
       printf("the unvisited sds routine.\n");
       SDendaccess(sds_id);
       return FAIL;
     }

     /* convert object reference number into string format. */
     if(conv_int_str(obj_ref,refstr) == FAIL) {
       printf("error in converting integer into string.\n");
       SDendaccess(sds_id);
       return FAIL;
     }

     /* check whether the current sds is visited or not. */
     check_sds = lookup(obj_ref,2*num_sds,sds_hashtab);

     /* if not visited, we will do the convertion. */

     if(check_sds == 0) {
     /* since different hdf sds may hold the same name and it is also 
	legal that sds may not have a name; but for hdf5 dataset, 
	it must hold a name, so we will use get_obj_aboname to assure 
	that each new hdf5 dataset converted from
	sds objects will have a disabiguous name. */

       /* checking whether vgroup name contains ORI_SLASH, changing into CHA_SLASH.*/
       cor_sdsname = correct_name(sds_name);
       if(cor_sdsname == NULL) {
	 printf("error in generating corrected sds name. \n");
	 SDendaccess(sds_id);
	 return FAIL;
       }

       h5csds_name = get_obj_aboname(cor_sdsname,refstr,NULL,HDF4_SDS);
       if(h5csds_name == NULL) {
	  printf("error in obtaining sds name.\n");
          SDendaccess(sds_id);
	  free(cor_sdsname);
	  return FAIL;
       }
       free(cor_sdsname);
       /* put this name into hashtable. */
       if(set_name(obj_ref,2*num_sds,sds_hashtab,h5csds_name)==FAIL) {
	  printf("error in setting object name.\n");
          SDendaccess(sds_id);
	  free(h5csds_name);
	  return FAIL;
       }

       /* do the convertion from sds into hdf5 dataset.*/
       if(Sds_h4_to_h5(file_id,sds_id,h5root,h5_dimg)== FAIL){
	  printf("error in translating sds into hdf5 dataset.\n");
          SDendaccess(sds_id);
	  free(h5csds_name);
	  return FAIL;
       }
       free(h5csds_name);

     }
     SDendaccess(sds_id);

  }
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    h4toh5unvisitedimages
 *
 * Purpose:     convert unvisited images into hdf5 dataset and put it
                under hdf5 root group
                This routine will cover old hdf file.
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id:   hdf file id
		h5_root: hdf5 root id
		h5_palg: hdf5 palette group id
               
   Out:
   Modification:         
 *-------------------------------------------------------------------------
 */	

int h4toh5unvisitedimages(int32 file_id,hid_t h5_root,hid_t h5_palg) {

  int     i;
  int32   istat;
  int32   gr_id;
  int32   ri_id;/*raster image identifer.*/
  char    image_name[MAX_GR_NAME];/* image name.*/
  char*   cor_imagename;
  int     check_image;/* flag to check whether this image is visited. 1 for visited  and 0 for non-visited.*/
  int32   obj_ref; /* obj reference number assigned to sds and images.*/
  char    refstr[MAXREF_LENGTH];/*object reference number in character string format.*/
  char   *h5cimage_name;/* absolute path name of hdf5 dataset transferred from old image.*/

  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("error in obtaining gr id. \n");
    return FAIL;
  }
   
  /* check all images. */
  for (i=0;i<num_images;i++) {
      
      ri_id = GRselect(gr_id,i);
      if(ri_id ==FAIL) {
	printf("error in selecting gr interface.\n");
	return FAIL;
      }
      
      /* obtain information of GR */
      istat = GRgetiminfo(ri_id, image_name, NULL, NULL, NULL, NULL, NULL);
      
      if(istat == FAIL) {
	 printf("error in getting GR images.\n");
	 GRendaccess(ri_id);
	 return FAIL;
      }
      
      /* obtain object reference number and convert it into string format. */
      obj_ref = GRidtoref(ri_id);
      if(obj_ref  == 0) {
	printf("error in obtaining image reference number");
	printf(" at h4toh5unvisitedimages routine.\n");
	GRendaccess(ri_id);
	return FAIL;
      }

      if(conv_int_str(obj_ref,refstr)== FAIL) {
	printf("error in converting object reference number");
	printf(" into string at h4toh5unvisitedimages routine.\n");
	GRendaccess(ri_id);
	return FAIL;
      }

      /* check whether the current image is visited or not. */
      check_image = lookup(obj_ref,2*num_images,gr_hashtab);

      if(check_image == 0) {

	 /* since different hdf image may hold the same name and it is 
	    also legal that an image may not have a name; but for hdf5 
	    dataset, it must hold a name, so we will use get_obj_aboname 
	    to guarrtte that each new hdf5 dataset converted from
	    image objects will have a disabiguous name. */

	 /* checking whether vgroup name contains ORI_SLASH, 
	    changing into CHA_SLASH.*/

	cor_imagename = correct_name(image_name);
	if(cor_imagename == NULL) {
	  printf("error in generating corrected image name. \n");
	  GRendaccess(ri_id);
	  return FAIL;
	}
	h5cimage_name = get_obj_aboname(cor_imagename,refstr,NULL,
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

	/* do the convertion from the image into hdf5 dataset.*/
	if(Image_h4_to_h5(file_id,ri_id,h5_root,h5_palg)== FAIL) {
	   printf("error in transferring image name into hdf5 dataset.\n");
	   GRendaccess(ri_id);
	   free(h5cimage_name);
	   return FAIL;
	}
	free(h5cimage_name);
      }
      GRendaccess(ri_id);
    }
  return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    free_allhashmemory()
 *
 * Purpose:     free memory allocated for all hashtables
 *              
 * Return:      
 *
 * In :	        
               
               
   Out:
   Modification:         
 *-------------------------------------------------------------------------
 */	

void free_allhashmemory(){

  if(estnum_vg != 0) freetable(estnum_vg,vg_hashtab);
  if(estnum_vd != 0) freetable(estnum_vd,vd_hashtab);

  if(num_sds !=0) {
    freetable(2*num_sds,sds_hashtab);
    freenametable(DIM_HASHSIZE,dim_hashtab);
  }

  if(num_images !=0) {
    freetable(2*num_images,gr_hashtab);
    freetable(PAL_HASHSIZE,pal_hashtab);
  }

  if(num_objects !=0) freenametable(num_objects,name_hashtab);

}



/********The following routines are adapted from h5toh4 converter. *******/
/*****************************************************************************

  Routine: test_file
 
  Description: Test a file for read/write - ability.
 
  Input: filename	- Unix filename
 
  Output: function return,  global variable - errno

*****************************************************************************/

int test_file(char *filename,int oflag,mode_t mode)
{
	int	fid;

	errno = 0;

	fid = open(filename, oflag, mode);
	if (fid < 0) {
		perror(filename);
	}
	close(fid);

	return errno;

}


/*****************************************************************************

  Routine: test_dir
 
  Description: Test pathway to determine if it is a directory
 
  Input: path	- pathname given
 
  Output:  function return TRUE/FALSE

*****************************************************************************/

int test_dir(char *path)
{

	struct stat buf;
	struct stat *buf_ptr;
	int idir;

	buf_ptr = &buf;

	idir = stat(path, buf_ptr);
	if (idir < 0) {
		if (errno == 2) {
			return 0;
		} else {
			perror(path);
		}
	}

	return S_ISDIR(buf_ptr->st_mode);
}

/*****************************************************************************

  Routine: BuildFilename()
 
  Description: Build a filename with new extension
 
  Input: filename	- present filename
		 ext		- extension to root of filename
 
  Output: (filename:r).ext

*****************************************************************************/

char *BuildFilename(char *filename, char *ext)
{
 	/* build outgoing filename */

	char *filename_out;
	char *lastper_ptr, *lastdir_ptr;
	int root_len;

	lastper_ptr = strrchr(filename,'.');
	lastdir_ptr = strrchr(filename,'/');

	if ( lastper_ptr <= lastdir_ptr ) { /* no extension */
		root_len = strlen(filename);
	} else {	/* existing extension */
		root_len = (int)(lastper_ptr - filename); 
	}

	filename_out = (char *)HDmalloc(root_len + strlen(ext) + 2);
	filename_out = strncpy(filename_out, filename, (size_t)root_len);
	filename_out[root_len] = '\0';
	filename_out = strcat(filename_out,".");
	filename_out = strcat(filename_out,ext);

	return filename_out;
}


/*****************************************************************************

  Routine: PrintOptions_h4toh5()
 
  Description: This routine prints the acceptable argument formats out to stderr.
           
  Input: None
 
  Output: output to stderr

*****************************************************************************/

void PrintOptions_h4toh5(void)
{
		fprintf(stderr,"\nUsage: ");
		fprintf(stderr,"\n  h4toh5 -h (gives this print-out)\n");
		fprintf(stderr,"  h4toh5 input.hdf output.h5\n");
		fprintf(stderr,"  h4toh5 input.hdf\n");
}





