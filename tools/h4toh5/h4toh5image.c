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

converting an hdf4 image object into an hdf5 dataset, for three component image, this object will be converted into an hdf5 dataset with compound data type.

Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/

#include "h4toh5main.h"

/*-------------------------------------------------------------------------
 * Function:	Image_h4_to_h5
 *
 * Purpose:     translate Image object into hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                ri_id: RI identifier
		h5_group: hdf5 group id
		h5_palgroup: hdf5 palette group id

 *-------------------------------------------------------------------------
 */	

int Image_h4_to_h5(int32 file_id,int32 ri_id,hid_t h5_group,hid_t h5_palgroup,int h4_attr) {

  int32    istat;
  int32    ngrattrs;
  int32    ncomp;
  int      check_gloattr;
  int32    start[2];
  int32    edges[2];
  int32    dimsizes[2];
  uint16   gr_ref;
  int32    image_dtype;

  int      check_imagename;
  int      i;
  char     image_name[MAX_GR_NAME];
  char     grlabel[MAX_GR_NAME];
  char     image_class[MAX_GR_NAME];
  char*    h5cimage_name;
  void*    image_data;
  HDF_CHUNK_DEF c_def_out;
  int32    chunk_dims[2];
  int32    chunk_dims24[3];
  int32    c_flags;
  int32    interlace_mode;

  /* for checking compression */

  sp_info_block_t info_block;
  int16           special_code;
  int32           access_id;
  uint16          ri_ref;
  int             gzip_level;
  uint16          temp_tag;
  uint16*          ptag_out;

  /* define varibles for hdf5. */

  hid_t    h5ty_id;
  hid_t    h5memtype;

  hid_t    h5_ctype;
  hid_t    h5_cmemtype;

  hid_t    h5d_sid;
  hid_t    h5dset;

  size_t   h4size;
  size_t   h4memsize;
  hsize_t   fielddim[1];
  hsize_t  h5dims[2];
  hsize_t  h5dims24[3];
  hsize_t bufsize;
  herr_t   ret;
  hid_t    create_plist;
  hid_t    write_plist;


  temp_tag = DFTAG_NULL;
  ptag_out = &temp_tag;

  /* zeroing out memory.*/

  h4toh5_ZeroMemory(image_name,MAX_GR_NAME);
  h4toh5_ZeroMemory(image_class,MAX_GR_NAME);
  h4toh5_ZeroMemory(grlabel,MAX_GR_NAME);

  /* Obtain information of the image.*/  

  if(GRgetchunkinfo(ri_id,&c_def_out,&c_flags)==FAIL){
    printf("error in getting chunking information. \n");
    return FAIL;
  }

  istat = GRgetiminfo(ri_id, image_name, &ncomp, &image_dtype, 
		      NULL, dimsizes, &ngrattrs);

  if(istat == FAIL) {
    printf("Cannot obtain GR info. at Image routine.\n");
    return FAIL;
  }

  /* data type transferring from hdf4 to hdf5. */
  if(h4type_to_h5type(image_dtype,&h5memtype,&h4memsize,
		      &h4size,&h5ty_id)== FAIL) {
    printf("failed to translate image datatype. \n");
    return FAIL;
  }
 
  /* check whether the datatype is string. */
  if (h5ty_id == H5T_STRING) {
    /* rechange string datatype into numerical datatype.*/

    if(h5string_to_int(image_dtype,&h5memtype,h4memsize,
		       &h5ty_id)== FAIL) {
      printf("error in translating H5T_STRING to int.\n");
      return FAIL;
    }
  }     

  start[0]   = 0;
  start[1]   = 0;
  edges[0]   = dimsizes[0];
  edges[1]   = dimsizes[1];
  
  image_data = malloc(h4memsize*dimsizes[0]*dimsizes[1]*ncomp);
  
  if(image_data == NULL) {
    printf("error in allocating memory for image data. \n");
    return FAIL;
  }

  istat = GRreadimage(ri_id, start, NULL, edges, (VOIDP)image_data);

  if (istat == FAIL) {
    printf("error in reading images.\n");
    free(image_data);
    return FAIL;
  }

  /* change the order of image dimension:
     due to the difference of hdf4 image specification and
     hdf5 image specification. Here we should separate 8-bit from
     24-bit according to H4TOH5 mapping specification.*/
  
  if(ncomp == 1) {
    h5dims[0] = edges[1]-start[1];
    h5dims[1] = edges[0]-start[0];
  }
  
  else {
    if(interlace_mode == MFGR_INTERLACE_PIXEL){
      h5dims24[0] = edges[1]-start[1];
      h5dims24[1] = edges[0]-start[0];
      h5dims24[2] = 3;
    }
    /* currently scan-line is not supported.
    else if (interlace_mode == MFGR_INTERLACE_LINE){
      h5dims24[0] = 3;
      h5dims24[1] = edges[1]-start[1];
      h5dims24[2] = edges[0]-start[0];
    }
    */
    else if (interlace_mode == MFGR_INTERLACE_COMPONENT){
      h5dims24[0] = 3;
      h5dims24[1] = edges[1]-start[1];
      h5dims24[2] = edges[0]-start[0];
    }

    else {/* treat as pixel */
      h5dims24[0] = edges[1]-start[1];
      h5dims24[1] = edges[0]-start[0];
      h5dims24[2] = 3;
    }
  }

  gr_ref = GRidtoref(ri_id);
  if(gr_ref == 0) {
    printf("error in obtaining gr reference number. \n");
    free(image_data);
    return FAIL;
  }
  
  /* obtaining absolute path of image name.*/
  check_imagename = -10;
  h5cimage_name = get_name(gr_ref,2*num_images,gr_hashtab,&check_imagename);

  if (h5cimage_name == NULL && check_imagename == 0 ) {
     printf("error,cannot find image name.\n");
     free(image_data);
     return FAIL;
  }

  if (h5cimage_name == NULL && check_imagename == -1) {
     printf("error,image name is not defined.\n");
     free(image_data);
     return FAIL;
  }

  if (h5cimage_name == NULL && check_imagename == -2) {
     printf("error,not enough memory for get_name. \n");
     free(image_data);
     return FAIL;
  }

  /****  check number of component of the image object,
	 and transfer HDF4 object into HDF5 object. ****/

  if (ncomp <= 0) {
     printf("error in obtaining image component\n");
     free(image_data);
     free(h5cimage_name);
     return FAIL;
  }

  /* create property list. */

  create_plist = H5Pcreate(H5P_DATASET_CREATE);
  /* wait until the compression information can be obtained for image,
     4/28/2001, Kent Yang.*/

  /* the following code deals with compression. Has to use 
     some middle-level APIs. */
  ri_ref = 0;
  /*
   ri_ref = get_RIref(file_id,DFTAG_VG,gr_ref,ptag_out);

  if(ri_ref >0 ){
    if(*ptag_out == DFTAG_RI) printf("okay\n");
    access_id = Hstartread(file_id,*ptag_out,ri_ref);
  }
  */
  if(ri_ref == 0) 
    access_id = FAIL;
  

  /*  if(access_id == FAIL){
    printf("cannot find RI tag,the compression mode will not be checked");
    printf(" when RIG is not chunked.\n");
    }*/
  
  if(access_id != FAIL) {
    istat = Hinquire(access_id,NULL,NULL,NULL,NULL,NULL,NULL,NULL,&special_code);
    if(istat == FAIL) {
      printf("failed to inquire information \n ");
      free(image_data);
       free(h5cimage_name);
      H5Pclose(create_plist);
      return FAIL;	
    }

    if(special_code >0){
      
      if(HDget_special_info(access_id,&info_block)==FAIL){
	printf("fail to get special info.\n");
	free(image_data);
	free(h5cimage_name);
	H5Pclose(create_plist);
	return FAIL;	
      }

      if(info_block.key == SPECIAL_COMP) {
   
	if(c_flags == HDF_NONE){
	  /* 1. the current HDF5 will not handle compression case itself, 
	     in order that the converted HDF5 is compressed, we have to
	     provide a chunking size. currently it is set to h5dim[i].*/

	  if(ncomp == 1) {
	    chunk_dims[0] = (hsize_t)(h5dims[0]);
            chunk_dims[1] =(hsize_t)(h5dims[1]);
	  
	    if(H5Pset_chunk(create_plist, 2, chunk_dims)<0) {
	      printf("failed to set up chunking information for ");
	      printf("property list.\n");
	      free(image_data);
	      free(h5cimage_name);
	      H5Pclose(create_plist);
	      return FAIL;	
	    }
	  }

	  else if(ncomp ==3) {/*24-bit chunk dimension is set to the current
				HDF5 dimension.*/
	    if(interlace_mode == MFGR_INTERLACE_PIXEL){
	      chunk_dims24[0] = edges[1]-start[1]; 
	      chunk_dims24[1] = edges[0]-start[0];
	      chunk_dims24[2] = 3;
	    }
	    /* currently scan-line is not supported.
	       else if (interlace_mode == MFGR_INTERLACE_LINE){
	       chunk_dims24[0] = 3; 
	       chunk_dims24[1] = edges[1]-start[1];
	       chunk_dims24[2] = edges[0]-start[0];
	       }
	    */
	    else if (interlace_mode == MFGR_INTERLACE_COMPONENT){
	      chunk_dims24[1] = edges[1]-start[1]; 
	      chunk_dims24[2] = edges[0]-start[0];
	      chunk_dims24[0] = 3;
	    }

	    else {/* treat as pixel */
	      chunk_dims24[0] = edges[1]-start[1]; 
	      chunk_dims24[1] = edges[0]-start[0];
	      chunk_dims24[2] = 3;
	    }
	    if(H5Pset_chunk(create_plist, 3, chunk_dims24)<0) {
	      printf("failed to set up chunking information for ");
	      printf("property list.\n");
	      free(image_data);
	      free(h5cimage_name);
	      H5Pclose(create_plist);
	      return FAIL;	
	    }
	  }
	  if(H5Pset_deflate(create_plist,GZIP_COMLEVEL)<0){
	    /*    if(H5Pset_deflate(create_plist,2)<0){*/
	    printf("fail to set compression method for HDF5 file.\n"); 
	     free(image_data);
	    free(h5cimage_name);
	    H5Pclose(create_plist);
	    return FAIL;
	  }
	}

      }
    }

  }
  if(c_flags == HDF_CHUNK || c_flags == (HDF_CHUNK | HDF_COMP)
     || c_flags == (HDF_CHUNK | HDF_NBIT)  ){

    if(c_def_out.comp.comp_type == COMP_CODE_RLE || c_def_out.comp.comp_type == COMP_CODE_NBIT || c_def_out.comp.comp_type == COMP_CODE_SKPHUFF || c_def_out.comp.comp_type == COMP_CODE_DEFLATE || c_def_out.comp.comp_type == COMP_CODE_JPEG) {
      if(ncomp ==1) {   
	chunk_dims[0] = c_def_out.chunk_lengths[0]; 
	chunk_dims[1] = c_def_out.chunk_lengths[1];
       
	if(H5Pset_chunk(create_plist, 2, (hsize_t *)chunk_dims)<0) {
	  printf("failed to set up chunking information for ");
	  printf("property list.\n");
	  free(image_data);
	  free(h5cimage_name);
	  H5Pclose(create_plist);
	  return FAIL;	
	}
      }

      else if(ncomp ==3) {
	if(interlace_mode == MFGR_INTERLACE_PIXEL){
      chunk_dims24[0] = c_def_out.chunk_lengths[1]; 
      chunk_dims24[1] = c_def_out.chunk_lengths[0];
      chunk_dims24[2] = 3;
    }
    /* currently scan-line is not supported.
    else if (interlace_mode == MFGR_INTERLACE_LINE){
      chunk_dims24[0] = c_def_out.chunk_lengths[1]; 
      chunk_dims24[2] = c_def_out.chunk_lengths[0];
      chunk_dims24[1] = 3;
    }
    */
    else if (interlace_mode == MFGR_INTERLACE_COMPONENT){
      chunk_dims24[1] = c_def_out.chunk_lengths[1]; 
      chunk_dims24[2] = c_def_out.chunk_lengths[0];
      chunk_dims24[0] = 3;
    }

    else {/* treat as pixel */
      chunk_dims24[0] = c_def_out.chunk_lengths[1]; 
      chunk_dims24[1] = c_def_out.chunk_lengths[0];
      chunk_dims24[2] = 3;
    }
      }
	if(c_def_out.comp.comp_type == COMP_CODE_DEFLATE)
	  gzip_level = c_def_out.comp.cinfo.deflate.level;
	else gzip_level = GZIP_COMLEVEL;
	if(H5Pset_deflate(create_plist,gzip_level)<0){
	  printf("fail to set compression method for HDF5 file.\n"); 
	   free(image_data);
	   free(h5cimage_name);
	   H5Pclose(create_plist);
	}
    }
  }

  /* HDF4 can support various compression methods including simple RLE, NBIT, Skip Huffman, gzip,Jpeg , HDF5 currently only supports gzip compression. 
 By default, we will compress HDF5 dataset by using gzip compression if HDF5 file is compressed. */

<<<<<<< h4toh5image.c
=======
  
 write_plist = H5Pcreate_list(H5P_DATASET_XFER_NEW);
 bufsize = h4memsize *h5dims[1]*ncomp;

  if(H5Pset_buffer(write_plist,bufsize,NULL,NULL)<0) {
    printf("fail to create data transfer property list.\n");
    free(image_data);
    free(h5cimage_name);
    H5Pclose(create_plist);
    H5Pclose_list(write_plist);
    return FAIL;		
  }

>>>>>>> 1.3
 if (ncomp == 1) {

     h5d_sid = H5Screate_simple(2,h5dims,NULL);
     
     if(h5d_sid <0) {
       printf("error in creating space for dataset. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
        H5Pclose_list(write_plist);
>>>>>>> 1.3
       return FAIL;
     }

     h5dset  = H5Dcreate(h5_group,h5cimage_name,h5ty_id,h5d_sid,create_plist);

     if(h5dset < 0) {
       printf("error in creating hdf5 dataset converted from images. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
        H5Pclose_list(write_plist);
>>>>>>> 1.3
       return FAIL;
     }

     if (H5Dwrite(h5dset,h5memtype,h5d_sid,h5d_sid,H5P_DEFAULT,
		  image_data)<0) {
        printf("error writing data for hdf5 dataset converted from images.\n");
	free(image_data);
	free(h5cimage_name);
        H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
	 H5Pclose_list(write_plist);
>>>>>>> 1.3
        return FAIL;
     }
	
  }
<<<<<<< h4toh5image.c
=======

  else { /* compound datatype. */

     h5_ctype    = H5Tcreate(H5T_COMPOUND,ncomp*h4size);
     if (h5_ctype < 0) {
       printf("error in generating hdf5 compound data type. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }

     h5_cmemtype = H5Tcreate(H5T_COMPOUND,ncomp*h4memsize);
     if (h5_cmemtype < 0) {
       printf("error in generating hdf5 memory compound data type. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }
    
>>>>>>> 1.3

  else { /* 24-bit image */

<<<<<<< h4toh5image.c
     h5d_sid     = H5Screate_simple(3,h5dims24,NULL);
=======
     {
     hid_t arr_type;    /* Array datatype for inserting fields */

     /* Create array datatype */
     if((arr_type=H5Tarray_create(h5ty_id,1,fielddim,NULL))<0) {
       printf("error creating array datatype.\n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }

     ret = H5Tinsert(h5_ctype,"HDF4Image_data",0,arr_type);
     if(ret < 0) {
       printf("error in inserting array of compound datatype. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }

     /* Close array datatype */
     if(H5Tclose(arr_type)<0) {
       printf("error closing array datatype.\n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }

     /* Create array datatype */
     if((arr_type=H5Tarray_create(h5memtype,1,fielddim,NULL))<0) {
       printf("error creating array datatype.\n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }

     ret = H5Tinsert(h5_cmemtype,"HDF4Image_data",0,arr_type);
     if(ret < 0) {
       printf("error in inserting array of compound datatype at memory. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }
     
     /* Close array datatype */
     if(H5Tclose(arr_type)<0) {
       printf("error closing array datatype.\n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
        H5Pclose_list(write_plist);
       return FAIL;
     }
     }

     h5d_sid     = H5Screate_simple(2,h5dims,NULL);
>>>>>>> 1.3
     if(h5d_sid < 0) {
       printf("error in creating space. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
        H5Pclose_list(write_plist);
>>>>>>> 1.3
       return FAIL;
     }

     h5dset      = H5Dcreate(h5_group,h5cimage_name,h5ty_id,h5d_sid,
			     create_plist);
     if(h5dset < 0) {
       printf("error in creating dataset. \n");
       free(image_data);
       free(h5cimage_name);
       H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
        H5Pclose_list(write_plist);
>>>>>>> 1.3
       return FAIL;
     }

     if (H5Dwrite(h5dset,h5memtype,h5d_sid,h5d_sid,H5P_DEFAULT,
		  (void *)image_data)<0) {
        printf("error writing data\n");
	free(image_data);
	free(h5cimage_name);
        H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
	 H5Pclose_list(write_plist);
>>>>>>> 1.3
	return FAIL;
     } 
  }
 
 free(image_data);
/* convert image annotation into attribute of image dataset.
     Since there is no routines to find the exact tag of image object,
     we will check three possible object tags of image objects, that is:
     DFTAG_RIG,DFTAG_RI,DFTAG_RI8. If the object tag of image object is 
     falling out of this scope, we will not convert annotations into
     hdf5 attributes; it is user's responsibility to make sure object tags
     for image objects are only one of the above three tags.*/

  if(Annoobj_h4_to_h5(file_id,gr_ref,DFTAG_RIG,h5dset)== FAIL){
    printf("failed to convert image annotation into hdf5 attribute.\n");
    free(h5cimage_name);
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    return FAIL;
  }

  if(Annoobj_h4_to_h5(file_id,gr_ref,DFTAG_RI,h5dset)== FAIL){
    printf("failed to convert image annotation into hdf5 attribute.\n");
    free(h5cimage_name);
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    return FAIL;
  }

  if(Annoobj_h4_to_h5(file_id,gr_ref,DFTAG_RI8,h5dset)== FAIL){
    printf("failed to convert image annotation into hdf5 attribute.\n");
    free(h5cimage_name);
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    return FAIL;
  }
  
  
 /************************************/
  /* translate GR attributes into HDF5 dataset attribute.*/

  check_gloattr = 0;
  if(gr_tranattrs(ri_id,h5dset,ngrattrs,check_gloattr)==FAIL){ 
    printf(" cannot obtain attributes. \n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    return FAIL;
  }

  /*  deal with h5dset predefined and user-defined attributes. 
      Obtain the name and data type and the total number of attributes.  
      Data attribute at hdf4 is only one-dimensional array. */

 
  if (ncomp == 1 && h4size == 1)
     strcpy(grlabel,RAST8LABEL);
  else if(ncomp == 3 && h4size == 1)
     strcpy(grlabel,RAST24LABEL);
  else
     strcpy(grlabel,GRLABEL);

  strcpy(image_class,IM_CLASS);

  /* transfer hdf4 predefined attributes into hdf5 dataset.*/
  if(h4_transpredattrs(h5dset,HDF4_OBJECT_TYPE,grlabel)==FAIL){
    printf("error in getting hdf4 image type attribute \n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    free(h5cimage_name);
    return FAIL;
  }

  if(h4_attr!=0) {
  if(h4_transpredattrs(h5dset,HDF4_OBJECT_NAME,image_name)==FAIL){
    printf("error in getting hdf4 image name attribute. \n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    free(h5cimage_name);
    return FAIL;
  }
  }
  if(h4_transpredattrs(h5dset,HDF4_IMAGE_CLASS,image_class)==FAIL){
    printf("error in getting hdf4 image class attribute. \n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    free(h5cimage_name);
    return FAIL;
  }

  if(ncomp >1) {
    if(interlace_mode == MFGR_INTERLACE_PIXEL){
      if(h4_transpredattrs(h5dset,INTERLACE_MODE,PIXEL_INTERLACE)==FAIL){
	printf("unable to generate image pixel attribute.\n");
      H5Pclose(create_plist);
      H5Sclose(h5d_sid); 
      H5Dclose(h5dset);
      free(h5cimage_name);
      return FAIL;
      }
    }
    /* currently scan-line is not supported.
    else if (interlace_mode == MFGR_INTERLACE_LINE){
     if(h4_transpredattrs(h5dset,INTERLACE_MODE,LINE_INTERLACE)==FAIL){
      printf("unable to generate image line attribute.\n");
      H5Pclose(create_plist);
      H5Sclose(h5d_sid); 
      H5Dclose(h5dset);
      free(h5cimage_name);
      return FAIL;
      }
      }*/
    else if (interlace_mode == MFGR_INTERLACE_COMPONENT){
      if(h4_transpredattrs(h5dset,INTERLACE_MODE,PLANE_INTERLACE)==FAIL){
       printf("unable to generate image component attribute.\n");
      H5Pclose(create_plist);
      H5Sclose(h5d_sid); 
      H5Dclose(h5dset);
      free(h5cimage_name);
      return FAIL;
      }
    }
   
    else {/* treat as pixel interlace mode. */
     if(h4_transpredattrs(h5dset,INTERLACE_MODE,PIXEL_INTERLACE)==FAIL){
      printf("unable to generate image pixel attribute.\n");
      H5Pclose(create_plist);
      H5Sclose(h5d_sid); 
      H5Dclose(h5dset);
      free(h5cimage_name);
      return FAIL;
      }
    } 
      
  }
  
  if(h4_attr !=0){
 
  gr_ref = GRidtoref(ri_id);

  if(gr_ref == 0) {
    printf("error in obtaining reference number of GR.\n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    free(h5cimage_name);
    return FAIL;
  }

  if(h4_transnumattr(h5dset,HDF4_REF_NUM,gr_ref)==FAIL) {
    printf("error in getting hdf4 image number attribute.\n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    free(h5cimage_name);
    return FAIL;
  }
  }
  /* deal with palette. */

  if(gr_palette(file_id,ri_id,h5dset,h5_palgroup,h4_attr)== FAIL) {
    printf("error in translating palette into h5 dataset.\n");
    H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
     H5Pclose_list(write_plist);
>>>>>>> 1.3
    H5Sclose(h5d_sid); 
    H5Dclose(h5dset);
    free(h5cimage_name);
    return FAIL;
  }
 
  ret   = H5Pclose(create_plist);
<<<<<<< h4toh5image.c
=======
   ret  = H5Pclose_list(write_plist);
>>>>>>> 1.3
  ret   = H5Sclose(h5d_sid); 
  ret   = H5Dclose(h5dset);
  istat = GRendaccess(ri_id);
  free(h5cimage_name);
  return SUCCEED;
}

/**** palette routine. ****/
/*-------------------------------------------------------------------------
 * Function:	gr_palette
 *
 * Purpose:     translate palette  into hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: HDF4 identifier
		ri: raster image id
		h5dset: hdf5 dataset
		h5_palgroup: hdf5 palette group

   Out:         
 *-------------------------------------------------------------------------
 */	

int gr_palette(int32 file_id,int32 ri_id,hid_t h5dset,hid_t h5_palgroup,int h4_attr) {

  int32      pal_id;
  uint16     pal_ref;
  char       palref_str[MAXREF_LENGTH];
  char       palg_name[MAX_GR_NAME];
  char       image_index[MAX_GR_NAME];
  int        check_pal;
  int        check_palname;
  int        pal_stat;
  char*      h5pal_name=NULL;


  /* get palette id */
  pal_id  = GRgetlutid(ri_id,0);
  if(pal_id == FAIL) {
    printf("error in obtaining palette id. \n");
    return FAIL;
  }

  pal_ref = GRluttoref(pal_id);
 
  if(pal_ref >0) {

    /* convert reference number into string format. */
    if(conv_int_str(pal_ref,palref_str)==FAIL) {
      printf("error in converting palette reference number into string.\n");
      return FAIL;
    }

    /* check whether this palette has been looked up already. */
    check_pal = lookup(pal_ref,PAL_HASHSIZE,pal_hashtab);

    if( check_pal < 0) {
      printf("error at looking up palette table. \n");
      return FAIL;
    }

    /* if check_pal equals to 1, this palette has already been 
       converted into hdf5 dataset, just obtain the palette name.
       if check_pal equals to 0, we will do the converting. */

    if(check_pal == 1) {

      h5pal_name = get_name(pal_ref,PAL_HASHSIZE,pal_hashtab,
			    &check_palname);
    
      if (h5pal_name == NULL && check_palname == 0 ) {		      
	 printf("error,cannot find group\n");			      
	 return FAIL;							      
      }							      
									      
      if (h5pal_name == NULL && check_palname == -1 ) {	       
         printf("error,group name is not defined.\n");		      
	 return FAIL;							      
      }	
						      
    }							      
    
    if(check_pal == 0) {
      /* do converting. */
      strcpy(palg_name,HDF4_PALG);

      /* obtain hdf5 dataset name converted from palette,
	 no name for hdf4 palette.*/
      h5pal_name = get_obj_aboname(NULL,palref_str,palg_name,HDF4_PALETTE);
      if(h5pal_name == NULL) {
	printf("error in getting hdf5 palette name.\n");
	return FAIL;
      }

      if(set_name(pal_ref,PAL_HASHSIZE,pal_hashtab,h5pal_name)==FAIL) {
	printf("error in setting object name.\n");	
	free(h5pal_name);
	return FAIL;
      }

      pal_stat = Palette_h4_to_h5(file_id,pal_id,h5_palgroup,h5pal_name,h4_attr);

      if(pal_stat == FAIL) {
        printf("error occurring in transferring palette into dataset. \n");
	free(h5pal_name);
	return FAIL;
      }

    }

    if(create_pal_objref(h5dset,h5_palgroup,h5pal_name)== FAIL) {
      printf("error in creating palette object reference.\n");
      free(h5pal_name);
      return FAIL;
    }

    if(h5pal_name != NULL) free(h5pal_name);

    strcpy(image_index,HDF4_IMAGE_INDEXED);
    if(h4_transpredattrs(h5dset,HDF4_IMAGE_SUBCLASS,image_index)== FAIL) {
      printf("failed to transfer hdf4 image indexed.\n");
      return FAIL;
    }
  }
  return SUCCEED;
}
/***** end of palette application. *****/
/*-------------------------------------------------------------------------
 * Function:	gr_tranattrs
 *
 * Purpose:     translate attributes of Image object into hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                sri_id: RI identifier
		sh5_dset: hdf5 dataset
		snum_grattrs: number of attribute
		check_gloflag: flag to check whether this attribute belongs
		to gr interface.

   Out:         
 *-------------------------------------------------------------------------
 */	
int gr_tranattrs(int32 sri_id, hid_t sh5_dset,int snum_grattrs,
		 int check_gloflag) {
    
  char      sgratrr_name[2*MAX_NC_NAME];
  char      grglo[MAX_NC_NAME];
  char*     grrepattr_name;
  int32     count_sgradata;
  int32     sgr_atype;
  size_t    sh4_amemsize;
  size_t    sh4_asize;

  hid_t     sh5a_sid;
  hid_t     sh5a_id;
  hid_t     sh5_atype;
  hid_t     sh5_amemtype;
  hid_t     sh5str_type;
  hid_t     sh5str_memtype;
  hsize_t   sh5dims[MAX_VAR_DIMS];
  void*     sgr_adata;
  herr_t    sret;
  int       i;

 
  for (i =0;i <snum_grattrs;i++) {
      
     if (GRattrinfo(sri_id,i,sgratrr_name,&sgr_atype,&count_sgradata)==FAIL){  
        printf("unable to obtain attribute information. \n"); 
        return FAIL;
     }
     
     /*convert datatype for attribute. */

     if(h4type_to_h5type(sgr_atype,&sh5_amemtype,&sh4_amemsize,
			 &sh4_asize,&sh5_atype)==FAIL){
       printf("unable to do type transferring.\n");
       return FAIL;
     }

     sgr_adata = malloc(sh4_amemsize*count_sgradata);

     if(GRgetattr(sri_id,i,(VOIDP)sgr_adata)==FAIL){
       printf("unable to get GR attributes. \n");
       return FAIL;
     }
	
     /* if attribute doesn't have name, a default name is set. */
     if(sgratrr_name[0] == '\0') {
       grrepattr_name = trans_obj_name(DFTAG_RIG,i);
       strcpy(sgratrr_name,grrepattr_name);
       free(grrepattr_name);
     }

     /* if the sds attribute is a file attribute. */
     if(check_gloflag == 1){
       strcpy(grglo,GLOIMAGE);
       strcat(sgratrr_name,"_");
       strcat(sgratrr_name,grglo);
     }
     /* now do attribute-transferring.
       1. deal with string data type
       2. set attribute space.
       3. get attribute name, set property list. */
           
     if (sh5_atype == H5T_STRING) {

	sh5a_sid = H5Screate(H5S_SCALAR);

	if (sh5a_sid < 0) {
	   printf("failed to create attribute space for IMAGE. \n"); 
	   return FAIL;
	}

	if ((sh5str_type = mkstr(count_sgradata*sh4_asize,H5T_STR_SPACEPAD))<0){
           printf("error in making string for image attribute \n");
	   return FAIL;
	}

        /* check this line later. */
       	if ((sh5str_memtype = mkstr(count_sgradata*sh4_amemsize,
				    H5T_STR_SPACEPAD))<0){
	  printf("error in making memory string. \n");
	  return FAIL;
	}

        sh5a_id = H5Acreate(sh5_dset,sgratrr_name,sh5str_type,sh5a_sid,
			    H5P_DEFAULT);
        if (sh5a_id <0) {
	   printf("failed to obtain attribute id for IMAGE. \n");
	   return FAIL;
	}

        sret = H5Awrite(sh5a_id,sh5str_memtype,(void *)sgr_adata);

	if (sret <0) {
	  printf("failed to obtain attribute of IMAGE.\n ");
	  return FAIL;
	}

        sret = H5Sclose(sh5a_sid);
        sret = H5Aclose(sh5a_id);
     }
	 
     else {
      
       if (count_sgradata == 1) {

	 sh5a_sid = H5Screate(H5S_SCALAR);

	 if (sh5a_sid < 0) {
	   printf("failed to create space id. \n");
	   return FAIL;
	 }
       }
       else {

          sh5dims[0] = count_sgradata;
	  sh5a_sid =  H5Screate_simple(1,sh5dims,NULL);
          
	  if (sh5a_sid < 0) {
	    printf("failed to create attribute space. \n");
	    return FAIL;
	  }
       }

       sh5a_id = H5Acreate(sh5_dset,sgratrr_name,sh5_atype,sh5a_sid,
			   H5P_DEFAULT);

       if(sh5a_id <0) {
	 printf("failed to obtain attribute id. \n");
         return FAIL;
       }

       sret = H5Awrite(sh5a_id,sh5_amemtype,(void *)sgr_adata);

       if(sret <0) {
	 printf("failed to obtain attribute.\n ");
         return FAIL;
       }
       sret = H5Aclose(sh5a_id);
       sret = H5Sclose(sh5a_sid);

     }

     free(sgr_adata);

  }

  return SUCCEED;
}
	
/*-------------------------------------------------------------------------
 * Function:	create_pal_objref
 *
 * Purpose:     create object reference for palette
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
		h5dset: hdf5 dataset
		h5_palgroup: hdf5 palette group
		h5pal_name: hdf5 palette name

   Out:         
 *-------------------------------------------------------------------------
 */	

int create_pal_objref(hid_t h5dset,hid_t h5_palgroup,char *h5pal_name){

    hobj_ref_t pal_refdat;
    hsize_t    pal_refDims[1];
    hid_t      pal_refSpace;
    hid_t      pal_refType;
    hid_t      attribID;
    herr_t     ret;

    pal_refDims[0] = 1;
    pal_refSpace   = H5Screate_simple(1,pal_refDims,NULL);

    if(pal_refSpace < 0) {
      printf("error in obtaining reference space. \n");
      return FAIL;
    }

    pal_refType    = H5Tcopy(H5T_STD_REF_OBJ);
    if(pal_refType < 0) {
      printf("error in obtaining reference type. \n");
      H5Sclose(pal_refSpace);
      return FAIL;
    }

    ret            = H5Rcreate(&pal_refdat,h5_palgroup,h5pal_name,
				H5R_OBJECT,-1);
    if(ret < 0) {
      printf("error in creating reference space. \n");
      H5Sclose(pal_refSpace);
      H5Tclose(pal_refType);
      return FAIL;
    }

    attribID       = H5Acreate(h5dset,PALETTE,pal_refType,pal_refSpace,
				H5P_DEFAULT);

    if(attribID < 0) {
      printf("error in obtaining attribute ID. \n");
      H5Sclose(pal_refSpace);
      H5Tclose(pal_refType);
      return FAIL;
    }

    ret            = H5Awrite(attribID,pal_refType,(void *)&pal_refdat);

      
    H5Sclose(pal_refSpace);
    if(H5Tclose(pal_refType)<0) {
      printf("error closing palette reference type.\n");
      H5Aclose(attribID);
    }   
    H5Aclose(attribID);
    return SUCCEED;
}


uint16 get_RIref(int32 file_id,uint16 tag,int32 gr_ref,uint16* ptag_out){

  DFdi di;
  int32 found,GroupID;
  int ri_ref = 0;
  
  if((GroupID = DFdiread(file_id,tag,(uint16)gr_ref))<0){
    printf("cannot find gr_ref\n");
    return ri_ref;
  }

  found = 0;
  di.tag = DFTAG_NULL;
  di.ref = 0;
  while((found == 0) &&(DFdiget(GroupID,&di.tag,&di.ref)==0)){
    if(di.tag == DFTAG_CI ||di.tag == DFTAG_RI || di.tag == DFTAG_CI8 ||
       di.tag == DFTAG_RI8)
      found = 1;
  }

  *ptag_out = di.tag;
  ri_ref = di.ref;
  ri_ref = 1;
  if(!found) {
    printf("cannot find ri_ref\n");
    return 0;
  }
  DFdifree(GroupID);
  return ri_ref;

}
