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

Converting an hdf4 palette object into a hdf5 dataset.
Author:  Kent Yang(ymuqun@ncsa.uiuc.edu)
 

*****************************************************************************/

#include "h4toh5main.h"


/*-------------------------------------------------------------------------
 * Function:	Palette_h4_to_h5
 *
 * Purpose:     translate palette  into hdf5 dataset
 *              
 * Return:	FAIL if failed, SUCCEED if successful.
 *
 * In :	        
                file_id: file id
                pal_id: PALETTE identifier
		h5_g: hdf5 group id
		pal_name: path name of the group where all palettes are in

 *-------------------------------------------------------------------------
 */	

int Palette_h4_to_h5(int32 file_id,int32 pal_id,hid_t h5g,char*pal_name) {

  int32   ncomp;
  int32   pal_ref;
  int32   pal_type;
  int32   interlace_mode;
  int32   num_entries;
  void*   pal_data;
  size_t  h4memsize;
  size_t  h4size;
 
  char    palette_label[MAX_NC_NAME];
  char    palette_class[MAX_NC_NAME];
  char    palette_type[MAX_NC_NAME];

  hid_t   h5memtype;
  hid_t   h5type;
  hid_t   h5d_sid;
  hid_t   h5dset;
  hsize_t h5dims[2];
  
  pal_ref = GRluttoref(pal_id);

  if(pal_ref <0) {
    printf("error in obtaining palette.\n");
    return FAIL;
  }

  /* no palette, just return. */
  if(pal_ref == 0) return SUCCEED;

  if(GRgetlutinfo(pal_id,&ncomp,&pal_type,&interlace_mode,&num_entries)==FAIL) {
    printf("error in getting palette information.\n");
    return FAIL;
  }

  if(h4type_to_h5type(pal_type,&h5memtype,&h4memsize,&h4size,&h5type)== FAIL) {
    fprintf(stderr,"failed to translate image datatype. \n");
    return FAIL;
  }
	
  /* according to mapping document, data type for palette will always be
     uint8. */

  if (h5type == H5T_STRING) {
    if(h5string_to_int(pal_type,&h5memtype,h4memsize,&h5type)==FAIL) {
      fprintf(stderr,"failed to translate H5T_STRING to int8.");
      return FAIL;
    }
  }

  h5dims[0] = num_entries;
  h5dims[1] = ncomp;
 
  pal_data = malloc(h4memsize*ncomp*num_entries);

  if (pal_data == NULL) {
    printf("error in allocating memory for palette data.\n");
    return FAIL;
  }

  if (GRreadlut(pal_id,(VOIDP)pal_data)==FAIL) {
    printf("error in reading palette data. \n");
    free(pal_data);
    return FAIL;
  }

  h5d_sid = H5Screate_simple(2,h5dims,NULL);

  if (h5d_sid <0) {
    printf("error in creating space.\n");
    free(pal_data);
    return FAIL;
  }

  h5dset = H5Dcreate(h5g,pal_name,h5type,h5d_sid,H5P_DEFAULT);

  if (h5dset < 0) {
    printf("error in creating dataset. \n");
    free(pal_data);
    H5Sclose(h5d_sid);
    return FAIL;
  }

  if (H5Dwrite(h5dset,h5memtype,h5d_sid,h5d_sid,H5P_DEFAULT,
	       (void *)pal_data)<0) {
     fprintf(stdout,"error writing data for palette data\n");
     free(pal_data);
     H5Sclose(h5d_sid);
     H5Dclose(h5dset);
     return FAIL;
  }
  free(pal_data);

  
  strcpy(palette_label,PALABEL);
  strcpy(palette_class,PALETTE);
  strcpy(palette_type,PAL_TYPE);

  /* convert palette annotation into attribute of palette dataset.
     Since there are no routines to find the exact tag of palette object,
     we will check three possible object tags of palette objects, that is:
     DFTAG_LUT. If the object tag of palette object is 
     falling out of this scope, we will not convert annotations into
     hdf5 attributes; it is user's responsibility to make sure that object tags
     for palette objects are DFTAG_LUT.*/

  if(Annoobj_h4_to_h5(file_id,pal_ref,DFTAG_LUT,h5dset)== FAIL){
    printf("failed to convert palette annotation into hdf5 attribute.\n");
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    return FAIL;
  }

  if(h4_transpredattrs(h5dset,HDF4_OBJECT_TYPE,palette_label)==FAIL) {
    printf("unable to transfer palette label to HDF4 OBJECT TYPE.\n");
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    return FAIL;
  }

  if(h4_transpredattrs(h5dset,HDF4_PALETTE_CLASS,palette_class)==FAIL){
    printf("unable to transfer palette class to HDF4 PALETTE CLASS.\n");
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    return FAIL;
  }

  if(h4_transpredattrs(h5dset,HDF4_PALETTE_TYPE,palette_type)==FAIL){
    printf("unable to transfer palette type to HDF4 PALETTE TYPE.\n");
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    return FAIL;
  }

  if(h4_transnumattr(h5dset,HDF4_REF_NUM,pal_ref)==FAIL) {
    printf("unable to transfer palette reference number to HDF4 REF. NUM.\n");
    H5Sclose(h5d_sid);
    H5Dclose(h5dset);
    return FAIL;
  }
  return SUCCEED;
}
  




