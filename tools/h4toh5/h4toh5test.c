/*** this code is to generate various hdf files to test h4toh5 converter and h4toh5 lib API. The code itself is NOT optimizied but try to consider various cases that examine how converter works. */

#include "hdf.h"
#include "mfhdf.h"
#include <stdio.h>


#define FILESDS1  "sds_typ_test.hdf"
#define FILESDS2  "sds_dim_test.hdf"
#define FILESDS3  "sds_attr_test.hdf"
#define FILEGR    "gr_typ_test.hdf"
#define FILERAS8  "ras_8_test.hdf"
#define FILERAS24 "ras_24_test.hdf"
#define FILEGRPAL "image_attr_test.hdf"
#define FILEVD    "vdata_test.hdf"
#define FILECLASHVG "vgnameclash_test.hdf"
#define FILECLASHSDS "sdsnameclash_test.hdf"
#define FILECLASHVD "vdnameclash_test.hdf"
#define FILECLASHGR "grnameclash_test.hdf"
#define FILELOOP  "vg_loop_test.hdf"
#define FILEHL    "vg_hl_test.hdf"
#define FILEVG    "vg_all_test.hdf"
#define FILEANNO  "anno_test.hdf"

/* for testing sds*/
#define TYP_RANK 3
#define TYP_DIMSIZE 4
#define INT8_UPLIMIT   0x7E
#define UINT8_UPLIMIT  0xFC
#define INT16_UPLIMIT  0x7FFE
#define UINT16_UPLIMIT 0xFFFC
#define INT32_UPLIMIT  0x7FFFFFFE
#define UINT32_UPLIMIT 0xFFFFFFFC
#define ATT_SIZE 10

/* for testing image*/
#define X_LENGTH 10
#define Y_LENGTH 5
#define NUM_COLORS 256
#define F_ATT1_NAME "File Attribute"
#define RI_ATT1_NAME "Image Attribute"
#define F_ATT1_VAL "Contents of First FILE Attribute"
#define F_ATT1_N_VALUES 32
#define RI_ATT1_VAL "Contents of IMAGE's First Attribute"
#define RI_ATT1_N_VALUES 35

/* for testing vdata*/
#define NRECORDS 10
#define FIELD_1 "Temp" 
#define FIELD_2 "Height" 
#define FIELD_3 "Speed" 
#define FIELD_4 "Ident"
#define FIELD_5 "Position"
#define FIELD_NAMES "Temp,Height,Speed,Ident,Position"
#define FIELD_VDNAMES "Temp,Height"

/*for testing vgroup*/
#define VGATTR_NAME "Vgroup attribute 1"
int test_sdstyp(void);
int test_sdsdim(void);
int test_sdsattr(void);
int test_grtyp(void);
int test_ras8(void);
int test_ras24(void);
int test_imageattr(void);
int test_vdata(void);
int test_vgnameclash(void);
int test_sdsnameclash(void);
int test_grnameclash(void);
int test_vdnameclash(void);
int test_vgloop(void);
int test_vghl(void);
int test_vgall(void);
int test_anno(void);

int main(void) {
  if(test_sdstyp()== FAIL) {
    printf("failed to create sds_typ_test.hdf file.\n");
    return FAIL;
  }
  if(test_sdsdim()== FAIL) {
    printf("failed to create sds_dim_test.hdf file. \n");
    return FAIL;
  }
  if(test_sdsattr()== FAIL) {
    printf("failed to create sds_attr_test.hdf file. \n");
    return FAIL;
  }
  if(test_grtyp()==FAIL) {
    printf("failed to create gr_typ_test.hdf file. \n");
    return FAIL;
  }

  if(test_ras8()==FAIL) {
    printf("failed to create ras8_test.hdf file.\n");
    return FAIL;
  }

  if(test_ras24()==FAIL) {
    printf("failed to create ras24_test.hdf file.\n");
    return FAIL;
  }

  if(test_imageattr()== FAIL) {
     printf("failed to create image_attr_test.hdf file.\n");
     return FAIL;
  }

  if(test_vdata()== FAIL) {
    printf("failed to create vdata_test.hdf file.\n");
    return FAIL;
  }

  if(test_vgnameclash()==FAIL) {
    printf("failed to create vg_nameclash.hdf file.\n");
    return FAIL;
  }

  if(test_sdsnameclash()==FAIL) {
    printf("failed to create sds_nameclash.hdf file.\n");
    return FAIL;
  }

  if(test_grnameclash()==FAIL) {
    printf("failed to create gr_nameclash.hdf file. \n");
    return FAIL;
  }

  if(test_vdnameclash()==FAIL) {
    printf("failed to create vd_nameclash.hdf file.\n");
    return FAIL;
  }
  if(test_vgloop()==FAIL) {
    printf("failed to create vg_loop.hdf file. \n");
    return FAIL;
  }
  if(test_vghl()==FAIL) {
    printf("failed to create vg_hl.hdf file. \n");
    return FAIL;
  }
  if(test_vgall()==FAIL) {
    printf("failed to create vg_all.hdf file. \n");
    return FAIL;
  }
  if(test_anno()==FAIL) {
    printf("failed to create vg_anno.hdf file. \n");
    return FAIL;
  }
  return 0;
}

/* this subroutine will hdf file with typical sds objects,

   The rank is TYP_RANK, each dimensional size is TYP_DIMSIZE.
   Datatype we are testing is:
   char,
   int8,
   int16,
   int32,
   uint16,
   uint32,
   lint16,
   lint32,
   luint32,
   float32,
   float64,

*/
int test_sdstyp(){

  int32 file_id,sds_id;
  int32 i,j,k;
  int32 typ_start[TYP_RANK],typ_edges[TYP_RANK],typ_stride[TYP_RANK];
  char8  typchar_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int8   typint8_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int16  typint16_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32  typint32_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  uint16 typuint16_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32  typlint32_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  uint32 typluint32_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  float32 typfloat32_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  float64 typfloat64_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  float64 typlfloat64_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32 typ_dims[TYP_RANK];
  int32 CUB_SIZE;
  int istat;
  /* TYPICAL sds array, we limit the dimensional size for testing purpose. */

  CUB_SIZE = (TYP_DIMSIZE-1)*(TYP_DIMSIZE-1)*(TYP_DIMSIZE-1);

  /* 1. data type is char */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++){
	typchar_array[i][j][k]=(char) (i+j+k);
      }

  /* data type is int8 */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typint8_array[i][j][k]= (int8)(INT8_UPLIMIT-i*j*k*2/CUB_SIZE*INT8_UPLIMIT +1);

  /* data type is int16 */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typint16_array[i][j][k]= (int16)(INT16_UPLIMIT-i*j*k*2/CUB_SIZE*INT16_UPLIMIT +1);

  /* data type is uint16 */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typuint16_array[i][j][k]= (uint16)(UINT16_UPLIMIT-i*j*k*2/CUB_SIZE*(UINT16_UPLIMIT/2)+1);

  /* data type is int32 */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typint32_array[i][j][k]= INT32_UPLIMIT-i*j*k*2/CUB_SIZE*INT32_UPLIMIT +1;
  
  /*data type is little-endian int32*/
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++) 
	typlint32_array[i][j][k]= INT32_UPLIMIT-i*j*k*2/CUB_SIZE*INT32_UPLIMIT +1;
          
  /* data type is unsigned little-endian int32*/
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++) 
	typluint32_array[i][j][k]= (uint32)(UINT32_UPLIMIT-i*j*k/CUB_SIZE*UINT32_UPLIMIT+1);
       
  /* data type is float32 */   
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typfloat32_array[i][j][k]= 1.0+i+j+k;
    
  /* data type is float64 */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typfloat64_array[i][j][k]= 1.0+i+j+k;

  /* data type is lfloat64 */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typlfloat64_array[i][j][k]= 1.0+i+j+k;

  for(i=0;i<TYP_RANK;i++){
    typ_dims[i] = TYP_DIMSIZE;
    typ_start[i] = 0;
    typ_edges[i] = TYP_DIMSIZE;
    typ_stride[i] = 1;
  }

  file_id = SDstart(FILESDS1,DFACC_CREATE);

  if(file_id == FAIL) {
    printf (" open file failed\n");
    return FAIL;
  }
  
  /* we also test different dimensional scale setting for typical array. */
  sds_id = SDcreate(file_id,"sds_char",DFNT_CHAR8,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typchar_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_int8",DFNT_INT8,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typint8_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_int16",DFNT_INT16,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typint16_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
   SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_int32",DFNT_INT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typint32_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_uint16",DFNT_UINT16,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typuint16_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_lint32",DFNT_LINT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typlint32_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_luint32",DFNT_LUINT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typluint32_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_float32",DFNT_FLOAT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typfloat32_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_float64",DFNT_FLOAT64,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typfloat64_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(file_id,"sds_lfloat64",DFNT_LFLOAT64,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typlfloat64_array);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);
  SDend(file_id);
  return 0;
}
/* generating a hdf file with sds dimensional scale dataset in it. 
   both limited and unlimited dimensions are provided.*/  
int test_sdsdim() {

  int32 file_id,sds_id,dim_id;
  int32 i,j,k;
  int32 typ_start[TYP_RANK],typ_edges[TYP_RANK],typ_stride[TYP_RANK];
  int32 typ_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32 typ_dims[TYP_RANK];
  int32 dim_sca0[TYP_DIMSIZE];
  int32 istat;

  char dim_name0[] = "dim0";
  char dim_name1[] = "dim1";
  char dim_name2[] = "dim2";

  char unldim_name0[] ="unldim0";
  char unldim_name1[]="unldim1";
  char unldim_name2[]="unldim2";

  /* typical sds array. */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typ_array[i][j][k] = i+j+k;

  for (i=0;i<TYP_DIMSIZE;i++)
    dim_sca0[i] = i;


  for(i=0;i<TYP_RANK;i++){
    typ_dims[i] = TYP_DIMSIZE;
    typ_start[i] = 0;
    typ_edges[i] = TYP_DIMSIZE;
    typ_stride[i] = 1;
  }
  
  file_id = SDstart(FILESDS2,DFACC_CREATE);
  
  if(file_id == FAIL){
    printf (" open file failed\n");
    return FAIL;
  }

  /* testing for normal dimensional scale dataset. */
  sds_id = SDcreate(file_id,"sds_dimnor",DFNT_INT32,TYP_RANK,typ_dims);

  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  
 
  istat=SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typ_array);

  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }

  for (i =0;i<TYP_RANK;i++){
    dim_id = SDgetdimid(sds_id,i);

    if(dim_id == FAIL) {
      printf("failed to generate dimensional id.\n");
      return FAIL;
    }

    switch(i) {

    case 0:
      istat = SDsetdimname(dim_id,dim_name0);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      istat =SDsetdimscale(dim_id,typ_dims[0],DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
         printf("sds set dim. scale failed. \n");
         return FAIL;
      }
      break;
    case 1:
      istat = SDsetdimname(dim_id,dim_name1);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      istat = SDsetdimscale(dim_id,typ_dims[1],DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
        printf("sds set dim. scale failed. \n");
        return FAIL;
      }
      break;
    case 2:
      istat = SDsetdimname(dim_id,dim_name2);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      istat = SDsetdimscale(dim_id,typ_dims[2],DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
        printf("sds set dim. scale failed. \n");
        return FAIL;
      }
      break;
    }
    
  }
  SDendaccess(sds_id);

  /* for unlimited dimensional scale data. */

  typ_dims[0] = SD_UNLIMITED;

  sds_id = SDcreate(file_id,"sds_dimunl",DFNT_INT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }

  istat =SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typ_array);
  if(istat == FAIL) {
    printf("failed to write sds object. \n");
    return FAIL;
  }
  for (i =0;i<TYP_RANK;i++){
    dim_id = SDgetdimid(sds_id,i);
    if(dim_id == FAIL) {
      printf("failed to generate dimensional id.\n");
      return FAIL;
    }
    switch(i) {

    case 0:
      istat= SDsetdimname(dim_id,unldim_name0);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      /* SDsetdimscale(dim_id,typ_dims[0],DFNT_INT32,(VOIDP)dim_sca0);*/
      istat= SDsetdimscale(dim_id,TYP_DIMSIZE,DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
        printf("sds set dim. scale failed. \n");
        return FAIL;
      }
      break;
    case 1:
      istat= SDsetdimname(dim_id,unldim_name1);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      istat = SDsetdimscale(dim_id,typ_dims[1],DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
        printf("sds set dim. scale failed. \n");
        return FAIL;
      }
      break;
    case 2:
      istat = SDsetdimname(dim_id,unldim_name2);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      istat = SDsetdimscale(dim_id,typ_dims[2],DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
        printf("sds set dim. scale failed. \n");
        return FAIL;
      }
      break;
    }
  }
  SDendaccess(sds_id);

  SDend(file_id);
  return 0;
}

/* a hdf file that includes sds predefined attributes, dimensional
   scale attributes and chunking and compress information*/
int test_sdsattr() {

  int32 file_id,sds_id,dim_id;
  int32 i,j,k,comp_flag;
  int32 typ_start[TYP_RANK],typ_edges[TYP_RANK],typ_stride[TYP_RANK];
  int32 typ_array[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32 typ_dims[TYP_RANK];
  int32 fill_value;
  int32 dim_sca0[TYP_DIMSIZE],dim_sca1[TYP_DIMSIZE];

  HDF_CHUNK_DEF c_def;
  int32 comp_type;
  comp_info c_info;
  int32 istat;
  float64 cal;
  float64 cal_err;
  float64 offset;
  float64 offset_err;

  char* attr_value;
  char* gloattr_value;
  char label[] = "sds.label";
  char unit[] = "sds.unit";
  char format[] = "sds.format";
  char coordsys[] = "sds.coord";

  char dim_name0[] ="dim0";
  char dim_name1[] ="dim1";
  char dim_label[] ="dim.label";
  char dim_unit[] ="dim.unit";
  char dim_format[] ="dim.format";

  /**** initial setting. ****/
  cal = 1.0;
  cal_err = 0.0;
  offset = 0.0;
  offset_err = 0.0;
  fill_value = 999;


  /* typical sds array. */
  for (i=0;i<TYP_DIMSIZE;i++)
    for (j=0;j<TYP_DIMSIZE;j++)
      for (k=0;k<TYP_DIMSIZE;k++)
	typ_array[i][j][k]= i+j+k;

  attr_value = malloc(ATT_SIZE*sizeof(char));
  if(attr_value == NULL) {
    printf("failed to allocate memory. \n");
    return FAIL;
  }

  gloattr_value = malloc(ATT_SIZE*sizeof(char));
  if(gloattr_value == NULL) {
    printf("failed to allocate memory. \n");
    return FAIL;
  }

  strcpy(gloattr_value,"glo attr");
  strcpy(attr_value,"test attr");

  for(i=0;i<TYP_RANK;i++){
    typ_dims[i] = TYP_DIMSIZE;
    typ_start[i] = 0;
    typ_edges[i] = TYP_DIMSIZE;
    typ_stride[i] = 1;
  }

  for (i=0;i<TYP_DIMSIZE;i++){
    dim_sca0[i] = i;
    dim_sca1[i] = 2*i;
  }
  file_id = SDstart(FILESDS3,DFACC_CREATE);

  if(file_id == FAIL) {
    printf (" open file failed\n");
    return FAIL;
  }
  
  istat = SDsetattr(file_id,"sds.gloattr",DFNT_CHAR8,10,(VOIDP)gloattr_value);
  if(istat == FAIL) {
    printf("failed to set attribute.\n");
    return FAIL;
  }

  istat = SDsetfillmode(file_id,SD_NOFILL);

  if (istat == FAIL) {
    printf("error setting fill mode\n");
    return FAIL;
  }

  sds_id = SDcreate(file_id,"sds_attr",DFNT_INT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }

  istat = SDsetfillvalue(sds_id,(VOIDP)(&fill_value));
  if (istat == FAIL){
    printf("error setting fill value\n");
    return FAIL;
  }

  istat = SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typ_array);

  if(istat == FAIL) {
    printf("failed to write sds data.\n");
    return FAIL;
  }
  /*** write dataset attribute ***/  
  
  istat = SDsetattr(sds_id,"sds.attr",DFNT_CHAR8,10,(VOIDP)attr_value); 
  if(istat == FAIL) {
    printf(" sds data attr. setting failed. \n");
    return FAIL;
  }
  /*** write dataset predefined attribute ***/

  istat = SDsetdatastrs(sds_id,label,unit,format,coordsys);
  if(istat == FAIL) {
    printf(" sds data predefined attr. setting failed. \n");
    return FAIL;
  }
  /*** set calibration information. ***/

  istat = SDsetcal(sds_id,cal,cal_err,offset,offset_err,DFNT_INT32);
  if(istat == FAIL) {
    printf(" sds data calibrating attr. setting failed. \n");
    return FAIL;
  }
  for (i =0; i<TYP_RANK;i++) {
    dim_id = SDgetdimid(sds_id,i);
    if(dim_id == FAIL) {
      printf("sds set dim id failed. \n");
      return FAIL;
    }
    if (i==0) {
      istat = SDsetdimname(dim_id,dim_name0);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
        return FAIL;
      }
      istat=SDsetdimscale(dim_id,typ_dims[0],DFNT_INT32,(VOIDP)dim_sca0);
      if(istat == FAIL) {
	printf("sds set dim. scale failed. \n");
        return FAIL;
      }
    }
    else {
      istat = SDsetdimname(dim_id,dim_name1);
      if(istat == FAIL) {
        printf("sds set dim.name failed. \n");
	return FAIL;
      }
      istat = SDsetdimscale(dim_id,typ_dims[1],DFNT_INT32,(VOIDP)dim_sca1);
      if(istat == FAIL) {
	printf("sds set dim. scale failed. \n");
	return FAIL;
      }
    }
 
    istat = SDsetdimstrs(dim_id,dim_label,dim_unit,dim_format);
    if(istat == FAIL) {
      printf("sds set dim. predefined attr. failed. \n");
      return FAIL;
    }
    istat = SDsetattr(dim_id,"sdsdim.attr",DFNT_CHAR8,10,(VOIDP)attr_value);

    if(istat == FAIL) {
      printf(" sds dim data attr. setting failed. \n");
      return FAIL;
    }
    SDendaccess(dim_id);
  }

  SDendaccess(sds_id);


  sds_id = SDcreate(file_id,"sds_compress",DFNT_INT32,TYP_RANK,typ_dims);
  if(sds_id == FAIL) {
    printf("failed to create object.\n");
    return FAIL;
  }
  comp_type = COMP_CODE_DEFLATE;
  c_info.deflate.level = 3;

  c_def.comp.chunk_lengths[0] = 2;
  c_def.comp.chunk_lengths[1] = 2;
  c_def.comp.chunk_lengths[2] = 2;
  c_def.comp.comp_type = COMP_CODE_DEFLATE;
  
  comp_flag = HDF_CHUNK;

  c_def.comp.cinfo.deflate.level = 3;

   istat = SDsetchunk(sds_id,c_def,comp_flag);
   if(istat == FAIL) {
      printf("chunking is not setting properly. \n");   
      return FAIL;
  }
  
   
  istat = SDwritedata(sds_id,typ_start,typ_stride,typ_edges,(VOIDP)typ_array);
  if(istat == FAIL) {
    printf("SDS cannot write chunking and compression mode. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  SDend(file_id);
  free(attr_value);
  free(gloattr_value);
  return 0;
}
 
/***** routines for generating gr datatype hdf testing file.
       we are only generating one and three components for
       datatype 
       uint32 and int16. ****/
int test_grtyp() {

  int32 gr_id, ri_id, file_id, il;
  int32  ncomp;
  int32 start[2], edges[2],dims[2];
  uint32 image_data32[Y_LENGTH][X_LENGTH][3];
  uint32 image_data321[Y_LENGTH][X_LENGTH];
  int16  image_data16[Y_LENGTH][X_LENGTH][3];
  int16  image_data161[Y_LENGTH][X_LENGTH];
  intn i, j;
  int32 CUB_SIZE;
  int istat;

  CUB_SIZE = (X_LENGTH-1)*(Y_LENGTH-1);
 
  /* 3-component GR image data type UINT32*/
  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data32[j][i][0] = UINT32_UPLIMIT - i*j*0/CUB_SIZE*(UINT32_UPLIMIT/2)+1;
      image_data32[j][i][1] = UINT32_UPLIMIT - i*j*1/CUB_SIZE*(UINT32_UPLIMIT/2)+1;
      image_data32[j][i][2] = UINT32_UPLIMIT - i*j*2/CUB_SIZE*(UINT32_UPLIMIT/2)+1;
           			
    }
  }

  /* 3-component GR image data type int16*/
  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data16[j][i][0] = INT16_UPLIMIT - i*j*0/CUB_SIZE*(INT16_UPLIMIT/2)+1;
      image_data16[j][i][1] = INT16_UPLIMIT - i*j*1/CUB_SIZE*(INT16_UPLIMIT/2)+1;
      image_data16[j][i][2] = INT16_UPLIMIT - i*j*2/CUB_SIZE*(INT16_UPLIMIT/2)+1;
			
    }
  }

  /* 1-component GR image data type uint32 */
  for (j = 0; j < Y_LENGTH; j++) 
    for (i = 0; i < X_LENGTH; i++) 
      image_data321[j][i] = UINT32_UPLIMIT - i*j/CUB_SIZE*UINT32_UPLIMIT+1;
			
  /* 1-component GR image data type  int16*/
  for (j = 0; j < Y_LENGTH; j++) 
    for (i = 0; i < X_LENGTH; i++) 
      image_data161[j][i] = INT16_UPLIMIT - i*j/CUB_SIZE*INT16_UPLIMIT+1;
	

  /* Open the file. */
  file_id = Hopen(FILEGR, DFACC_CREATE, 0);

  if(file_id == FAIL) {
    printf("fail to create GR file.\n");
    return FAIL;
  }
  /* Initiate the GR interface. */
  gr_id = GRstart(file_id);

  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }

  /* Define the number of components and dimensions of the image. */
  ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_uint32", ncomp, DFNT_UINT32, il, dims);

  if(ri_id == FAIL) {
    printf("fail to create RI interface.\n");
    return FAIL;
  }

  /* Define the location, pattern, and size of the data set */
  for (i = 0; i < 2; i++) {
    start[i] = 0;
    edges[i] = dims[i];
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data32);

  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  /* Terminate access to the image. */
  GRendaccess(ri_id);

  /* For number of components =3, data type is int16*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_int16", ncomp, DFNT_INT16, il, dims);
  
  if(ri_id == FAIL) {
    printf("fail to create RI interface.\n");
    return FAIL;
  }
  
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data16);

  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  /* Terminate access to the image. */
  GRendaccess(ri_id);

  /* For number of components =1, data type is uint32*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 1;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_uint321", ncomp, DFNT_UINT32, il, dims);
  if(ri_id == FAIL) {
    printf("fail to create RI interface.\n");
    return FAIL;
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data321);

  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  /* Terminate access to the image. */
  GRendaccess(ri_id);
  

  /* For number of components = 1, data type is int16*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 1;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;
  
  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_int161", ncomp, DFNT_INT16, il, dims);

  if(ri_id == FAIL) {
    printf("fail to create RI interface.\n");
    return FAIL;
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data161);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  /* Terminate access to the image. */
  GRendaccess(ri_id);
  /* Terminate access to the GR interface. */
  GRend(gr_id);

  /* Close the file. */
  Hclose(file_id);
  return 0;
}


/***** routine to generate raster 8 bit data type hdf file. ****/
int  test_ras8() {

  int32 gr_id, ri_id, file_id, il,istat;
  int32  ncomp;
  int32 start[2], edges[2],dims[2];
  int8 image_data8[Y_LENGTH][X_LENGTH];
  uint8 image_datau8[Y_LENGTH][X_LENGTH];
  intn i, j;
  int32 CUB_SIZE;

  CUB_SIZE = (X_LENGTH-1)*(Y_LENGTH-1);

  /* 1-component Raster-8 image data type uint8 */
  for (j = 0; j < Y_LENGTH; j++) 
    for (i = 0; i < X_LENGTH; i++) 
      image_datau8[j][i] = UINT8_UPLIMIT - i*j/CUB_SIZE*UINT8_UPLIMIT+1;
			
  /* 1-component Raster-8 image data type  int8*/
  for (j = 0; j < Y_LENGTH; j++) 
    for (i = 0; i < X_LENGTH; i++) 
      image_data8[j][i] = INT8_UPLIMIT - i*j/CUB_SIZE*INT8_UPLIMIT+1;

  /* Open the file. */
  file_id = Hopen(FILERAS8, DFACC_CREATE, 0);

  if(file_id == FAIL) {
    printf("fail to open raster 8 file.\n");
    return FAIL;
  }
  /* Initiate the GR interface. */
  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }

  /* For number of components = 1, data type is uint8*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 1;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;
  /* define start and edges value. */
  for (i =0;i<2;i++) {
	start[i] = 0;
        edges[i] = dims[i];
  }

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_uint8", ncomp, DFNT_UINT8, il, dims);
  if(ri_id == FAIL) {
    printf("fail to create GR object.\n");
    return FAIL;
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_datau8);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  istat = GRendaccess(ri_id);
  if(istat == FAIL) {
    printf("fail in ending RI interface.\n");
    return FAIL;
  }

  /* For number of components = 1, data type is int8*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 1;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;
  
  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_int8", ncomp, DFNT_INT8, il, dims);
  if(ri_id == FAIL) {
    printf("fail to create GR object.\n");
    return FAIL;
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data8);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  GRendaccess(ri_id);
  /* Terminate access to the GR interface. */
  GRend(gr_id);

  /* Close the file. */
  Hclose(file_id);
  return 0;
}

/***** routine to generate raster 24 bit data type hdf file. ****/
int  test_ras24() {

  int32 gr_id, ri_id, file_id, il;
  int32 ncomp,istat;
  int32 start[2], edges[2],dims[2];
  int8 image_data24[Y_LENGTH][X_LENGTH][3];
  uint8 image_datau24[Y_LENGTH][X_LENGTH][3];
  intn i, j;
  int32 CUB_SIZE;


  CUB_SIZE = (X_LENGTH-1)*(Y_LENGTH-1);

  /* 3-component Raster-8 image data type uint8 */
  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_datau24[j][i][0] = UINT8_UPLIMIT - i*j*0/CUB_SIZE*UINT8_UPLIMIT+1;
      image_datau24[j][i][1] = UINT8_UPLIMIT - i*j*1/CUB_SIZE*(UINT8_UPLIMIT/2)+1;
      image_datau24[j][i][2] = UINT8_UPLIMIT - i*j*2/CUB_SIZE*(UINT8_UPLIMIT/2)+1;		
    }
  }	
  /* 3-component Raster-8 image data type int8*/
  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data24[j][i][0] = INT8_UPLIMIT - i*j*0/CUB_SIZE*INT8_UPLIMIT+1;
      image_data24[j][i][1] = INT8_UPLIMIT - i*j*1/CUB_SIZE*(INT8_UPLIMIT/2)+1;
      image_data24[j][i][2] = INT8_UPLIMIT - i*j*2/CUB_SIZE*(INT8_UPLIMIT/2)+1;
			
    }
  }

  /* Open the file. */
  file_id = Hopen(FILERAS24, DFACC_CREATE, 0);

  if(file_id == FAIL) {
    printf("fail to open the file. \n");
    return FAIL;
  }

  /* Initiate the GR interface. */
  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }
 
  
  /* For number of components = 3, data type is uint8*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;
  for(i=0;i<2;i++) {
	start[i] = 0;
        edges[i] = dims[i];
  }

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_uint24", ncomp, DFNT_UINT8, il, dims);
  if(ri_id == FAIL) {
    printf("fail to create GR object.\n");
    return FAIL;
  }
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_datau24);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  /* Terminate access to the image. */
  GRendaccess(ri_id);
  

  /* For number of components = 3, data type is int8*/
  /* Define the number of components and dimensions of the image. */
  ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;
  
  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_int24", ncomp, DFNT_INT8, il, dims);
  if(ri_id == FAIL) {
    printf("fail to create GR object.\n");
    return FAIL;
  }
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data24);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  GRendaccess(ri_id);
  /* Terminate access to the GR interface. */
  GRend(gr_id);

  /* Close the file. */
  Hclose(file_id);
  return 0;
}

/* routine to generate image attribute file.*/
int test_imageattr() {

  int32 gr_id, ri_id, file_id, il,pal_id,istat;
  int32 ncomp,data_type;
  int32 start[2], edges[2],dims[2];
  uint16 image_data[Y_LENGTH][X_LENGTH][3];

  uint8 palette_data[NUM_COLORS * 3];
  int32 num_comp,num_entries;
  intn i, j;

  /* Open the file. */
  file_id = Hopen(FILEGRPAL, DFACC_CREATE, 0);

  /* Initiate the GR interface. */
  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }

  /* Define the number of components and dimensions of the image. */
  ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  dims[0] = X_LENGTH;
  dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_1", ncomp, DFNT_UINT16, il, dims);
   if(ri_id == FAIL) {
    printf("fail to create GR object.\n");
    return FAIL;
  }
  /* Fill the stored-data array with values. */
  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data[j][i][0] = i+j+1;
      image_data[j][i][1] = (i+j)*2;
      image_data[j][i][2] = (i+j)*3;
    }
  }

  /* Define the location, pattern, and size of the data set */
  for (i = 0; i < 2; i++) {
    start[i] = 0;
    edges[i] = dims[i];
  }

  /* set GR global attribute. */

  istat = GRsetattr(gr_id,"File Attribute",DFNT_CHAR8,F_ATT1_N_VALUES,(VOIDP)F_ATT1_VAL);
   if(istat == FAIL) {
    printf("fail to set GR global attribute.\n");
    return FAIL;
  }
  /* set GR attribute. */
  istat = GRsetattr(ri_id,"Image Attribute",DFNT_CHAR8,RI_ATT1_N_VALUES,(VOIDP)RI_ATT1_VAL);
  if(istat == FAIL) {
    printf("fail to set GR  attribute.\n");
    return FAIL;
  }  
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data);
   if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  /* Initialize the palette to grayscale. */
  for (i = 0; i < NUM_COLORS; i++) {
    palette_data[i * 3] = i;
    palette_data[i * 3 + 1] = i;
    palette_data[i * 3 + 2] = i;
  }

  /* Set palette characteristics. */
  data_type = DFNT_UINT8;
  num_entries = NUM_COLORS;
  num_comp = 3;
	
  /* Get the id for the palette. */
  pal_id = GRgetlutid(ri_id,0 );
  if(pal_id == FAIL) {
    printf("fail to obtain palette id.\n");
    return FAIL;
  }

  /* Write the palette to file. */
  istat = GRwritelut(pal_id, num_comp, data_type, 
		      0, num_entries, 
		      (VOIDP)palette_data);
  if(istat == FAIL) {
    printf("fail to write lookup table.\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  GRendaccess(ri_id);

  /* Terminate access to the GR interface. */
  GRend(gr_id);

  /* Close the file. */
  Hclose(file_id);
  return 0;
}

/* routines to generate vdata hdf testing file. */
int test_vdata( ) 
{

  struct {
    float32      temp;
    int16        height;
    float32      speed;
    char         ident[3];
    float32      position[2];
  } source[NRECORDS];

  int32  file_id, vdata_id, istat, values[4] ={32, 16, 32, 8};
  uint8  *databuf, *pntr;
  int    i,j, bufsize, recsize;
  VOIDP  fldbufpt[5];

  /* Open the HDF file. */
  file_id = Hopen(FILEVD, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open HDF file.\n");
    return FAIL;
  }

  /* Initialize the Vset interface. */
  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start V interface.\n");
    return FAIL;
  }

  /* Create a new Vdata. */
  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach Vdata.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define field1 \n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
  if(istat == FAIL) {
    printf("fail to define field2 \n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_3, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define field3 \n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_4, DFNT_CHAR8, 3); 
  if(istat == FAIL) {
    printf("fail to define field4 \n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_5, DFNT_FLOAT32,2);
  if(istat == FAIL) {
    printf("fail to define field5 \n");
    return FAIL;
  }

  /* Set the Vdata name. */
  istat=VSsetname(vdata_id, "Test Vset Name");
  if(istat == FAIL) {
    printf("fail to set vdata name\n");
    return FAIL;
  }

  /* Set the Vdata class. */
  istat=VSsetclass(vdata_id, "Test Vset Class");
  if(istat == FAIL) {
    printf("fail to set vdata class\n");
    return FAIL;
  }

  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_NAMES);
  if(istat == FAIL) {
    printf("fail to set fields of vdata.\n");
    return FAIL;
  }

  recsize = (2 * sizeof(float32) + sizeof(int16))+ 2* sizeof(float32) 
    +3*sizeof(char); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc memory for vdata failed\n");
    return FAIL;
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
    source[i].speed = 1.11 * (i+1);
    source[i].ident[0] = 'A' + i; 
    source[i].ident[1] = 'a' + i;
    source[i].ident[2] ='0'+i;
    for (j=0; j< 2; j++) 
      source[i].position[j] = 1.0+j;
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    fldbufpt[2] = &source[i].speed;
    fldbufpt[3] = &source[i].ident[0]; 
    fldbufpt[4] = &source[i].position[0];
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
    return FAIL;
  }
  /* Set Vdata attribute */
  istat = VSsetattr (vdata_id, _HDF_VDATA, "vdata attr", DFNT_INT32, 4, (VOIDP)values);
  if(istat == FAIL) {
    printf("fail to set vdata attribute.\n");
    return FAIL;
  }
  /* Set attribute for "speed" field */
  istat = VSsetattr (vdata_id, 2, "field attr", DFNT_CHAR, 3, "MAX");
  if(istat == FAIL) {
    printf("fail to set vdata field attribute.\n");
    return FAIL;
  }
  /* 
   * Terminate access to the Vdata, the Vset interface 
   * and the HDF file.
   */
  VSdetach(vdata_id);
  Vend(file_id);
  Hclose(file_id);
  free(databuf);
  return 0;
}

/* this routine will generate hdf file that has name clashings for different
   vgroups. 

   two situations: 
     1. two groups share the same name.
     2. one group doesn't have name.
    
   */
int test_vgnameclash() {

  int32	  file_id, vgroupa_ref, vgroupa_id,vgroupb_ref,vgroupb_id;
  int32   vgroupc_id,vgroupc_ref;
  int32	  dim_sizes[TYP_RANK];
  intn    i, j;
  int32   sd_id,sds_id;
  int32   sds_ref;
  int32   array_data[X_LENGTH][Y_LENGTH];
  int32   start[TYP_RANK],edges[TYP_RANK],stride[TYP_RANK];
  int     istat;

  for (i=0;i<X_LENGTH;i++){
    for(j=0;j<Y_LENGTH;j++) {
      array_data[i][j] =i+j;
    }
  }

  dim_sizes[0] = X_LENGTH;
  dim_sizes[1] = Y_LENGTH;

  for (i=0;i<TYP_RANK;i++){
    stride[i]=1;
    start[i]=0;
    edges[i]=dim_sizes[i];

  }

  /* Open the HDF file. */

  /* We are testing name clashings for vgroups.*/

  /* two situations: 
     1. two groups share the same name.
     2. one group doesn't have name. */

  file_id = Hopen(FILECLASHVG, DFACC_CREATE, 0);
  if (file_id == FAIL) {
    printf("fail to open vg_clash.hdf.\n");
    return FAIL;
  }

  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start vgroup interface.\n");
    return FAIL;
  }
  vgroupa_ref = -1;
  vgroupb_ref = -1;
  vgroupc_ref = -1;
  vgroupa_id = Vattach(file_id, vgroupa_ref, "w");
  if(vgroupa_id == FAIL) {
    printf("fail to attach groupa.\n");
    return FAIL;
  }
  vgroupb_id = Vattach(file_id,vgroupb_ref,"w");
   if(vgroupb_id == FAIL) {
    printf("fail to attach groupb.\n");
    return FAIL;
  }
  vgroupc_id = Vattach(file_id,vgroupc_ref,"w");
   if(vgroupc_id == FAIL) {
    printf("fail to attach groupc.\n");
    return FAIL;
  }
  istat=Vsetname(vgroupa_id,"groupA");
  if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }

  istat=Vsetname(vgroupb_id,"groupA");
  if(istat == FAIL) {
    printf("fail to set name for groupb.\n");
    return FAIL;
  }
  sd_id = SDstart(FILECLASHVG,DFACC_WRITE);
  if(sd_id == FAIL) {
    printf("fail to start SD interface.\n");
    return FAIL;
  }

  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("failed to transfer sds id to reference number.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupa_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add tag for reference.\n");
    return FAIL;
  }

  SDendaccess(sds_id);
  SDend(sd_id);
  Vdetach(vgroupa_id);
  istat = Vdetach(vgroupb_id);
  istat = Vdetach(vgroupc_id);
  istat = Vend(file_id);
  istat = Hclose(file_id);
  return 0;
}

/* This routine creates different SDS objects with name clashing.
We are testing different SDS 
       1. under the same group sharing the same name. 
       2. one sds doesn't have name.
       3. different sds objects under different vgroups sharing the same name.
       4. sds objects under no specific groups share the same name.
       5. sds objects under no specific groups with no name. */
  

int test_sdsnameclash() {


  int32	  file_id, vgroupa_ref, vgroupa_id,vgroupb_ref,vgroupb_id;
  int32	  dim_sizes[TYP_RANK];
  int32   vgroupc_ref,vgroupc_id;
  intn    i, j,istat;
  int32   sd_id,sds_id;
  int32   sds_ref;
  int32   array_data[X_LENGTH][Y_LENGTH];
  int32   start[TYP_RANK],edges[TYP_RANK],stride[TYP_RANK];


  for (i=0;i<X_LENGTH;i++){
    for(j=0;j<Y_LENGTH;j++) {
      array_data[i][j] =i+j;
    }
  }

  dim_sizes[0] = X_LENGTH;
  dim_sizes[1] = Y_LENGTH;

  for (i=0;i<TYP_RANK;i++){
    stride[i]=1;
    start[i]=0;
    edges[i]=dim_sizes[i];

  }

  /* Open the HDF file. */

  file_id = Hopen(FILECLASHSDS, DFACC_CREATE, 0);
  istat = Vstart(file_id);
  vgroupa_ref = -1;
  vgroupb_ref = -1;
  vgroupc_ref = -1;
  vgroupa_id = Vattach(file_id,vgroupa_ref,"w");
  if(vgroupa_id == FAIL) {
    printf("fail to attach groupa.\n");
    return FAIL;
  }
  vgroupb_id = Vattach(file_id,vgroupb_ref,"w");
  if(vgroupb_id == FAIL) {
    printf("fail to attach groupb.\n");
    return FAIL;
  }
  vgroupc_id = Vattach(file_id,vgroupc_ref,"w");
   if(vgroupc_id == FAIL) {
    printf("fail to attach groupc.\n");
    return FAIL;
  }
  istat=Vsetname(vgroupa_id,"groupA");
   if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }
  istat=Vsetname(vgroupb_id,"groupB");
  if(istat == FAIL) {
    printf("fail to set name for groupb.\n");
    return FAIL;
  }
  Vsetname(vgroupc_id,"groupC");
  if(istat == FAIL) {
    printf("fail to set name for groupc.\n");
    return FAIL;
  }
  sd_id = SDstart(FILECLASHSDS,DFACC_WRITE);
  if(sd_id == FAIL) {
    printf("fail to start SD interface.\n");
    return FAIL;
  }
  /* putting one sds object under groupa. */
  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }

  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("failed to transfer sds id to reference number.\n");
    return FAIL;
  }
  /* a sds object with the name "sds" is put under group a.*/
  istat = Vaddtagref(vgroupa_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add tag for reference.\n");
    return FAIL;
  }
  SDendaccess(sds_id);

  /* putting another sds with the same same "sds" under groupa.
      It is legal for hdf lib. */

  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("failed to transfer sds id to reference number.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupa_id,DFTAG_NDG,sds_ref);
   if(istat == FAIL) {
    printf("fail to add tag for reference.\n");
    return FAIL;
  }

  SDendaccess(sds_id);

  /* no sds name is given under groupc*/
  sds_id = SDcreate(sd_id,NULL,DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
    if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
   if(sds_ref == FAIL) {
    printf("failed to transfer sds id to reference number.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupc_id,DFTAG_NDG,sds_ref);
   if(istat == FAIL) {
    printf("fail to add tag for reference.\n");
    return FAIL;
  }

  SDendaccess(sds_id);

  /* another no name sds object is put under group c. */
  sds_id = SDcreate(sd_id,NULL,DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("failed to transfer sds id to reference number.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupc_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add tag for reference.\n");
    return FAIL;
  }
  SDendaccess(sds_id);

  /* another sds with the same name under groupb is given.*/
  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("failed to transfer sds id to reference number.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupb_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add tag for reference.\n");
    return FAIL;
  }
  SDendaccess(sds_id);
  
  /* two sds share the same name under no specific groups.*/
  sds_id = SDcreate(sd_id,"sds_independent",DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  sds_id = SDcreate(sd_id,"sds_independent",DFNT_INT32,2,dim_sizes);
  if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  /* another sds with no name and is not tagged with any group. */
  sds_id = SDcreate(sd_id,NULL,DFNT_INT32,2,dim_sizes);
   if(sds_id == FAIL) {
    printf("failed to create sds object.\n");
    return FAIL;
  }
  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("failed to write sds data. \n");
    return FAIL;
  }
  SDendaccess(sds_id);

  SDend(sd_id);
  istat = Vdetach(vgroupa_id);
  istat = Vdetach(vgroupb_id);
  istat = Vdetach(vgroupc_id);
  istat = Vend(file_id);
  istat = Hclose(file_id);
  return 0;
}
  
/*A routine to generate a testing file that deals with different
name clashings for image. 
We are testing different Image
      1. under the same group sharing the same name. 
      2. one image  doesn't have name(not allowed)
      3. different image objects under different vgroups sharing the same name.
      4. image objects under no specific groups share the same name.
      5. image objects under no specific groups with no name(not allowed). */
  

int test_grnameclash() {


  int32	  file_id, vgroupa_ref, vgroupa_id,vgroupb_ref,vgroupb_id,istat;
  int32	  dim_sizes[2];
 
  int    i, j;
  int32   gr_id,ri_id,il,ncomp;
  int32   gr_ref;
  int32   start[2], edges[2];
  uint32  image_data32[Y_LENGTH][X_LENGTH][3];

  for (j=0;j<Y_LENGTH;j++){
    for(i=0;i<X_LENGTH;i++) {
      image_data32[j][i][0] = i;
      image_data32[j][i][1] = j;
      image_data32[j][i][2] = i+j;
    }
  }

  dim_sizes[0] = X_LENGTH;
  dim_sizes[1] = Y_LENGTH;

  
  /* Open the HDF file. */

  file_id = Hopen(FILECLASHGR, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open the file.\n");
    return FAIL;
  }
  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start vgroup interface.\n");
    return FAIL;
  }
  vgroupa_ref = -1;
  vgroupb_ref = -1;

  vgroupa_id = Vattach(file_id,vgroupa_ref,"w");
  if(vgroupa_id == FAIL) {
    printf("fail to attach groupa.\n");
    return FAIL;
  }
  vgroupb_id = Vattach(file_id,vgroupb_ref,"w");
  if(vgroupb_id == FAIL) {
    printf("fail to attach groupb.\n");
    return FAIL;
  }
  
  istat=Vsetname(vgroupa_id,"groupA");
  if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }

  istat=Vsetname(vgroupb_id,"groupB");
   if(istat == FAIL) {
    printf("fail to set name for groupb.\n");
    return FAIL;
  }
 
  gr_id = GRstart(file_id);
 
  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }

  /* Define the location, pattern, and size of the data set */
  for (i = 0; i < 2; i++) {
    start[i] = 0;
    edges[i] = dim_sizes[i];
  }
  /* Define the number of components and dimensions of the image. */
  ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
    
  /* Create the array. */
  /* 1.1. put one image with the name "Imagea" under groupa.*/
  ri_id = GRcreate(gr_id, "Imagea", ncomp, DFNT_UINT32, il, dim_sizes);
  if(ri_id == FAIL) {
    printf("fail to create GR interface.\n");
    return FAIL;
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data32);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  gr_ref = GRidtoref(ri_id);
  if(gr_ref == FAIL) {
    printf("fail to convert ri_id into reference.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupa_id,DFTAG_RIG,gr_ref);
  if(istat == FAIL) {
    printf("fail to add gr object into vgroup a.\n");
    return FAIL;
  }
  GRendaccess(ri_id);

  /* 1.2 putting the same same  image object under groupa. */
  /* Create the array. */
  ri_id = GRcreate(gr_id, "Imagea", ncomp, DFNT_UINT32, il, dim_sizes);
  if(ri_id == FAIL) {
    printf("fail to create GR interface.\n");
    return FAIL;
  }
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data32);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  gr_ref = GRidtoref(ri_id);
   if(gr_ref == FAIL) {
    printf("fail to convert ri_id into reference.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupa_id,DFTAG_RIG,gr_ref);
   if(istat == FAIL) {
    printf("fail to add gr object into vgroup a.\n");
    return FAIL;
  }
  GRendaccess(ri_id);


  /* 2.0 no image name is given, it is illegal for hdf4 lib; therefore;
     no test cases are given.  */
 

  /* 3.1 another image with the "imagea" under groupb is given.*/
  ri_id = GRcreate(gr_id,"imagea", ncomp, DFNT_UINT32, il, dim_sizes);
  if(ri_id == FAIL) {
    printf("fail to create GR interface.\n");
    return FAIL;
  }
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data32);
   if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  gr_ref = GRidtoref(ri_id);
   if(gr_ref == FAIL) {
    printf("fail to convert ri_id into reference.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupb_id,DFTAG_RIG,gr_ref);
  if(istat == FAIL) {
    printf("fail to add gr object into vgroup a.\n");
    return FAIL;
  }
  GRendaccess(ri_id);
  
  /* 4.0 two images share the same name under no specific groups.*/
 
  ri_id = GRcreate(gr_id, "Image_independent", ncomp, DFNT_UINT32, il, dim_sizes);

   if(ri_id == FAIL) {
    printf("fail to create GR interface.\n");
    return FAIL;
  }
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data32);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }
  GRendaccess(ri_id);
  
  ri_id = GRcreate(gr_id, "Image_independent", ncomp, DFNT_UINT32, il, dim_sizes);
  if(ri_id == FAIL) {
    printf("fail to create GR interface.\n");
    return FAIL;
  }
  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_data32);
  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }							 
  GRendaccess(ri_id);
  GRend(gr_id);
  istat = Vdetach(vgroupa_id);
  istat = Vdetach(vgroupb_id);
 
  istat = Vend(file_id);
  istat = Hclose(file_id);
  return 0;
}
/* a routine that generates the name clashing for vdata object. some 
   redundant codes can found in this routine, it should be corrected later.
   
    Cases:

     1. two independent Vdata with the same name not under any user-specified group.
     2. vdata without any names and not under any user-specified groups.
     3. different Vdata objects under the same vgroup sharing the same name.
     4. different Vdata objects under different vgroups sharing the same name.
     5. Vdata without any names under one user-specified group*/  

int test_vdnameclash() {

  int32	  file_id, vgroupa_ref, vgroupa_id,vgroupb_ref,vgroupb_id;
  int32   vgroupc_id,vgroupc_ref,vd_ref;
  
  struct {
    float32      temp;
    int16        height;
  } source[NRECORDS];

  int32  vdata_id, istat;
  uint8  *databuf, *pntr;
  int    i, bufsize, recsize;
  VOIDP  fldbufpt[2];

  /* Open the HDF file. */
  file_id = Hopen(FILECLASHVD, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open the file.\n");
    return FAIL;
  }
  /* Initialize the Vset interface. */
  istat = Vstart(file_id);
   if(istat == FAIL) {
     printf("fail to start vgroup interface.\n");
     return FAIL;
  }

  /* 1.0
     Create an independent new Vdata with the name "Test Vset Name". */
  
  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata id.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }

  /* Set the Vdata name. */
  istat=VSsetname(vdata_id, "Test Vset Name");
  if(istat == FAIL) {
    printf("fail to set vdata name.\n");
    return FAIL;
  }

  /* Set the Vdata class. */
  istat=VSsetclass(vdata_id, "Test Vset Class");
  if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
  }

  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
  if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("fail to allocate memory for databuf");
    return FAIL;
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }      
 
  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
   printf("fail to write vdata.\n");
   return FAIL;
  }

  free(databuf);
  VSdetach(vdata_id);
 
  /* 1.1 
     Create another independent Vdata with the name "Test Vset Name" */

  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }

  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }
  /* Set the Vdata name. */
  istat=VSsetname(vdata_id, "Test Vset Name");
  if(istat == FAIL) {
    printf("fail to set vdata name.\n");
    return FAIL;
  }
  /* Set the Vdata class. */
  istat=VSsetclass(vdata_id, "Test Vset Class");
  if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
  }

  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
  if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
    return FAIL;
  }
  free(databuf);
  VSdetach(vdata_id);

  /* An independent Vdata without name not under any user-specified vgroup. */
  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata id.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }
  /* Set the Vdata class. 
     istat=VSsetclass(vdata_id, "Test Vset Class");*/
   if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
   }
  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
  if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
   printf("fail to write vdata.\n");
   return FAIL;
  }
  free(databuf);
  VSdetach(vdata_id);
 
  vgroupa_ref = -1;
  vgroupb_ref = -1;
  vgroupc_ref = -1;

  /* 3. two vdata with the same name under groupa. */
  vgroupa_id = Vattach(file_id, vgroupa_ref,"w");
  
  Vsetname(vgroupa_id,"groupA");
  /* Create a new Vdata. */
  vdata_id = VSattach(file_id, -1, "w");
     
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }
  /* Set the Vdata name. */
  istat=VSsetname(vdata_id, "Test Vset Name");
  if(istat == FAIL) {
    printf("fail to set vdata name. \n");
    return FAIL;
  }

  /* Set the Vdata class. */
  istat=VSsetclass(vdata_id, "Test Vset Class");
  if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
   }
  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
  if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;    
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE);
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
   return FAIL;
  }
  free(databuf); 
  vd_ref = VSQueryref(vdata_id);
  if(vd_ref == FAIL) {
    printf("fail to query vdata.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupa_id,DFTAG_VH,vd_ref);   
  if(istat == FAIL){
    printf("fail to add vdata into vgroup.\n");
    return FAIL;
  }
  VSdetach(vdata_id);
     
  /* Create a new Vdata. */
  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }

  /* Set the Vdata name. */
  istat=VSsetname(vdata_id, "Test Vset Name");
   if(istat == FAIL) {
    printf("fail to set vdata name.\n");
    return FAIL;
  }
  /* Set the Vdata class. */
  istat=VSsetclass(vdata_id, "Test Vset Class");
  if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
   }
  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
  if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;
    
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
    return FAIL;
  }
  free(databuf);
  vd_ref = VSQueryref(vdata_id);
  if(vd_ref== FAIL) {
    printf("fail to query reference.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupa_id,DFTAG_VH,vd_ref);  
  if(istat == FAIL) {
    printf("fail to add vdata into vgroup a.\n");
    return FAIL;
  }
  VSdetach(vdata_id);
    

  /*4. another vdata with the same name under groupb.*/
  vgroupb_id = Vattach(file_id,vgroupb_ref,"w");
  if(vgroupb_id == FAIL) {
    printf("fail to attach vgroup b.\n");
    return FAIL;
  }

  istat=Vsetname(vgroupb_id,"groupB");
  if(istat == FAIL) {
    printf("fail to set group b name.\n");
    return FAIL;
  }
  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }
  /* Set the Vdata name. */
  istat=VSsetname(vdata_id, "Test Vset Name");
   if(istat == FAIL) {
    printf("fail to set vdata name.\n");
    return FAIL;
  }
  /* Set the Vdata class. */
  istat=VSsetclass(vdata_id, "Test Vset Class");
  if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
   }
  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
   if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  { 
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
    return FAIL;
  }

  free(databuf);
  vd_ref = VSQueryref(vdata_id);
  if(vd_ref == FAIL) {
    printf("fail to query reference.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupb_id,DFTAG_VH,vd_ref); 
  if(istat == FAIL) {
    printf("fail to add vdata into vgroup.\n");
    return FAIL;
  }

  VSdetach(vdata_id);

  /* 5. vdata without name under groupc. */
  vgroupc_id = Vattach(file_id,vgroupc_ref,"w");
  if(vgroupc_id == FAIL) {
    printf("fail to attach vgroup c.\n");
    return FAIL;
  }
  istat = Vsetname(vgroupc_id,"groupC");
  if(istat == FAIL) {
    printf("fail to set group c name.\n");
    return FAIL;
  }
  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata.\n");
    return FAIL;
  }
  /* Define the field to write. */
  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
  if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }
  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_VDNAMES);
  if(istat == FAIL) {
    printf("fail to set field name.\n");
    return FAIL;
  }

  recsize = sizeof(float32) + sizeof(int16); 

  bufsize = recsize * NRECORDS;
  databuf = (uint8 *) malloc((size_t)bufsize);
  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;
  }
  pntr = databuf;
  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
        
  }
  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }        

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
    return FAIL;
  }

  free(databuf);
  vd_ref = VSQueryref(vdata_id);
  if(vd_ref == FAIL) {
    printf("fail to get reference number for vata.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupc_id,DFTAG_VH,vd_ref); 
  if(istat == FAIL) {
    printf("fail to add vdata into groupc.\n");
    return FAIL;
  }
  VSdetach(vdata_id);

  istat = Vdetach(vgroupa_id);
  istat = Vdetach(vgroupb_id);
  istat = Vdetach(vgroupc_id);
  istat = Vend(file_id);
  istat = Hclose(file_id);
  return 0;
}
  
/* a routine that generates a hdf file with vgroup loops.va->vb and vb->va */

int test_vgloop() {

  int32	  file_id, vgroupa_ref, vgroupa_id, vgroupb_ref,vgroupb_id;
  int32	  istat,dims[TYP_RANK];
  int i,j,k;

  /*2. for sds */
  int32   sd_id,sds_id;
  int32   sds_ref;
  int32   array_data[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32   start[TYP_RANK],edges[TYP_RANK],stride[TYP_RANK];


  for (i=0;i<TYP_DIMSIZE;i++){
    for(j=0;j<TYP_DIMSIZE;j++) {
      for(k=0;k<TYP_DIMSIZE;k++){
	array_data[i][j][k] =i+j;
      }
    }
  }

  dims[0] = TYP_DIMSIZE;
  dims[1] = TYP_DIMSIZE;
  dims[2] = TYP_DIMSIZE;
  for (i=0;i<TYP_RANK;i++){
    stride[i]=1;
    start[i]=0;
    edges[i]=dims[i];

  }

  /* Open the HDF file. */

  file_id = Hopen(FILELOOP, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open the file.\n");
    return FAIL;
  }
  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start vdata interface.\n");
    return FAIL;
  }

  vgroupa_ref = -1;
  vgroupb_ref = -1;
  vgroupa_id = Vattach(file_id, vgroupa_ref, "w");
  if(vgroupa_id == FAIL) {
    printf("fail to attach group a.\n");
    return FAIL;
  }
  vgroupb_id = Vattach(file_id, vgroupb_ref, "w");
  if(vgroupb_id == FAIL) {
    printf("fail to attach groupb.\n");
    return FAIL;
  }
  istat = Vsetname(vgroupa_id,"groupA");
   if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }
  istat = Vsetname(vgroupb_id,"groupB");
   if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }
  istat = Vinsert(vgroupa_id,vgroupb_id);
  if(istat == FAIL) {
    printf("fail to insert groupb into groupa.\n");
    return FAIL;
  }

  istat = Vinsert(vgroupb_id,vgroupa_id);
  if(istat == FAIL) {
    printf("fail to insert groupa into groupb.\n");
    return FAIL;
  }
  sd_id = SDstart(FILELOOP,DFACC_WRITE);
  if(sd_id == FAIL) {
    printf("fail to start sd interface.\n");
    return FAIL;
  }

  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,TYP_RANK,dims);
  if(sds_id == FAIL) {
    printf("fail to create sds interface.\n");
    return FAIL;
  }

  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("fail to write sds data.\n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("fail to create sds reference.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupa_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add sds into vgroupa\n");
    return FAIL;
  }
  SDendaccess(sds_id);
  SDend(sd_id);
  istat = Vdetach(vgroupa_id);
  istat = Vdetach(vgroupb_id);
  istat = Vend(file_id);
  istat = Hclose(file_id);
  return 0;
}

/* a routine that creates a hdf file which has one sds under two
   different vgroups. */
int test_vghl() {

  int32	  file_id, vgroupa_ref, vgroupa_id, vgroupb_ref,vgroupb_id;
  int32	  istat,dims[TYP_RANK];
  int    i,j,k;

  /*2. for sds */
  int32   sd_id,sds_id;
  int32   sds_ref;
  int32   array_data[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32   start[TYP_RANK],edges[TYP_RANK],stride[TYP_RANK];

  for (i=0;i<TYP_DIMSIZE;i++){
    for(j=0;j<TYP_DIMSIZE;j++) {
      for(k=0;k<TYP_DIMSIZE;k++){
	array_data[i][j][k] =i+j;
      }
    }
  }

  dims[0] = TYP_DIMSIZE;
  dims[1] = TYP_DIMSIZE;
  dims[2] = TYP_DIMSIZE;
  for (i=0;i<TYP_RANK;i++){
    stride[i]=1;
    start[i]=0;
    edges[i]=dims[i];

  }

  /* Open the HDF file. */

  file_id = Hopen(FILEHL, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open hdf file.\n");
    return FAIL;
  }

  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start vdata interface.\n");
    return FAIL;
  }
  vgroupa_ref = -1;
  vgroupb_ref = -1;
  vgroupa_id = Vattach(file_id, vgroupa_ref, "w");
   if(vgroupa_id == FAIL) {
    printf("fail to attach group a.\n");
    return FAIL;
  }
  vgroupb_id = Vattach(file_id, vgroupb_ref, "w");
  if(vgroupb_id == FAIL) {
    printf("fail to attach groupb.\n");
    return FAIL;
  }
  istat = Vsetname(vgroupa_id,"groupA");
  if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }
  istat = Vsetname(vgroupb_id,"groupB");
  if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }
  sd_id = SDstart(FILEHL,DFACC_WRITE);
  if(sd_id == FAIL) {
    printf("fail to start sd interface.\n");
    return FAIL;
  }
  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,TYP_RANK,dims);
  if(sds_id == FAIL) {
    printf("fail to create sds interface.\n");
    return FAIL;
  }

  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("fail to write sds data.\n");
    return FAIL;
  }
  sds_ref = SDidtoref(sds_id);
   if(sds_ref == FAIL) {
    printf("fail to create sds reference.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupa_id,DFTAG_NDG,sds_ref);
   if(istat == FAIL) {
    printf("fail to add sds into vgroupa\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupb_id,DFTAG_NDG,sds_ref);
   if(istat == FAIL) {
    printf("fail to add sds into vgroupb\n");
    return FAIL;
  }
  SDendaccess(sds_id);
  SDend(sd_id);
  istat = Vdetach(vgroupa_id);
  istat = Vdetach(vgroupb_id);
  istat = Vend(file_id);
  istat = Hclose(file_id);
  return 0;
}
/* this is a comprehensive testing file for h4-h5 converter. It includes
   annotation,vdata,image,sds,palette and vgroup objects*/
/* the structure of the hdf5 file is as follows:
            root
            /  \
           gc  ga<--
           /    \  |
          gd    gb--
          /
	 ge

     This file includes vdata,image and sds.          

*/
int test_vgall() {

  typedef struct {
    float32      temp;
    int16        height;
    float32      speed;
    char         ident;
    float32      position[2];
  } source_t;

  source_t source[NRECORDS];
	
  int32	  file_id, vgroupa_ref, vgroupa_id,istat;
  int32   vgroupc_ref, vgroupc_id,vgroupb_ref,vgroupb_id;
  int32   vgroupd_ref, vgroupd_id,vgroupe_ref,vgroupe_id;
  int32   vdata_id,values[4]={32, 16, 32, 8};

  intn    i, j,k;

  uint8   *databuf, *pntr;
  int     bufsize, recsize;
  VOIDP   fldbufpt[5];

  /*2. for sds */
  int32   sd_id,sds_id,dim_id;
  int32   sds_ref;
  int32   fill_value;
  int32   array_data[TYP_DIMSIZE][TYP_DIMSIZE][TYP_DIMSIZE];
  int32   dimo_sca[TYP_DIMSIZE];
  int32   dim_sizes[TYP_RANK];
  int32   start[TYP_RANK],edges[TYP_RANK],stride[TYP_RANK];
  float64 cal;
  float64 cal_err;
  float64 offset;
  float64 offset_err;

  /*3. for image. */

  int32   gr_id, ri_id, il,pal_id;
  int32   image_ncomp,image_data_type;
  int32   image_start[2], image_edges[2],image_dims[2];
  int16   image_data[Y_LENGTH][X_LENGTH][3];
  uint8   palette_data[NUM_COLORS*3];
  int32   image_num_comp;
  int32   image_num_entries;
  int32   image_ref;

  char*   attr_value;

  /*4. for annotation. */
  int32 an_id,ann_id;
  int32 group_tag,group_ref;

  static char file_label[] = "This is a file label.";
  static char file_desc[] =  "This is a file description.";
  static char data_label[] = "This is a data label.";
  static char data_desc[] =  "This is a data description.";

  char label[]   = "sds.label";
  char unit[]    = "sds.unit";
  char format[]  = "sds.format";
  char coordsys[] = "sds.coord";

  char dim_name0[] ="dim0";
  char dim_name1[] ="dim1";
  char dim_label[] ="dim.label";
  char dim_unit[] ="dim.unit";
  char dim_format[] ="dim.format";

  /**** initial setting. ****/
  cal = 1.0;
  cal_err = 0.0;
  offset = 0.0;
  offset_err = 0.0;
  fill_value = 999;

  attr_value = malloc(ATT_SIZE*sizeof(char));
  if(attr_value == NULL) {
    printf("fail to allocate memory.\n");
    return FAIL;
  }
  strcpy(attr_value,"test attr");

  for (i=0;i<TYP_DIMSIZE;i++){
    for(j=0;j<TYP_DIMSIZE;j++) {
      for(k=0;k<TYP_DIMSIZE;k++) {
	array_data[i][j][k]=i+j;
      }
    }
  }

  for (i=0;i<TYP_DIMSIZE;i++) 
    dimo_sca[i]= 2*i;

  for (i=0;i<TYP_RANK;i++){
    stride[i]=1;
    start[i]=0;
    dim_sizes[i] = TYP_DIMSIZE;
    edges[i]=dim_sizes[i];

  }
  
  /* Open the HDF file. */

  file_id = Hopen(FILEVG, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open the file.\n");
    return FAIL;
  }
  
  /* create annotations. */

  /* Initialize the AN interface and obtain an interface id. */
  an_id = ANstart(file_id);
  if(an_id == FAIL) {
    printf("fail to start annotation interface.\n");
    return FAIL;
  }
  /* Create a file label and obtain an annotation id. */
  ann_id = ANcreatef(an_id, AN_FILE_LABEL);
  if(ann_id == FAIL) {
    printf("fail to create file annotion interface.\n");
    return FAIL;
  }

  /* Write the file label to the file. */
  istat = ANwriteann(ann_id, file_label, (int32)(strlen(file_label)));
  if(istat == FAIL) {
    printf("fail to write file annotation.\n");
    return FAIL;
  }

  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);

  /* Create a file description.  */
  ann_id = ANcreatef(an_id, AN_FILE_DESC);
  if(ann_id == FAIL) {
    printf("fail to create file  annotion interface.\n");
    return FAIL;
  }
  /* Write the file description to the file. */
  istat = ANwriteann(ann_id, file_desc, (int32)(strlen(file_desc)));
   if(istat == FAIL) {
    printf("fail to write file annotation.\n");
    return FAIL;
  }
  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);

  /* Initialize the Vset interface. */

  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start vdata interface.\n");
    return FAIL;
  }
  /** Create Vgroup VA and VB***/
  vgroupa_ref = -1;
  vgroupb_ref = -1;

  /* Attach to the target vgroup. */
  vgroupa_id = Vattach(file_id, vgroupa_ref, "w");
  if(vgroupa_id == FAIL) {
    printf("fail to attach group a.\n");
    return FAIL;
  }
  istat=Vsetname(vgroupa_id,"groupA");
  if(istat == FAIL) {
    printf("fail to set name for groupa.\n");
    return FAIL;
  }
  /* Attach an attribute to the vgroup. */

  istat = Vsetattr(vgroupa_id,"vgattr",DFNT_CHAR,5,"TESTa");
  if(istat == FAIL) {
    printf("fail to set attribute.\n");
    return FAIL;
  }

  vgroupb_id = Vattach(file_id,vgroupb_ref,"w");
  if(vgroupb_id == FAIL) {
    printf("fail to attach group b.\n");
    return FAIL;
  }
  Vsetname(vgroupb_id,"groupB");
  if(istat == FAIL) {
    printf("fail to set name for groupb.\n");
    return FAIL;
  }
  /* adding annotation into the file. */
  group_tag = VQuerytag(vgroupb_id);
  if(group_tag == FAIL) {
    printf("query groupb tag failed.\n");
    return FAIL;
  }
  group_ref = VQueryref(vgroupb_id);
  if(group_ref == FAIL) {
    printf("query groupb ref failed.\n");
    return FAIL;
  }

  /* Create a data label for the Vgroup just created.  */
  ann_id = ANcreate(an_id, (uint16)group_tag,(uint16)group_ref, AN_DATA_LABEL);
  if(ann_id == FAIL) {
    printf("AN create interface failed.\n");
    return FAIL;
  }

  /* Write the data label to the file. */
  istat = ANwriteann(ann_id, data_label, (int32)(strlen(data_label)));
  if(istat == FAIL) {
    printf("fail to write annotation.\n");
    return FAIL;
  }
  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);

  /* Create a data description for the Vgroup just created.*/
  ann_id = ANcreate(an_id, (uint16)group_tag, (uint16)group_ref, AN_DATA_DESC);
  if(ann_id == FAIL) {
    printf("AN create interface failed.\n");
    return FAIL;
  }
  /* Write the data description to the file. */
  istat = ANwriteann(ann_id, data_desc, (int32)(strlen(data_desc)));
  if(istat == FAIL) {
    printf("fail to write annotation.\n");
    return FAIL;
  }
  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);

  /* Terminate access to the AN interface. */
  istat = ANend(an_id);
  /* end of annotations. */
 
  istat = Vsetattr(vgroupb_id, VGATTR_NAME, DFNT_CHAR, 5, "TESTb");
  if(istat == FAIL) {
    printf("fail to set attribute.\n");
    return FAIL;
  }
  istat = Vinsert(vgroupa_id,vgroupb_id);
  if(istat == FAIL) {
    printf("fail to insert groupb into groupa. \n");
    return FAIL;
  }
     
  vgroupc_ref = -1;
  vgroupd_ref = -1;
  vgroupe_ref = -1;
  vgroupc_id = Vattach(file_id,vgroupc_ref,"w");
  if(vgroupc_id == FAIL) {
    printf("fail to attach groupc into file.\n");
    return FAIL;
  }

  istat= Vsetname(vgroupc_id,"groupA");
  if(istat == FAIL) {
    printf("fail to set name for groupA.\n");
    return FAIL;
  }

  vgroupd_id = Vattach(file_id,vgroupd_ref,"w");
  if(vgroupd_id == FAIL) {
    printf("fail to attach groupd into the file.\n");
    return FAIL;
  }

  istat= Vsetname(vgroupd_id,"groupD");
   if(istat == FAIL) {
    printf("fail to set name for groupD.\n");
    return FAIL;
  }
  vgroupe_id = Vattach(file_id,vgroupe_ref,"w");
  if(vgroupe_id == FAIL) {
    printf("fail to attach groupe into the file.\n");
    return FAIL;
  }
  istat=Vsetname(vgroupe_id,"groupE");
  if(istat == FAIL) {
    printf("fail to set name for groupD.\n");
    return FAIL;
  }
  istat = Vinsert(vgroupc_id,vgroupd_id);
  if(istat == FAIL) {
    printf("fail to insert groupd into groupc.\n");
    return FAIL;
  }

  istat = Vinsert(vgroupd_id,vgroupe_id);
  if(istat == FAIL) {
     printf("fail to insert groupe into groupd.\n");
    return FAIL;
  }
  istat = Vinsert(vgroupe_id,vgroupc_id);
  if(istat == FAIL) {
     printf("fail to insert groupc into groupe.\n");
    return FAIL;
  }
  /* Create a new Vdata. */

  vdata_id = VSattach(file_id, -1, "w");
   if(vdata_id == FAIL) {
    printf("fail to attach vdata.\n");
    return FAIL;
  }
  /* Define the field to write. */

  istat = VSfdefine(vdata_id, FIELD_1, DFNT_FLOAT32, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 1.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_2, DFNT_INT16, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 2.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_3, DFNT_FLOAT32, 1); 
   if(istat == FAIL) {
    printf("fail to define vdata field 3.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_4, DFNT_CHAR8, 1);  
   if(istat == FAIL) {
    printf("fail to define vdata field 4.\n");
    return FAIL;
  }
  istat = VSfdefine(vdata_id, FIELD_5, DFNT_FLOAT32,2);
   if(istat == FAIL) {
    printf("fail to define vdata field 5.\n");
    return FAIL;
  }
  /* Set the Vdata name. */
  istat = VSsetname(vdata_id, "Example Vset Name");
   if(istat == FAIL) {
    printf("fail to set vdata name.\n");
    return FAIL;
  }
  /* Set the Vdata class. */
  istat = VSsetclass(vdata_id, "Example Vset Class");
   if(istat == FAIL) {
    printf("fail to set vdata class.\n");
    return FAIL;
   }
  /* Set the field names. */
  istat = VSsetfields(vdata_id, FIELD_NAMES);
  if(istat == FAIL) {
    printf("fail to set fields.\n");
    return FAIL;
  }
  recsize = (2 * sizeof(float32) + sizeof(int16)) +2* sizeof(float32) 
    +sizeof(char);
  /********************debugging *********************/
  /*     + sizeof(char));  */

  bufsize = recsize * NRECORDS;

  databuf = (uint8 *) malloc((size_t)bufsize);

  if (databuf == NULL) {
    printf("malloc failed\n");
    return FAIL;
  }
  pntr = databuf;

  /* set record values */
  for (i = 0; i < NRECORDS; i++) {
    source[i].temp = 1.11 * (i+1);
    source[i].height = i;
    source[i].speed = 1.11 * (i+1);
    source[i].ident = 'A' + i; 
    for (j=0; j< 2; j++) 
      source[i].position[j] = 1.0+j;

  }

  /* pack one record at a time */
  for (i = 0; i< NRECORDS; i++) {
    /* set field buf address */
    fldbufpt[0] = &source[i].temp;
    fldbufpt[1] = &source[i].height;
    fldbufpt[2] = &source[i].speed;
    fldbufpt[3] = &source[i].ident; 
    fldbufpt[4] = &source[i].position[0];
    /* pack field data into databuf */
    istat = VSfpack(vdata_id,_HDF_VSPACK,NULL,(VOIDP)pntr,
		    recsize, 1, NULL, fldbufpt);
    if (istat == FAIL)  {
      printf("VSfpack failed in packing record %d\n", i);
      return FAIL;
    }
    pntr = pntr + recsize; 
  }      

  /* Write the data to the Vset object. */
  istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE); 
  if(istat == FAIL) {
    printf("fail to write vdata.\n");
    return FAIL;
  }
  /* Set Vdata attribute */
  istat = VSsetattr (vdata_id, _HDF_VDATA, "Fixed_type", DFNT_INT32, 4, 
		     (VOIDP)values);
  if(istat == FAIL) {
    printf("fail to set vdata attribute.\n");
    return FAIL;
  }
  /* Set attribute for "speed" field */
  istat = VSsetattr (vdata_id, 2, "Symbol", DFNT_CHAR, 3, "MAX");
  if(istat == FAIL) {
    printf("fail to set speed attribute.\n");
    return FAIL;
  }
 
  istat = VSdetach(vdata_id);
  if(istat == FAIL) {
    printf("fail to detach vdata.\n");
    return FAIL;
  }
  
  /*** initialize another empty vdata set to test name clashings ***/

  vdata_id = VSattach(file_id, -1, "w");
  if(vdata_id == FAIL) {
    printf("fail to attach vdata_id into file_id.\n");
    return FAIL;
  }

  /* Set the Vdata name. */

  istat = VSsetname(vdata_id, "Example Vset Name");
  if(istat == FAIL) {
    printf("fail to set vdata name.\n");
    return FAIL;
  }
  istat = VSdetach(vdata_id);
  if(istat == FAIL) {
    printf("fail to detach vdata.\n");
    return FAIL;
  }

  /* Initialize an independent Image  interface. */
  /************************************************/
  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }
  image_ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  image_dims[0] = X_LENGTH;
  image_dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_1", image_ncomp, DFNT_INT16, il, 
		   image_dims);
  if(ri_id == FAIL) {
    printf("fail to create GR object.\n");
    return FAIL;
  }

  /* Fill the stored-data array with values. */

  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data[j][i][0] = (i + j) + 1;
      image_data[j][i][1] = (i + j) + 1;
      image_data[j][i][2] =(i+j)+1;
    }
  }

  /* Define the location, pattern, and size of the data set */

  for (i = 0; i < 2; i++) {
    image_start[i] = 0;
    image_edges[i] = image_dims[i];
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, image_start, NULL, image_edges, 
		       (VOIDP)image_data);

  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  /* Initialize the palette to grayscale. */
  for (i = 0; i < NUM_COLORS; i++) {
    palette_data[i * 3] = i;
    palette_data[i * 3 + 1] = i;
    palette_data[i * 3 + 2] = i;
  }
	
  /* Set palette characteristics. */

  image_data_type   = DFNT_UINT8;
  image_num_entries = NUM_COLORS;
  image_num_comp    = 3;

	
  /* Get the id for the palette. */
  pal_id = GRgetlutid(ri_id,0 );
  if(pal_id == FAIL) {
    printf("fail to get palette id.\n");
    return FAIL;
  }

  /* Write the palette to file. */
  istat  = GRwritelut(pal_id, image_num_comp, image_data_type, 
		      0, image_num_entries,(VOIDP)palette_data);
  if(istat == FAIL) {
    printf("fail to write palette.\n");
    return FAIL;
  }
  image_ref = GRidtoref(ri_id);
  if(image_ref == FAIL) {
    printf("fail to get reference of image.\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  istat = GRendaccess(ri_id);

  /* Terminate access to the GR interface. */
  istat  = GRend(gr_id);

    
  /*** III. setting SDS data and all SDS attributes. ***/

  sd_id = SDstart(FILEVG,DFACC_WRITE);

  if (sd_id == FAIL){
    printf (" fail to open sd interface. \n");
    return FAIL;
  }

  istat = SDsetfillmode(sd_id,SD_NOFILL);

  if (istat == FAIL) {
    printf("error setting fill mode\n");
    return FAIL;
  }
  sds_id = SDcreate(sd_id,"sds",DFNT_INT32,3,dim_sizes);
  if(sds_id == FAIL) {
    printf("fail to create sds objects.\n");
    return FAIL;
  }

  istat = SDsetfillvalue(sds_id,(VOIDP)(&fill_value));
 
  if (istat == FAIL){
    printf("error setting fill value\n");
    return FAIL;
  }

  /*** 3.1 write data. ***/

  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);

  if(istat == FAIL) {
    printf(" sds data written failed. \n");
    return FAIL;
  }
  /*** 3.2  write dataset attribute ***/  
  
  istat = SDsetattr(sds_id,"sds.attr",DFNT_CHAR8,10,(VOIDP)attr_value);
  if(istat == FAIL) {
    printf(" sds data attr. setting failed. \n");
    return FAIL;
  }
  /*** 3.3 write dataset predefined attribute ***/

  istat = SDsetdatastrs(sds_id,label,unit,format,coordsys);
  if(istat == FAIL) {
    printf(" sds data predefined attr. setting failed. \n");
    return FAIL;
  }
  /*** 3.4 set calibration information. ***/

  istat = SDsetcal(sds_id,cal,cal_err,offset,offset_err,DFNT_INT32);
  if(istat == FAIL) {
    printf(" sds data calibrating attr. setting failed. \n");
    return FAIL;
  }

  /*** 3.5 write down dimension scale dataset. ***/

  for (i =0; i<TYP_RANK;i++) {
    dim_id = SDgetdimid(sds_id,i);
    if(dim_id <0) {
      printf("sds set dim id failed. \n");
      return FAIL;
    }
    if (i==0) {
      istat = SDsetdimname(dim_id,dim_name0);
      if(istat == FAIL) {
	printf("sds set dim.name failed. \n");
	return FAIL;
      }
      istat= SDsetdimscale(dim_id,dim_sizes[0],DFNT_INT32,(VOIDP)dimo_sca);
      if(istat == FAIL) {
	printf("sds set dim. scale failed. \n");
	return FAIL;
      }
    }
    else {
      istat = SDsetdimname(dim_id,dim_name1);
      if(istat == FAIL) {
	printf("sds set dim.name failed. \n");
	return FAIL;
      }
      istat = SDsetdimscale(dim_id,dim_sizes[1],DFNT_INT32,(VOIDP)dimo_sca);
      if(istat == FAIL) {
	printf("sds set dim. scale failed. \n");
	return FAIL;
      }
    }
 
    istat=SDsetdimstrs(dim_id,dim_label,dim_unit,dim_format);
    if(istat == FAIL) {
      printf("sds set dim. predefined attr. failed. \n");
      return FAIL;
    }
    istat = SDsetattr(dim_id,"sdsdim.attr",DFNT_CHAR8,10,(VOIDP)attr_value);

    if(istat == FAIL) {
      printf(" sds dim data attr. setting failed. \n");
      return FAIL;
    }
    SDendaccess(dim_id);
  }

  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("fail to transfer id to reference.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupb_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add reference into vgroup.\n");
    return FAIL;
  }

  SDendaccess(sds_id);
  SDend(sd_id);

  sd_id = SDstart(FILEVG,DFACC_WRITE);

  if (sd_id == FAIL){
    printf (" open file failed\n");
    return FAIL;
  }
  istat = SDsetfillmode(sd_id,SD_NOFILL);

  if (istat == FAIL){ 
    printf("error setting fill mode\n");
    return FAIL;
  }
  sds_id = SDcreate(sd_id,"sds2",DFNT_INT32,3,dim_sizes);
  if(sds_id == FAIL) {
    printf("fail to create sds id.\n");
    return FAIL;
  }
  istat = SDsetfillvalue(sds_id,(VOIDP)(&fill_value));
 
  if (istat == FAIL){
    printf("error setting fill value\n");
    return FAIL;
  }
  /*** 3.1 write data. ***/

  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);
  if(istat == FAIL) {
    printf("fail to write sds data.\n");
    return FAIL;
  }

  sds_ref = SDidtoref(sds_id);
  if(sds_ref == FAIL) {
    printf("fail to generate reference number.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupd_id,DFTAG_NDG,sds_ref);
  if(istat == FAIL) {
    printf("fail to add sds reference into groupd.\n");
    return FAIL;
  }

  SDendaccess(sds_id);
  SDend(sd_id);

  sd_id = SDstart(FILEVG,DFACC_WRITE);
  if (sd_id == FAIL) {
    printf (" open file failed\n");
    return FAIL;
  }

  istat = SDsetfillmode(sd_id,SD_NOFILL);

  if (istat == FAIL) {
    printf("error setting fill mode. \n");
    return FAIL;
  }
 
  sds_id = SDcreate(sd_id,"sds2",DFNT_INT32,3,dim_sizes);
  if(sds_id == FAIL) {
    printf("error creating sds objects.\n");
    return FAIL;
  }

  istat = SDsetfillvalue(sds_id,(VOIDP)(&fill_value));
 
  if (istat == FAIL) {
    printf("error setting fill value\n");
    return FAIL;
  }

  /*** 3.1 write data. ***/

  istat = SDwritedata(sds_id,start,stride,edges,(VOIDP)array_data);

  if(istat == FAIL) {
    printf("error writing sds object.\n");
    return FAIL;
  }

  SDendaccess(sds_id);
  SDend(sd_id);

  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("error starting gr interface.\n");
    return FAIL;
  }

  image_ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  image_dims[0] = X_LENGTH;
  image_dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_1", image_ncomp, DFNT_INT16, il, 
		   image_dims);

  if(ri_id == FAIL) {
    printf("fail to creat GR interface.\n");
    return FAIL;
  }

  /* Fill the stored-data array with values. */

  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data[j][i][0] = (i + j) + 2;
      image_data[j][i][1] = (i + j) + 3;
      image_data[j][i][2] =(i+j)+1;
    }
  }

  /* Define the location, pattern, and size of the data set */

  for (i = 0; i < 2; i++) {
    image_start[i] = 0;
    image_edges[i] = image_dims[i];
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, image_start, NULL, image_edges, 
		       (VOIDP)image_data);

  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  /* Initialize the palette to grayscale. */
  for (i = 0; i < NUM_COLORS; i++) {
    palette_data[i * 3] = i;
    palette_data[i * 3 + 1] = i;
    palette_data[i * 3 + 2] = i;
  }
	
  /* Set palette characteristics. */

  image_data_type   = DFNT_UINT8;
  image_num_entries = NUM_COLORS;
  image_num_comp    = 3;

	
  /* Get the id for the palette. */
  pal_id = GRgetlutid(ri_id,0 );

  if(pal_id == FAIL) {
    printf("fail to get palette id.\n");
    return FAIL;
  }

  /* Write the palette to file. */
  istat  = GRwritelut(pal_id, image_num_comp, image_data_type, 
		      0, image_num_entries,(VOIDP)palette_data);
  if(istat == FAIL) {
    printf("fail to write palette data.\n");
    return FAIL;
  }

  image_ref = GRidtoref(ri_id);
  if(image_ref == FAIL) {
    printf("fail to obtain image reference number.\n");
    return FAIL;
  }
  istat = Vaddtagref(vgroupd_id,DFTAG_RIG,image_ref);
  if(istat == FAIL) {
    printf("fail to add image into group vgroupd\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  istat = GRendaccess(ri_id);
  if(istat == FAIL) {
    printf("fail to end GR interface.\n");
    return FAIL;
  }

  /* Terminate access to the GR interface. */
  istat  = GRend(gr_id);

  gr_id = GRstart(file_id);
  if(gr_id == FAIL) {
    printf("fail to start GR interface.\n");
    return FAIL;
  }

  image_ncomp = 3;
  il = MFGR_INTERLACE_PIXEL;
  image_dims[0] = X_LENGTH;
  image_dims[1] = Y_LENGTH;

  /* Create the array. */
  ri_id = GRcreate(gr_id, "Image_2", image_ncomp, DFNT_INT16, il, 
		   image_dims);
  if(ri_id == FAIL) {
    printf("fail to create GR interface.\n");
    return FAIL;
  }

  /* Fill the stored-data array with values. */

  for (j = 0; j < Y_LENGTH; j++) {
    for (i = 0; i < X_LENGTH; i++) {
      image_data[j][i][0] = (i + j) + 2;
      image_data[j][i][1] = (i + j) + 3;
      image_data[j][i][2] =(i+j)+1;
    }
  }

  /* Define the location, pattern, and size of the data set */

  for (i = 0; i < 2; i++) {
    image_start[i] = 0;
    image_edges[i] = image_dims[i];
  }

  /* Write the stored data to the image array. */
  istat = GRwriteimage(ri_id, image_start, NULL, image_edges, 
		       (VOIDP)image_data);

  if(istat == FAIL) {
    printf("fail to write image.\n");
    return FAIL;
  }

  /* Initialize the palette to grayscale. */
  for (i = 0; i < NUM_COLORS; i++) {
    palette_data[i * 3] = i;
    palette_data[i * 3 + 1] = i;
    palette_data[i * 3 + 2] = i;
  }
	
  /* Set palette characteristics. */

  image_data_type   = DFNT_UINT8;
  image_num_entries = NUM_COLORS;
  image_num_comp    = 3;

	
  /* Get the id for the palette. */
  pal_id = GRgetlutid(ri_id,0 );
  if(pal_id == FAIL) {
    printf("fail to obtain palette id.\n");
    return FAIL;
  }

  /* Write the palette to file. */
  istat  = GRwritelut(pal_id, image_num_comp, image_data_type, 
		      0, image_num_entries,(VOIDP)palette_data);

  if(istat == FAIL) {
    printf("fail to write GR image.\n");
    return FAIL;
  }

  image_ref = GRidtoref(ri_id);
  if(image_ref == FAIL) {
    printf("fail to generate image reference number.\n");
    return FAIL;
  }

  istat = Vaddtagref(vgroupa_id,DFTAG_RIG,image_ref);
  if(istat == FAIL) {
    printf("fail to add image into vgroupa\n");
    return FAIL;
  }

  /* Terminate access to the image. */
  istat = GRendaccess(ri_id);

  /* Terminate access to the GR interface. */
  istat  = GRend(gr_id);

  istat =Vdetach(vgroupb_id);
  /* Detach from the vgroup, close the V interface and the file. */
  istat = Vdetach(vgroupa_id);
  istat = Vdetach(vgroupc_id);
  istat = Vdetach(vgroupd_id);
  istat = Vdetach(vgroupe_id);
  istat = Vend(file_id);
  
  istat = Hclose(file_id);
  return 0;
}
/* this routine creates annotation for four cases:

1. file label
2. file description
3. object label(vgroup)
4. object description(vgroup)

*/

int test_anno() {

  int32 file_id, an_id, ann_id, vgroup_id,istat;
  uint16 obj_tag, obj_ref;
  static char file_label[] = "This is a file label.";
  static char file_desc[] =  "This is a file description.";
  static char data_labelvg[] = "This is a vgroup data label.";
  static char data_descvg[] =  "This is a vgroup data description.";

  /* Create the HDF file. */
  file_id = Hopen(FILEANNO, DFACC_CREATE, 0);
  if(file_id == FAIL) {
    printf("fail to open HDF file \n");
    return FAIL;
  }
  /* Initialize the AN interface and obtain an interface id. */
  an_id = ANstart(file_id);
  if(an_id == FAIL) {
    printf("fail to start annotation interface.\n");
    return FAIL;
  }

  /* Create a file label and obtain an annotation id. */
  ann_id = ANcreatef(an_id, AN_FILE_LABEL);
  if(ann_id == FAIL) {
    printf("fail to create annotation id.\n");
    return FAIL;
  }

  /* Write the file label to the file. */
  istat = ANwriteann(ann_id, file_label, (int32)(strlen(file_label)));
  if(istat == FAIL) {
    printf("fail to write annotation.\n");
    return FAIL;
  }

  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);

  /* Create a file description.  */
  ann_id = ANcreatef(an_id, AN_FILE_DESC);
  if(ann_id == FAIL) {
    printf("fail to create annotation\n");
    return FAIL;
  }

  /* Write the file description to the file. */
  istat = ANwriteann(ann_id, file_desc, (int32)(strlen(file_desc)));
  if(istat == FAIL) {
    printf("fail to write annotation.\n");
    return FAIL;
  }

  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);

  /* Create a vgroup. */
  istat = Vstart(file_id);
  if(istat == FAIL) {
    printf("fail to start v interface.\n");
    return FAIL;
  }

  vgroup_id = Vattach(file_id, -1, "w");
  if(vgroup_id == FAIL) {
    printf("fail to attach vgroup \n");
    return FAIL;
  }

  istat = Vsetname (vgroup_id,  "Vgroup w/Annotations");
  if(istat == FAIL) {
    printf("fail to set group name\n");
    return FAIL;
  }
  /* Get reference number of vgroup just created. */
  obj_ref = Vfind (file_id, "Vgroup w/Annotations");
  if(obj_ref == 0) {
    printf("fail to set object reference.\n");
    return FAIL;
  }
  obj_tag = DFTAG_VG;

  /* Create a data label for the Vgroup just created.  */
  ann_id = ANcreate(an_id, (uint16)obj_tag, (uint16)obj_ref, AN_DATA_LABEL);
  if(ann_id == FAIL) {
    printf("fail to create annotation.\n");
    return FAIL;
  }
  /* Write the data label to the file. */
  istat = ANwriteann(ann_id, data_labelvg, (int)(strlen(data_labelvg)));
  if(istat == FAIL){
    printf("fail to write annotation.\n");
    return FAIL;
  }
  istat = ANendaccess(ann_id);

  obj_tag = DFTAG_VG;

  /* Create a data description for the Vgroup just created.*/
  ann_id = ANcreate(an_id, (uint16)obj_tag, (uint16)obj_ref, AN_DATA_DESC);
  if(ann_id == FAIL) {
    printf("fail to create annotation.\n");
    return FAIL;
  }

  /* Write the data description to the file. */
  istat = ANwriteann(ann_id, data_descvg, (int)(strlen(data_descvg)));
  if(istat == FAIL) {
    printf("fail to write annotation.\n");
    return FAIL;
  }
  /* Terminate access to the annotation. */
  istat = ANendaccess(ann_id);
  istat = Vdetach(vgroup_id);
  istat = Vend(file_id);
  /* Close the file. */

  /* Terminate access to the AN interface. */
  istat = ANend(an_id);   
  istat = Hclose(file_id);
  return 0;
}







