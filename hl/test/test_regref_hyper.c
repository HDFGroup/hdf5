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
/*
    This program shows how to create, store and dereference references
    to the dataset regions.

    It creates a file and writes a two dimensional integer dataset
    to it. Then it creates a dataset to store region references in. It
    stores references to a hyperslab and 3 points selected (for the
    integer dataset previously created).

    It then reopens the references dataset, reads and dereferences the
    region references, and then reads and displays the selected hyperslab
    and selected elements data from the integer dataset.
*/

#include <stdlib.h>
#include "h5hltest.h"
#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "FileA.h5"
#define dsetnamev "DS2"
#define dsetnamer "R1"

#define TESTING2(WHAT) {printf("%-70s", "Testing     " WHAT); fflush(stdout);}

int main(void)
{
    hid_t file_id;        /* file identifier */
    hid_t space_id;       /* dataspace identifiers */
    hid_t space_id1;       /* dataspace identifiers */
    hid_t space_id3;       /* dataspace identifiers */
    hid_t spacer_id;
    hid_t group_id, group_id1, group_id3;
    hid_t dsetv_id;       /*dataset identifiers*/
    hid_t dsetr_id;
    hsize_t dims[2] =  {9,8};
    hsize_t dimsr[1] =  {4};
    int rank = 2;
    int rank1D = 1;
    int rank3D = 3;
    hsize_t dims1D[1] = {17};
    hsize_t dims3D[3] = {6,6,6};
    int rankr =1;
    herr_t status;
    hdset_reg_ref_t ref[4];
    hdset_reg_ref_t ref_out[4];
    int data[9][8];
    int data_out[2][9] = {{0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0}};
    int data1D[17] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    int data3D[6][6][6];
    int rdata1[3][2];
    hsize_t start[2];
    hsize_t count[2];
    hsize_t start3D[3];
    hsize_t count3D[3];
    hsize_t coord[5] = {1,6,9,10,14};
    unsigned num_points = 5;
    int i, j, k;
    size_t name_size1, name_size2;
    char buf1[14], buf2[14];
    size_t nlength;
    int rank_out;
    hid_t dtype, sid, sid2;
    hid_t dtype_native;
    hsize_t *buf;
    char *name;
    size_t numelem;
    H5S_sel_type sel_type;
    int data_out2[3][2];
    int data_out3[2][2][2];
    int **rdata;
    hsize_t buf3[4];
    hsize_t dims2[2] = {3, 2};
    hid_t mem_space;
    hid_t dset;	
    hsize_t block_coord_3D[6] ={ 0, 1, 2, 1, 2, 3};
    int n1=2, n2=2;
    hsize_t block_coord[4] ={ 2, 2, 3, 3};
    int data_out2a[n1][n2];
    size_t      size;                  /*
				        * size of the data element
				        * stored in file
				        */
    hsize_t num_elem = 2;
    const char *path[num_elem];
    int  ranks[num_elem];
    hsize_t block_coord_6[6] ={ 0, 5, 0, 0, 2, 2};
    hsize_t block_coord_4[4] ={ 1, 2, 3, 4};
    hsize_t block_coord_1D_src[2] ={ 0, 5};
    hsize_t block_coord_1D_dest[2] ={ 5, 10};
    hsize_t block_coord_2D_src[4] ={ 0, 0, 1, 2};
    hsize_t block_coord_2D_dest[4] ={0, 0, 2, 1 };
    hdset_reg_ref_t ref6[num_elem];
    hdset_reg_ref_t ref_new;

    path[0] ="/Group_1D/DS1";
    path[1] ="/Group_2D/DS2";

    ranks[0] =1;
    ranks[1] =2;

    for (i=0; i<6; i++)
      for (j=0; j<6; j++)
	for (k=0; k<6; k++)
	  data3D[i][j][k] =  100*(i+1)+10*(j+1) + k + 1;

    printf("FULL 2D ARRAY:\n   /                          \\");
    for (i=0; i<9; i++)
      {
	printf("\n  |  ");
	for (j=0; j<8; j++) {
	  data[i][j] =  10*(i+1)+j+1;
	  printf("%d ", data[i][j]);
	}
	printf("  |");
      }
    printf("\n   \\                          /\n");
    printf("\n");

    /*
     * Create file with default file access and file creation properties.
     */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    spacer_id = H5Screate_simple(rankr, dimsr, NULL);

    /*
     * Create dataspace for datasets.
     */
    space_id = H5Screate_simple(rank, dims, NULL);
    group_id = H5Gcreate (file_id, "/Group_2D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(group_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data);
    status = H5Dclose(dsetv_id);

    /*
     * Create dataspace for 1D datasets.
     */
    space_id1 = H5Screate_simple(rank1D, dims1D, NULL);
    group_id1 = H5Gcreate (file_id, "/Group_1D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(group_id1, "DS1", H5T_NATIVE_INT, space_id1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data1D);
    status = H5Dclose(dsetv_id);

    /*
     * Create dataspace for 3D datasets.
     */
    space_id3 = H5Screate_simple(rank3D, dims3D, NULL);
    group_id3 = H5Gcreate (file_id, "/Group_3D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(group_id3, "DS3", H5T_NATIVE_INT, space_id3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data3D);
    status = H5Dclose(dsetv_id);


    /*
     * Dataset with references.
     */
    dsetr_id = H5Dcreate2(file_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a reference to elements selection.
     */
    status = H5Sselect_none(space_id1);
    status = H5Sselect_elements(space_id1, H5S_SELECT_SET, num_points, coord);
    status = H5Rcreate(&ref[0], group_id1, "DS1", H5R_DATASET_REGION, space_id1);

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 3;
    start[1] = 3;
    count[0] = 3;
    count[1] = 2;
    status = H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL);
    status = H5Rcreate(&ref[1], group_id, dsetnamev, H5R_DATASET_REGION, space_id);

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 0;
    start[1] = 7;
    count[0] = 9;
    count[1] = 2;
    status = H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL);
    status = H5Rcreate(&ref[2], group_id, dsetnamev, H5R_DATASET_REGION, space_id);


    /*
     * Create a reference to the hyperslab.
     */
    start3D[0] = 3;
    start3D[1] = 3;
    start3D[2] = 3;
    count3D[0] = 2;
    count3D[1] = 2;
    count3D[2] = 2;
    status = H5Sselect_hyperslab(space_id3, H5S_SELECT_SET, start3D, NULL, count3D, NULL);
    status = H5Rcreate(&ref[3], group_id3, "DS3", H5R_DATASET_REGION, space_id3);


    /*
     * Write dataset with the references.
     */
    status = H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref);

    /*
     * Close all objects.
     */
    status = H5Sclose(space_id);
    status = H5Sclose(spacer_id);
    status = H5Dclose(dsetr_id);
    status = H5Fclose(file_id);
    status = H5Gclose(group_id);

    /* Start Reading section */


    /*
     * Reopen the file to read selections back.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
    nlength = 0;
    rank_out = 0;

    /* set wrong object id */

/*     status = H5LRget_region_info(110101, */
/* 		    ref[1], */
/* 		    NULL, */
/* 		    &nlength, */
/* 		    &rank_out, */
/* 		    NULL, */
/* 		    NULL, */
/* 		    H5R_DATASET_REGION ); */

/*     status = H5LRget_region_info(-1, */
/* 				 NULL, */
/* 				 NULL, */
/* 				 &nlength, */
/* 				 &rank_out, */
/* 				 NULL, */
/* 				 NULL, */
/* 				 H5R_DATASET_REGION ); */


/*     if(status < 0 )  */
/*       printf(" Testing for incorrect object id, PASSED \n"); */

    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[1],
				 &nlength,
				 NULL,
				 &rank_out,
				 NULL,
				 &sel_type,
				 &numelem,
				 NULL);

    if(status < 0 ) 
      printf(" ERROR: H5LRget_region_info <1>\n");

    /* allocate the name */

    name = (char *) malloc (nlength);

    /* allocate space for hyperslab block
                            | NUMBLOCKS |   RANK    |  SIZEOF DATA TYPE | NUMBER COORDINATE POINTS | */
    buf = (hsize_t *)malloc(  numelem   * rank_out  *   sizeof(hsize_t) *          2                );

    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[1],
				 &nlength,
				 name,
				 &rank_out,
				 &dtype,
				 &sel_type,
				 &numelem,
				 buf );

    if(status < 0 ) 
      printf(" ERROR: H5LRget_region_info <2>\n");

    /* check values */

    if(buf[0] != 3 || buf[1] != 3 || buf[2] != 5 || buf[3] != 4)
      printf(" VALIDATION ERROR: hyperslab coordinates\n");

/*     size  = H5Tget_size(dtype); */

/*     /\* */
/*      * Allocate array of pointers to rows. */
/*      *\/ */
/*     rdata = (int **) malloc ( (buf[2]-buf[0]+1 )* sizeof (int *)); */

/*     /\* */
/*      * Allocate space for integer data. */
/*      *\/ */
/*     rdata[0] = (int *) malloc ( (buf[2]-buf[0]+1) * (buf[3]-buf[1]+1) * sizeof (int)); */

/*     /\* */
/*      * Set the rest of the pointers to rows to the correct addresses. */
/*      *\/ */
/*     for (i=1; i<(buf[2]-buf[0]+1); i++) */
/*         rdata[i] = rdata[0] + i * (buf[3]-buf[1]+1); */


/*     rdata = ( int **) malloc(  numelem * sizeof (int *) ); */

     status = H5LRread_region(file_id, 
 			     &ref[1], 
 			     H5T_NATIVE_INT, 
 			     &numelem, 
 			     data_out2);

     printf("REGION REFERENCED 2D HYPERSLAB:\n   /        \\");
    for (i=0; i<3; i++)
      {
	printf("\n  |  ");
	for (j=0; j<2; j++) {
	  printf("%d ", data_out2[i][j]);
	}
	printf("  |");
      }
    printf("\n   \\        /\n");
    printf("\n");

    status = H5Fclose(file_id);

    status = H5LTread_region(filename,
			     "/Group_3D/DS3",
	                     block_coord_3D,
 			     H5T_NATIVE_INT,
 			     data_out3);

    printf("REGION REFERENCED 3D HYPERSLAB:");
    for (k=0; k<2; k++){
      printf("\n   /          \\");
      for (i=0; i<2; i++)
	{
	  printf("\n  |  ");
	  for (j=0; j<2; j++) {
	    printf("%d ", data_out3[i][j][k]);
	  }
	  printf("  |");
	}
      printf("\n   \\          /\n");
      printf("\n");
    }
 

/*     status = H5LTcopy_region(filename,  */
/* 			     "/Group_1D/DS1", */
/* 	                     block_coord_1D_src, */
/* 			     filename, */
/*  			     "/Group_1D/DS1", */
/*  			     block_coord_1D_dest); */

    status = H5LTcopy_region(filename,
 			     "/Group_2D/DS2",
 	                     block_coord_2D_src,
 			     filename,
  			     "/Group_2D/DS2",
  			     block_coord_2D_dest);

    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);

    status =  H5LRcopy_references( file_id, &ref[1], filename,
				    "/Group_2D/DS2", block_coord_2D_dest, &ref_new);
    

    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);

    status = H5LRcreate_region_references(file_id,
					  num_elem,
					  path,
					  block_coord_6,
					  ref6);

    status = H5LRcopy_region(file_id, ref[1], filename, "/Group_2D/DS2", block_coord_4);


/*     status = H5LRmake_dataset(file_id, "/Group_2D/DS2a", H5T_NATIVE_INT, file_id, 2, ref6); */




/*        printf("Selected  hyperslab: "); */
/*     for (i=0; i<2; i++) */
/*       for (j=0; j<2; j++) */
/* 	for (k=0; k<2; k++) */
/* 	  printf("%d %d %d %d \n ",i,j,k,data3D[i][j][k]); */

    return 0;
}



