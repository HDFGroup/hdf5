 /* 
  *       This program illustrates how references to the objects can be used.
  *       Program creates two datasets in the file. It also creates the third
  *       dataset, and references to the first two datasets are stored in it.
  *       Program reopens the file and reads dataset with the references.
  *       References are used to open first two datasets and read datatspace
  *       and datatype information about them. 
  *         
 */

#include <stdlib.h>
#include<hdf5.h>

#define FILE "refere.h5"

int
main(void) {
   hid_t fid;                         /* File, datasets, datatypes and    */
   hid_t did_a, sid_a;                /* dataspaces identifiers for three */ 
   hid_t did_b, tid_b, sid_b;         /* datasets.                        */ 
   hid_t did_r, tid_r, sid_r;
   herr_t status;

   hobj_ref_t *wbuf; /* buffer to write to disk */
   hobj_ref_t *rbuf; /* buffer to read from disk */
 

   hsize_t dim_r[1]; 
   hsize_t dim_a[1];
   hsize_t dim_b[2];

   herr_t ret; /* return values */
   
   /* 
    *  Create a file using default properties.
    */
   fid = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /* 
    *  Create  dataset "A" in the file.
    */
   dim_a[0] = 5;
   sid_a = H5Screate_simple(1, dim_a, NULL);
   did_a = H5Dcreate(fid, "A", H5T_NATIVE_INT, sid_a, H5P_DEFAULT);
   
  /* 
   *  Create dataset "B" in the file.
   */
   dim_b[0] = 2;
   dim_b[1] = 6;
   sid_b = H5Screate_simple(2, dim_b, NULL);  
   did_b = H5Dcreate(fid, "B", H5T_NATIVE_FLOAT, sid_b, H5P_DEFAULT);
   
   /* 
    *  Create dataset "R" to store references to the datasets "A" and "B".
    */
   dim_r[0] = 2;
   sid_r = H5Screate_simple(1, dim_r, NULL);
   tid_r = H5Tcopy(H5T_STD_REF_OBJ);
   did_r = H5Dcreate(fid, "R", tid_r, sid_r, H5P_DEFAULT );

   /* 
    *  Allocate write and read buffers.
    */
   wbuf = malloc(sizeof(hobj_ref_t)*2);
   rbuf = malloc(sizeof(hobj_ref_t)*2);
  
   /*
    *  Create references to the datasets "A" and "B"
    *  and store them in the wbuf.
    */
   H5Rcreate(&wbuf[0], fid, "A", H5R_OBJECT, -1); 
   H5Rcreate(&wbuf[1], fid, "B", H5R_OBJECT, -1);
 
   /* 
    *  Write dataset R using default transfer properties.
    */
   status = H5Dwrite(did_r, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, wbuf);

   /* 
    *  Close all objects. 
    */
   H5Sclose(sid_a);
   H5Dclose(did_a);
   
   H5Sclose(sid_b);
   H5Dclose(did_b);

   H5Tclose(tid_r);
   H5Sclose(sid_r);
   H5Dclose(did_r);
   
   H5Fclose(fid);
   
   /* 
    * Reopen the file.
    */
   fid = H5Fopen(FILE, H5F_ACC_RDWR, H5P_DEFAULT);

   /* 
    *  Open and read dataset "R".
    */
   did_r = H5Dopen(fid, "R");
   status = H5Dread(did_r, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL,
		    H5P_DEFAULT, rbuf);

   /* 
    *  Open dataset A using reference to it.
    */
   did_a = H5Rdereference(did_r, H5R_OBJECT, &rbuf[0]);

   /* 
    *  Get rank of the dataset "A"
    */

   printf("\n");
   sid_a = H5Dget_space(did_a);
   ret = H5Sget_simple_extent_ndims(sid_a);
   
   if(ret == 1) printf("Rank of A is %d.\n", ret);
   printf("\n");

   /* 
    *  Get datatype of the dataset "B"
    */
   did_b = H5Rdereference(did_r, H5R_OBJECT, &rbuf[1]);
   tid_b = H5Dget_type(did_b);
   if(H5Tequal(tid_b, H5T_NATIVE_FLOAT))
     printf("Datatype of B is H5T_NATIVE_FLOAT.\n");
   printf("\n");

   /* 
    * Close all objects.
    */
   H5Dclose(did_a);
   H5Sclose(sid_a);
   H5Dclose(did_b);
   H5Tclose(tid_b);
   H5Fclose(fid);

   return 0;

 }



