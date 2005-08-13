/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  This program illustrates the usage of the H5A Interface functions.
 *  It creates and writes a dataset, and then creates and writes array,
 *  scalar, and string attributes of the dataset.
 *  Program reopens the file, attaches to the scalar attribute using
 *  attribute name and reads and displays its value. Then index of the
 *  third attribute is used to read and display attribute values.
 *  The H5Aiterate function is used to iterate through the dataset attributes,
 *  and display their names. The function is also reads and displays the values
 *  of the array attribute.
 */

#include <stdlib.h>

#include "hdf5.h"

#define H5FILE_NAME "Attributes.h5"

#define RANK  1   /* Rank and size of the dataset  */
#define SIZE  7

#define ARANK  2   /* Rank and dimension sizes of the first dataset attribute */
#define ADIM1  2
#define ADIM2  3
#define ANAME  "Float attribute"      /* Name of the array attribute */
#define ANAMES "Character attribute" /* Name of the string attribute */

herr_t attr_info(hid_t loc_id, const char *name, void *opdata);
                                     /* Operator function */

int
main (void)
{

   hid_t   file, dataset;       /* File and dataset identifiers */

   hid_t   fid;                 /* Dataspace identifier */
   hid_t   attr1, attr2, attr3; /* Attribute identifiers */
   hid_t   attr;
   hid_t   aid1, aid2, aid3;    /* Attribute dataspace identifiers */
   hid_t   atype;               /* Attribute type */

   hsize_t fdim[] = {SIZE};
   hsize_t adim[] = {ADIM1, ADIM2};  /* Dimensions of the first attribute  */

   float matrix[ADIM1][ADIM2]; /* Attribute data */

   herr_t  ret;                /* Return value */
   unsigned i,j;                /* Counters */
   int     idx;                /* Attribute index */
   char    string_out[80];     /* Buffer to read string attribute back */
   int     point_out;          /* Buffer to read scalar attribute back */

   /*
    * Data initialization.
    */
   int vector[] = {1, 2, 3, 4, 5, 6, 7};  /* Dataset data */
   int point = 1;                         /* Value of the scalar attribute */
   char string[] = "ABCD";                /* Value of the string attribute */


   for (i=0; i < ADIM1; i++) {            /* Values of the array attribute */
       for (j=0; j < ADIM2; j++)
       matrix[i][j] = -1.;
   }

   /*
    * Create a file.
    */
   file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /*
    * Create the dataspace for the dataset in the file.
    */
   fid = H5Screate(H5S_SIMPLE);
   ret = H5Sset_extent_simple(fid, RANK, fdim, NULL);

   /*
    * Create the dataset in the file.
    */
   dataset = H5Dcreate(file, "Dataset", H5T_NATIVE_INT, fid, H5P_DEFAULT);

   /*
    * Write data to the dataset.
    */
   ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT, vector);

   /*
    * Create dataspace for the first attribute.
    */
   aid1 = H5Screate(H5S_SIMPLE);
   ret  = H5Sset_extent_simple(aid1, ARANK, adim, NULL);

   /*
    * Create array attribute.
    */
   attr1 = H5Acreate(dataset, ANAME, H5T_NATIVE_FLOAT, aid1, H5P_DEFAULT);

   /*
    * Write array attribute.
    */
   ret = H5Awrite(attr1, H5T_NATIVE_FLOAT, matrix);

   /*
    * Create scalar attribute.
    */
   aid2  = H5Screate(H5S_SCALAR);
   attr2 = H5Acreate(dataset, "Integer attribute", H5T_NATIVE_INT, aid2,
                     H5P_DEFAULT);

   /*
    * Write scalar attribute.
    */
   ret = H5Awrite(attr2, H5T_NATIVE_INT, &point);

   /*
    * Create string attribute.
    */
   aid3  = H5Screate(H5S_SCALAR);
   atype = H5Tcopy(H5T_C_S1);
           H5Tset_size(atype, 4);
           H5Tset_strpad(atype,H5T_STR_NULLTERM);
   attr3 = H5Acreate(dataset, ANAMES, atype, aid3, H5P_DEFAULT);

   /*
    * Write string attribute.
    */
   ret = H5Awrite(attr3, atype, string);

   /*
    * Close attribute and file dataspaces.
    */
   ret = H5Sclose(aid1);
   ret = H5Sclose(aid2);
   ret = H5Sclose(aid3);
   ret = H5Sclose(fid);

   /*
    * Close the attributes.
    */
   ret = H5Aclose(attr1);
   ret = H5Aclose(attr2);
   ret = H5Aclose(attr3);

   /*
    * Close the dataset.
    */
   ret = H5Dclose(dataset);

   /*
    * Close the file.
    */
   ret = H5Fclose(file);

   /*
    * Reopen the file.
    */
   file = H5Fopen(H5FILE_NAME, H5F_ACC_RDONLY, H5P_DEFAULT);

   /*
    * Open the dataset.
    */
   dataset = H5Dopen(file,"Dataset");

   /*
    * Attach to the scalar attribute using attribute name, then read and
    * display its value.
    */
   attr = H5Aopen_name(dataset,"Integer attribute");
   ret  = H5Aread(attr, H5T_NATIVE_INT, &point_out);
   printf("The value of the attribute \"Integer attribute\" is %d \n", point_out);
   ret =  H5Aclose(attr);

   /*
    * Attach to the string attribute using its index, then read and display the value.
    */
   attr  = H5Aopen_idx(dataset, 2);
   atype = H5Tcopy(H5T_C_S1);
           H5Tset_size(atype, 5);
   ret   = H5Aread(attr, atype, string_out);
   printf("The value of the attribute with index 2 is %s \n", string_out);
   ret   = H5Aclose(attr);
   ret   = H5Tclose(atype);

   /*
    * Get attribute info using iteration function.
    */
   idx = H5Aiterate(dataset, NULL, attr_info, NULL);

   /*
    * Close the dataset and the file.
    */
   H5Dclose(dataset);
   H5Fclose(file);

   return 0;
}

/*
 * Operator function.
 */
herr_t
attr_info(hid_t loc_id, const char *name, void *opdata)
{
    hid_t attr, atype, aspace;  /* Attribute, datatype and dataspace identifiers */
    int   rank;
    hsize_t sdim[64];
    herr_t ret;
    int i;
    size_t npoints;             /* Number of elements in the array attribute. */
    float *float_array;         /* Pointer to the array attribute. */

    /* avoid warnings */
    opdata = opdata;

    /*
     * Open the attribute using its name.
     */
    attr = H5Aopen_name(loc_id, name);

    /*
     * Display attribute name.
     */
    printf("\n");
    printf("Name : ");
    puts(name);

    /*
     * Get attribute datatype, dataspace, rank, and dimensions.
     */
    atype  = H5Aget_type(attr);
    aspace = H5Aget_space(attr);
    rank = H5Sget_simple_extent_ndims(aspace);
    ret = H5Sget_simple_extent_dims(aspace, sdim, NULL);

    /*
     *  Display rank and dimension sizes for the array attribute.
     */

    if(rank > 0) {
    printf("Rank : %d \n", rank);
    printf("Dimension sizes : ");
    for (i=0; i< rank; i++) printf("%d ", (int)sdim[i]);
    printf("\n");
    }

    /*
     * Read array attribute and display its type and values.
     */

    if (H5T_FLOAT == H5Tget_class(atype)) {
    printf("Type : FLOAT \n");
    npoints = H5Sget_simple_extent_npoints(aspace);
    float_array = (float *)malloc(sizeof(float)*(int)npoints);
    ret = H5Aread(attr, atype, float_array);
    printf("Values : ");
    for( i = 0; i < (int)npoints; i++) printf("%f ", float_array[i]);
    printf("\n");
    free(float_array);
    }

    /*
     * Release all identifiers.
     */
    H5Tclose(atype);
    H5Sclose(aspace);
    H5Aclose(attr);

    return 0;
}
