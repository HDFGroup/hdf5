/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by  The HDF Group (THG) and                                     *
 *               The Board of Trustees of the University of Illinois.        *
 * All rights reserved.                                                      *
 *                                                                           * 
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   * 
 * is linked from the top-level documents page.  It can also be found at     *  
 * http://www.hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have      *
 * access to either file, you may request a copy from help@hdfgroup.org.     * 
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */          
#include "h5test.h"

#define filename "refreg_name.h5"
#define dsetnamev "MATRIX"
#define dsetnamer "REGION_REFERENCES"

static int basic_test(void)
{
    hid_t	file_id;        /* file identifier */
    hid_t	dsetv_id;       /*dataset identifiers*/
    hid_t	dsetr_id;      
    hid_t	space_id, spacer_id;      
    hsize_t	dims[2] =  {2,9};
    hsize_t	dimsr[1] =  {2};
    int		rank = 2;
    int		rankr =1;
    herr_t	status;
    hdset_reg_ref_t	 ref[2];
    hdset_reg_ref_t	 ref_out[2];
    int		data[2][9] = {{1,1,2,3,3,4,5,5,6},{1,2,2,3,4,4,5,6,6}};
    hsize_t 	start[2];
    hsize_t 	count[2];
    hsize_t 	coord[2][3] = {{0, 0, 1}, {6, 0, 8}};
    unsigned 	num_points = 3;
    size_t 	name_size1, name_size2;
    char 	buf1[10], buf2[10];

    /* Create file with default file access and file creation properties */
    if((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	TEST_ERROR

    /* Create dataspace for datasets */
    if((space_id = H5Screate_simple(rank, dims, NULL)) < 0)
	TEST_ERROR
    if((spacer_id = H5Screate_simple(rankr, dimsr, NULL)) < 0)
	TEST_ERROR

    /* Create integer dataset */
    if((dsetv_id = H5Dcreate(file_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT)) < 0)
	TEST_ERROR

     /* Write data to the dataset */
    if((status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data)) < 0)
	TEST_ERROR
    if((status = H5Dclose(dsetv_id)) < 0)
	TEST_ERROR

    /* Dataset with references */
    if((dsetr_id = H5Dcreate(file_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT)) < 0)
	TEST_ERROR

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 0; 
    start[1] = 3; 
    count[0] = 2;
    count[1] = 3;
    if((status = H5Sselect_hyperslab(space_id,H5S_SELECT_SET,start,NULL,count,NULL)) < 0)
	TEST_ERROR
    if((status = H5Rcreate(&ref[0], file_id, dsetnamev, H5R_DATASET_REGION, space_id)) < 0)
	TEST_ERROR

    /* Create a reference to elements selection */
    if((status = H5Sselect_none(space_id)) < 0)
	TEST_ERROR
    if((status = H5Sselect_elements(space_id, H5S_SELECT_SET, num_points, (const hsize_t **)coord)) < 0)
	TEST_ERROR
    if((status = H5Rcreate(&ref[1], file_id, dsetnamev, H5R_DATASET_REGION, space_id)) < 0)
	TEST_ERROR

    /* Write dataset with the references */
    if((status = H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref)) < 0)
	TEST_ERROR

    /* Close all objects */
    if((status = H5Sclose(space_id)) < 0)
	TEST_ERROR
    if((status = H5Sclose(spacer_id)) < 0)
	TEST_ERROR
    if((status = H5Dclose(dsetr_id)) < 0)
	TEST_ERROR
    if((status = H5Fclose(file_id)) < 0)
	TEST_ERROR

    /* Reopen the file to read selections back */
    if((file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT)) < 0)
	TEST_ERROR

    /* Reopen the dataset with object references and read references to the buffer */
    if((dsetr_id = H5Dopen (file_id, dsetnamer)) , 0)
	TEST_ERROR

    if((status = H5Dread(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_out)) < 0)
	TEST_ERROR

    /* Dereference the first reference */
    dsetv_id = H5Rdereference(dsetr_id, H5R_DATASET_REGION, &ref_out[0]);

    /* Get name of the dataset the first region reference points to using H5Rget_name */

    TESTING("H5Rget_name to get name from region reference (hyperslab)");
    name_size1 = H5Rget_name(dsetr_id, H5R_DATASET_REGION, &ref_out[0], (char*)buf1, 10);      
/* 
    if(!((HDstrcmp(buf1, "/MATRIX") == 0) && (name_size1 == 8))) TEST_ERROR 
    PASSED()
*/
    SKIPPED()

    /* Get name of the dataset the first region reference points using H5Iget_name */

    TESTING("H5Iget_name to get name from region reference (hyperslab)");
    name_size2 = H5Iget_name(dsetv_id, (char*)buf2, 10); 
    if(!((HDstrcmp(buf2, "/MATRIX") == 0) && (name_size2 == 8))) TEST_ERROR
    PASSED()

    if((status = H5Dclose(dsetv_id)) < 0)
	TEST_ERROR

    /* Dereference the second reference */
    if((dsetv_id = H5Rdereference(dsetr_id, H5R_DATASET_REGION, &ref_out[1])) < 0)
	TEST_ERROR

    TESTING("H5Rget_name to get name from region reference (pnt selec)");
    name_size1 = H5Rget_name(dsetr_id, H5R_DATASET_REGION, &ref_out[1], (char*)buf1, 10);      
/*
    if(!((HDstrcmp(buf1, "/MATRIX") == 0) && (name_size1 == 8))) TEST_ERROR
    PASSED()
 */
    SKIPPED() 

    /* Get name of the dataset the first region reference points using H5Iget_name */

    TESTING("H5Iget_name to get name from region reference (pnt selec)");
    name_size2 = H5Iget_name(dsetv_id, (char*)buf2, 10); 
    if(!((HDstrcmp(buf2, "/MATRIX") == 0) && (name_size2 == 8))) TEST_ERROR
    PASSED()

    if((status = H5Dclose(dsetv_id)) < 0)
	TEST_ERROR

    if((status = H5Dclose(dsetr_id)) < 0)
	TEST_ERROR
    if((status = H5Fclose(file_id)) < 0)
	TEST_ERROR
     
    return 0;


error:
    return -1;
}

int main(void) 
{
    int 	nerrors=0;
    nerrors += basic_test()<0 	?1:0;

    if (nerrors){
		 printf("***** %d Get name from region reference TEST%s FAILED! *****\n",
			nerrors, nerrors > 1 ? "S" : "");
		return 1;
    }
    printf("Get name from region reference tests passed\n");
    return 0;
}
    


