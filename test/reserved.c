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

#include "h5test.h"

/*-------------------------------------------------------------------------
 * Function:	rsrv_heap
 *
 * Purpose:	Ensure that heaps reserve file address space.
 *			This function does this by creating datasets up to and past
 *			the limit of the file, then ensuring that an error (not an
 *			assert) was generated and that the file is readable.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	James Laird
 *				Nat Furrer
 *              Friday, May 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
rsrv_heap(void)
{
    hid_t       file_id, dataset_id, dataspace_id;
    hid_t       pfc_id;
    hsize_t     dims[1] = {1};
    herr_t      ret_value = 0;
    char dset_name[10];
    int    i;

    TESTING("Reserving file space for heap");

    /* Create a new file. */
    pfc_id = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_userblock(pfc_id, (hsize_t)0);
    H5Pset_sym_k(pfc_id, 1, 1);
    H5Pset_istore_k(pfc_id, 1);

    /* Set file address sizes to be very small. */
    H5Pset_sizes(pfc_id, (size_t)2,(size_t)2);

    file_id = H5Fcreate("rsrv_heap", H5F_ACC_TRUNC, pfc_id, H5P_DEFAULT);

    /* Write datasets until the file is full, at which point HDF5
     * should throw an error.
     */
    for (i = 0; i < 200; i++)
    {
        H5E_BEGIN_TRY {
            dataspace_id = H5Screate_simple(1, dims, dims);
        } H5E_END_TRY

        sprintf(dset_name, "Dset %d", i);

        H5E_BEGIN_TRY {
            dataset_id = H5Dcreate(file_id, dset_name, H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT);
        } H5E_END_TRY

        if(dataset_id < 0)
            break;

        H5E_BEGIN_TRY {
            H5Dwrite(dataset_id, H5T_NATIVE_INT, dataspace_id, dataspace_id, H5P_DEFAULT, &i);
        } H5E_END_TRY

        H5Dclose(dataset_id);
        H5Sclose(dataspace_id);
    }

    /* Close the file, property lists, and library */
    H5Fclose(file_id);
    H5Pclose(pfc_id);
    H5close();

    /* The loop should have broken before completing--the file should not have had
     * enough address space to hold 200 datasets (or this test needs to be updated!).
     */
    if(i == 200)
    {
        ret_value = 1;
        H5_FAILED();
    }

    /* Re-open the library and try to read a dataset from the file we created */
    H5open();

    sprintf(dset_name, "Dset %d", i - 2);

    file_id = H5Fopen("rsrv_heap", H5F_ACC_RDONLY, H5P_DEFAULT);

    dataset_id = H5Dopen(file_id, dset_name);

    /* If we can read a dataset from the file, the file has been flushed to disk
     * (if the heap or object headers weren't flushed, the file would be empty).
     */
    if(dataset_id == H5I_BADID)
    {
        ret_value = 1;
        H5_FAILED();
    }

    H5Dclose(dataset_id);
    H5Fclose(file_id);
    HDremove("rsrv_heap");

    if(ret_value == 0)
    {
        PASSED();
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:	rsrv_ohdr
 *
 * Purpose:	Ensure that object headers reserve file address space.
 *			This function does this by creating attributes of a dataset
 *			past the limit of the file, then ensuring that an error (not
 *			an assert) was generated and that the file is readable.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	James Laird
 *				Nat Furrer
 *              Friday, May 28, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
rsrv_ohdr(void)
{
    hid_t       file_id, dataset_id, dataspace_id;
    hid_t       pfc_id, aid, attr_id;
    hsize_t     dims[2];
    herr_t      status, ret_value = 0;
    int         attrval[4][6];
    char attrname[20];
    int    i;

    TESTING("Reserving file space for object headers");

    /* Create a new file using default properties. */
    pfc_id = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_userblock(pfc_id, (hsize_t)0);
    H5Pset_sym_k(pfc_id, 1, 1);
    H5Pset_istore_k(pfc_id, 1);
    H5Pset_sizes(pfc_id, (size_t)2,(size_t)2);

    file_id = H5Fcreate("rsrv_ohdr", H5F_ACC_TRUNC, pfc_id, H5P_DEFAULT);

    /* Create the data space for the dataset. */
    dims[0] = 4; 
    dims[1] = 6; 
    dataspace_id = H5Screate_simple(2, dims, NULL);

    /* Create the dataset. */
    dataset_id = H5Dcreate(file_id, "/dset", H5T_STD_I32BE, dataspace_id, H5P_DEFAULT);

    for(i=0; i<6; i++)
    {
        attrval[0][i] = 0;
        attrval[1][i] = 1;
        attrval[2][i] = 2;
        attrval[3][i] = 3;
    }

    for (i = 0; i< 2000; i++)
    {
        sprintf(attrname, "attr %d", i);
        H5E_BEGIN_TRY{
            aid =  H5Screate_simple(2, dims, NULL);
            attr_id = H5Acreate (dataset_id, attrname, H5T_STD_I32BE, aid, H5P_DEFAULT);
            status = H5Awrite(attr_id, H5T_NATIVE_INT, attrval);
            status = H5Aclose(attr_id);
        } H5E_END_TRY

        if(status < 0)
            break;
    } 

    /* End access to the dataset and dataspace and release resources. */
    H5Dclose(dataset_id);
    H5Pclose (pfc_id);
    H5Sclose(dataspace_id);

    /* Close the file and the library. */
    H5Fclose(file_id);
    H5close();

    /* The loop should have broken before completing--the file should not have had
     * enough address space to hold 2000 attributes (or this test needs to be updated!).
     */
    if(i == 2000)
    {
        ret_value = 1;
        H5_FAILED();
    }

    /* Re-open the library and try to read a dataset from the file we created */
    H5open();

    file_id = H5Fopen("rsrv_ohdr", H5F_ACC_RDONLY, H5P_DEFAULT);

    dataset_id = H5Dopen(file_id, "/dset");

    /* If we can read the dataset from the file, the file has been flushed to disk
     * (if the heap or object headers weren't flushed, the file would be empty).
     */
    if(dataset_id == H5I_BADID)
    {
        ret_value = 1;
        H5_FAILED();
    }

    if(ret_value == 0)
        PASSED();

    H5Dclose(dataset_id);
    H5Fclose(file_id);

    HDremove("rsrv_ohdr");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:	rsrv_vlen
 *
 * Purpose:	Ensure that variable length datatypes properly ensure that
 *              enough file address space exists before writing.
 *		This function does this by creating a dataset containing
 *              variable length data past the limit of the file, then
 *              ensuring that an error (not an assert) was generated and 
 *              that the file is readable.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	James Laird
 *		Nat Furrer
 *              Thursday, July 1, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
rsrv_vlen(void)
{
    hid_t       file_id, dataset_id, dataspace_id, type_id;
    hid_t       pfc_id, mem_space_id;
    hssize_t    offset[1];
    hssize_t    start[1];
    hsize_t     dims[1], count[1];
    herr_t      status, ret_value = 0;
    int    i;
    int    write_buf[20];
    hvl_t  vlen_data;

    TESTING("Reserved space with variable length data");

    /* Create a new file using default properties. */
    pfc_id = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_userblock(pfc_id, (hsize_t)0);
    H5Pset_sym_k(pfc_id, 1, 1);
    H5Pset_istore_k(pfc_id, 1);
    H5Pset_sizes(pfc_id, (size_t)2,(size_t)2);

    file_id = H5Fcreate("rsrv_vlen", H5F_ACC_TRUNC, pfc_id, H5P_DEFAULT);

    /* Create the data space for the dataset. */
    dims[0] = 2000; 
    dataspace_id = H5Screate_simple(1, dims, NULL);

    /* Create a variable length type */
    type_id = H5Tvlen_create(H5T_NATIVE_INT);

    /* Create the dataset. */
    dataset_id = H5Dcreate(file_id, "/dset", type_id, dataspace_id, H5P_DEFAULT);

    /* Create some data to write */
    for (i = 0; i < 20; i++)
        write_buf[i] = i+1;
    vlen_data.p = write_buf;

    /* Create a memory dataspace for writing */
    dims[0] = 1;
    mem_space_id = H5Screate_simple(1, dims, NULL); 

    /* Create a selection to write to */
    start[0] = 0;
    count[0] = 1;
    H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, start, NULL, count, NULL);

    for (i = 0; i< 2000; i++)
    {
        vlen_data.len = (i%20) + 1;

        offset[0] = i;
        H5Soffset_simple(dataspace_id, offset); 

        H5E_BEGIN_TRY
            status = H5Dwrite(dataset_id, type_id, mem_space_id, dataspace_id, H5P_DEFAULT, &vlen_data);
        H5E_END_TRY

        if(status < 0)
            break;
    } 

    /* End access to the dataset and dataspace and release resources. */
    H5Dclose(dataset_id);
    H5Pclose (pfc_id);
    H5Sclose(dataspace_id);
    H5Tclose(type_id);
    H5Sclose(mem_space_id);

    /* Close the file and the library. */
    H5Fclose(file_id);
    H5close();

    /* The loop should have broken before completing--the file should not have had
     * enough address space to hold 2000 attributes (or this test needs to be updated!).
     */
    if(i == 2000)
    {
        ret_value = 1;
        H5_FAILED();
    }

    /* Re-open the library and try to read a dataset from the file we created */
    H5open();

    file_id = H5Fopen("rsrv_vlen", H5F_ACC_RDONLY, H5P_DEFAULT);

    dataset_id = H5Dopen(file_id, "/dset");

    /* If we can read the dataset from the file, the file has been flushed to disk
     * (if the heap or object headers weren't flushed, the file would be empty).
     */
    if(dataset_id == H5I_BADID)
    {
        ret_value = 1;
	H5_FAILED();
    }

    if(ret_value == 0)
        PASSED();

    H5Dclose(dataset_id);
    H5Fclose(file_id);

    HDremove("rsrv_vlen");

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Nat Furrer and James Laird
 *              Thursday, July 1, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int num_errs=0;

    num_errs+=rsrv_ohdr();
    num_errs+=rsrv_heap();
    num_errs+=rsrv_vlen();

    return num_errs;
}

