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

/***********************************************************
*
* Test program:	 th5o
*
* Test public H5O functions for accessing
*
*************************************************************/

#include "testhdf5.h"

/*#include "H5private.h"
#include "H5Bprivate.h"
#include "H5Sprivate.h"
#include "H5Pprivate.h"
*/

#define TEST_FILENAME "th5o_file"

#define RANK 2
#define DIM0 5
#define DIM1 10

/****************************************************************
**
**  test_h5o_open(): Test H5Oopen function.
**
****************************************************************/
static void
test_h5o_open(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hsize_t     dims[RANK];
    H5I_type_t  id_type;                    /* Type of IDs returned from H5Oopen */
    H5G_info_t  ginfo;                      /* Group info struct */
    H5T_class_t type_class;                 /* Class of the datatype */
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Now make sure that H5Oopen can open all three types of objects */
    grp = H5Oopen(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Oopen");
    dtype = H5Oopen(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Oopen");
    /* Check that we can use the group as a valid location */
    dset = H5Oopen(grp, "/dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Oopen");

    /* Make sure that each is the right kind of ID */
    id_type = H5Iget_type(grp);
    VERIFY(id_type, H5I_GROUP, "H5Iget_type for group ID");
    id_type = H5Iget_type(dtype);
    VERIFY(id_type, H5I_DATATYPE, "H5Iget_type for datatype ID");
    id_type = H5Iget_type(dset);
    VERIFY(id_type, H5I_DATASET, "H5Iget_type for dataset ID");

    /* Do something more complex with each of the IDs to make sure they "work" */
    ret = H5Gget_info(grp, ".", &ginfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 1, "H5Gget_info"); /* There should be one object, the datatype */

    type_class = H5Tget_class(dtype);
    VERIFY(type_class, H5T_INTEGER, "H5Tget_class");

    dspace = H5Dget_space(dset);
    CHECK(dspace, FAIL, "H5Dget_space");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Trying to open objects with bogus names should fail gracefully */
    H5E_BEGIN_TRY {
        grp = H5Oopen(fid, "bogus_group", H5P_DEFAULT);
        VERIFY(grp, FAIL, "H5Oopen");
        dtype = H5Oopen(fid, "group/bogus_datatype", H5P_DEFAULT);
        VERIFY(dtype, FAIL, "H5Oopen");
        dset = H5Oopen(fid, "/bogus_dataset", H5P_DEFAULT);
        VERIFY(dset, FAIL, "H5Oopen");
    } H5E_END_TRY

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Trying to open an object with a bogus file ID should fail */
    H5E_BEGIN_TRY {
        dset = H5Oopen(fid, "dataset", H5P_DEFAULT);
        VERIFY(dset, FAIL, "H5Oopen");
    } H5E_END_TRY
} /* test_h5o_open() */



/****************************************************************
**
**  test_h5o_close(): Test H5Oclose function.
**
****************************************************************/
static void
test_h5o_close(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hsize_t     dims[RANK];
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group and close it with H5Oclose */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");
    VERIFY(H5Iget_type(grp), H5I_GROUP, "H5Iget_type");
    ret = H5Oclose(grp);
    CHECK(ret, FAIL, "H5Oclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Oclose(dtype);
    CHECK(ret, FAIL, "H5Oclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Oclose(dset);
    CHECK(ret, FAIL, "H5Oclose");

    /* Attempting to close the data space with H5Oclose should fail */
    H5E_BEGIN_TRY {
       ret = H5Oclose(dspace);
       VERIFY(ret, FAIL, "H5Oclose");
    } H5E_END_TRY
    /* Close the dataspace for real */
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Make sure that H5Oclose can close objects opened with H5Oopen */
    grp = H5Oopen(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Oopen");
    dtype = H5Oopen(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Oopen");
    dset = H5Oopen(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Oopen");

    ret = H5Oclose(grp);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dtype);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dset);
    CHECK(ret, FAIL, "H5Oclose");

    /* Make sure H5Oclose can close objects opened with H5*open */
    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "group/datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Oclose(grp);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dtype);
    CHECK(ret, FAIL, "H5Oclose");
    ret = H5Oclose(dset);
    CHECK(ret, FAIL, "H5Oclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}
   

/****************************************************************
**
**  test_h5o_open_by_addr(): Test H5Oopen_by_addr function.
**
****************************************************************/
static void
test_h5o_open_by_addr(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    H5L_info_t li;                      /* Buffer for H5Lget_info */
    haddr_t grp_addr;                       /* Addresses for objects */
    haddr_t dset_addr;
    haddr_t dtype_addr;
    hsize_t     dims[RANK];
    H5I_type_t  id_type;                    /* Type of IDs returned from H5Oopen */
    H5G_info_t  ginfo;                      /* Group info struct */
    H5T_class_t type_class;                 /* Class of the datatype */
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "group/datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Get address for each object */
    ret = H5Lget_info(fid, "group", &li, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_info");
    grp_addr = li.u.address;
    ret = H5Lget_info(fid, "group/datatype", &li, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_info");
    dtype_addr = li.u.address;
    ret = H5Lget_info(fid, "dataset", &li, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lget_info");
    dset_addr = li.u.address;

    /* Now make sure that H5Oopen_by_addr can open all three types of objects */
    grp = H5Oopen_by_addr(fid, grp_addr);
    CHECK(grp, FAIL, "H5Oopen_by_addr");
    dtype = H5Oopen_by_addr(fid, dtype_addr);
    CHECK(dtype, FAIL, "H5Oopen_by_addr");
    /* Check that we can use the group ID as a valid location */
    dset = H5Oopen_by_addr(grp, dset_addr);
    CHECK(dset, FAIL, "H5Oopen_by_addr");

    /* Make sure that each is the right kind of ID */
    id_type = H5Iget_type(grp);
    VERIFY(id_type, H5I_GROUP, "H5Iget_type for group ID");
    id_type = H5Iget_type(dtype);
    VERIFY(id_type, H5I_DATATYPE, "H5Iget_type for datatype ID");
    id_type = H5Iget_type(dset);
    VERIFY(id_type, H5I_DATASET, "H5Iget_type for dataset ID");

    /* Do something more complex with each of the IDs to make sure they "work" */
    ret = H5Gget_info(grp, ".", &ginfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 1, "H5Gget_info"); /* There should be one object, the datatype */

    type_class = H5Tget_class(dtype);
    VERIFY(type_class, H5T_INTEGER, "H5Tget_class");

    dspace = H5Dget_space(dset);
    CHECK(dspace, FAIL, "H5Dget_space");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Try giving some bogus values to H5O_open_by_addr. */
    /* Try to open an object with a bad address */
    grp_addr += 20;
    H5E_BEGIN_TRY{
      grp = H5Oopen_by_addr(fid, grp_addr);
    }H5E_END_TRY
    VERIFY(grp, FAIL, "H5Oopen_by_addr");

    /* For instance, an objectno smaller than the end of the file's superblock should
     * trigger an error */
    grp_addr = 10;
    H5E_BEGIN_TRY{
      grp = H5Oopen_by_addr(fid, grp_addr);
    }H5E_END_TRY
    VERIFY(grp, FAIL, "H5Oopen_by_addr");

    /* Likewise, an objectno larger than the size of the file should fail */
    grp_addr = 0;
    grp_addr = 1000000000;
    H5E_BEGIN_TRY{
      grp = H5Oopen_by_addr(fid, grp_addr);
    }H5E_END_TRY
    VERIFY(grp, FAIL, "H5Oopen_by_addr");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Also, trying to open an object without a valid location should fail */
    H5E_BEGIN_TRY{
      dtype = H5Oopen_by_addr(fid, dtype_addr);
    }H5E_END_TRY
    VERIFY(dtype, FAIL, "H5Oopen_by_addr");
} /* test_h5o_open_by_addr() */


/****************************************************************
**
**  test_h5o_refcount(): Test H5O refcounting functions.
**
****************************************************************/
static void
test_h5o_refcount(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    H5O_info_t	oinfo;                      /* Object info struct */
    hsize_t     dims[RANK];
    herr_t      ret;                        /* Value returned from API calls */

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create a group, dataset, and committed datatype within the file */
    /* Create the group */
    grp = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate2");

    /* Commit the type inside the group */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit2(fid, "datatype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create the data space for the dataset. */
    dims[0] = DIM0;
    dims[1] = DIM1;
    dspace = H5Screate_simple(RANK, dims, NULL);
    CHECK(dspace, FAIL, "H5Screate_simple");

    /* Create the dataset. */
    dset = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Get ref counts for each object.  They should all be 1, since each object has a hard link. */
    ret = H5Oget_info(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");

    /* Increment each object's reference count. */
    ret = H5Oincr_refcount(grp);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dtype);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dset);
    CHECK(ret, FAIL, "H5Oincr_refcount");

    /* Get ref counts for each object.  They should all be 2 now. */
    ret = H5Oget_info(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info");

    /* Decrement the reference counts and check that they decrease back to 1. */
    ret = H5Odecr_refcount(grp);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dtype);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dset);
    CHECK(ret, FAIL, "H5Odecr_refcount");

    ret = H5Oget_info(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");

    /* Increment the reference counts and then close the file to make sure the increment is permanant */
    ret = H5Oincr_refcount(grp);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dtype);
    CHECK(ret, FAIL, "H5Oincr_refcount");
    ret = H5Oincr_refcount(dset);
    CHECK(ret, FAIL, "H5Oincr_refcount");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file and check that the reference counts were really incremented */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Oget_info(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 2, "reference count in H5Oget_info");

    /* Decrement the reference counts and close the file */
    ret = H5Odecr_refcount(grp);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dtype);
    CHECK(ret, FAIL, "H5Odecr_refcount");
    ret = H5Odecr_refcount(dset);
    CHECK(ret, FAIL, "H5Odecr_refcount");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file and check that the reference counts were really decremented */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Oget_info(fid, "group", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "datatype", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");
    ret = H5Oget_info(fid, "dataset", &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info");
    VERIFY(oinfo.rc, 1, "reference count in H5Oget_info");

    /* Close the IDs */
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Make sure that bogus IDs return errors properly */
    H5E_BEGIN_TRY {
        ret = H5Oincr_refcount(grp);
        VERIFY(ret, FAIL, "H5Oincr_refcount");
        ret = H5Oincr_refcount(dtype);
        VERIFY(ret, FAIL, "H5Oincr_refcount");
        ret = H5Oincr_refcount(dset);
        VERIFY(ret, FAIL, "H5Oincr_refcount");
        ret = H5Odecr_refcount(grp);
        VERIFY(ret, FAIL, "H5Odecr_refcount");
        ret = H5Odecr_refcount(dtype);
        VERIFY(ret, FAIL, "H5Odecr_refcount");
        ret = H5Odecr_refcount(dset);
        VERIFY(ret, FAIL, "H5Odecr_refcount");
    } H5E_END_TRY

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_h5o_refcount() */


/****************************************************************
**
**  test_h5o_plist(): Test object creation properties
**
****************************************************************/
static void
test_h5o_plist(void)
{
    hid_t       fid;                        /* HDF5 File ID      */
    hid_t       grp, dset, dtype, dspace;   /* Object identifiers */
    hid_t       fapl;                       /* File access property list */
    hid_t       gcpl, dcpl, tcpl;           /* Object creation properties */
    unsigned    def_max_compact, def_min_dense; /* Default phase change parameters */
    unsigned    max_compact, min_dense;         /* Actual phase change parameters */
    herr_t      ret;                        /* Value returned from API calls */

    /* Make a FAPL that uses the "use the latest version of the format" flag */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set the "use the latest version of the format" flag for creating objects in the file */
    ret = H5Pset_latest_format(fapl, TRUE);
    CHECK(ret, FAIL, "H5Pset_latest_format");

    /* Create a new HDF5 file */
    fid = H5Fcreate(TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create group, dataset & named datatype creation property lists */
    gcpl = H5Pcreate(H5P_GROUP_CREATE);
    CHECK(gcpl, FAIL, "H5Pcreate");
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");
    tcpl = H5Pcreate(H5P_DATATYPE_CREATE);
    CHECK(tcpl, FAIL, "H5Pcreate");

    /* Retrieve default attribute phase change values */
    ret = H5Pget_attr_phase_change(gcpl, &def_max_compact, &def_min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Set non-default attribute phase change values on each creation property list */
    ret = H5Pset_attr_phase_change(gcpl, def_max_compact + 1, def_min_dense - 1);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");
    ret = H5Pset_attr_phase_change(dcpl, def_max_compact + 1, def_min_dense - 1);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");
    ret = H5Pset_attr_phase_change(tcpl, def_max_compact + 1, def_min_dense - 1);
    CHECK(ret, FAIL, "H5Pset_attr_phase_change");

    /* Retrieve attribute phase change values on each creation property list and verify */
    ret = H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(tcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");

    /* Create a group, dataset, and committed datatype within the file,
     *  using the respective type of creation property lists.
     */

    /* Create the group anonymously and link it in */
    grp = H5Gcreate_anon(fid, gcpl, H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gcreate_anon");
    ret = H5Llink(fid, "group", grp, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Llink");

    /* Commit the type inside the group anonymously and link it in */
    dtype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(dtype, FAIL, "H5Tcopy");
    ret = H5Tcommit_anon(fid, dtype, tcpl, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit_anon");
    ret = H5Llink(fid, "datatype", dtype, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Llink");

    /* Create the dataspace for the dataset. */
    dspace = H5Screate(H5S_SCALAR);
    CHECK(dspace, FAIL, "H5Screate");

    /* Create the dataset anonymously and link it in */
    dset = H5Dcreate_anon(fid, H5T_NATIVE_INT, dspace, dcpl, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate_anon");
    ret = H5Llink(fid, "dataset", dset, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Llink");
    ret = H5Sclose(dspace);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close current creation property lists */
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(tcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Retrieve each object's creation property list */
    gcpl = H5Gget_create_plist(grp);
    CHECK(gcpl, FAIL, "H5Gget_create_plist");
    tcpl = H5Tget_create_plist(dtype);
    CHECK(dcpl, FAIL, "H5Tget_create_plist");
    dcpl = H5Dget_create_plist(dset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");

    /* Retrieve attribute phase change values on each creation property list and verify */
    ret = H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(tcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");

    /* Close current objects */
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(tcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file and check that the object creation properties persist */
    fid = H5Fopen(TEST_FILENAME, H5F_ACC_RDONLY, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Re-open objects */
    grp = H5Gopen2(fid, "group", H5P_DEFAULT);
    CHECK(grp, FAIL, "H5Gopen2");
    dtype = H5Topen2(fid, "datatype", H5P_DEFAULT);
    CHECK(dtype, FAIL, "H5Topen2");
    dset = H5Dopen2(fid, "dataset", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    /* Retrieve each object's creation property list */
    gcpl = H5Gget_create_plist(grp);
    CHECK(gcpl, FAIL, "H5Gget_create_plist");
    tcpl = H5Tget_create_plist(dtype);
    CHECK(dcpl, FAIL, "H5Tget_create_plist");
    dcpl = H5Dget_create_plist(dset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");

    /* Retrieve attribute phase change values on each creation property list and verify */
    ret = H5Pget_attr_phase_change(gcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");
    ret = H5Pget_attr_phase_change(tcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(max_compact, (def_max_compact + 1), "H5Pget_attr_phase_change");
    VERIFY(min_dense, (def_min_dense - 1), "H5Pget_attr_phase_change");

    /* Close current objects */
    ret = H5Pclose(gcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(tcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Tclose(dtype);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Close the FAPL */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
} /* test_h5o_plist() */


/****************************************************************
**
**  test_h5o(): Main H5O (generic object) testing routine.
**
****************************************************************/
void
test_h5o(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Objects\n"));

    test_h5o_open();		/* Test generic open function */
    test_h5o_open_by_addr();	/* Test opening objects by address */
    test_h5o_close();		/* Test generic close function */
    test_h5o_refcount();        /* Test incrementing and decrementing reference count */
    test_h5o_plist();           /* Test object creation properties */
} /* test_h5o() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_h5o
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	James Laird
 *              June 3, 2006
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_h5o(void)
{
    remove(TEST_FILENAME);
}
