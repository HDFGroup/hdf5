/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Test parallel HDF5 basic components
 */

#include "testphdf5.h"

/*-------------------------------------------------------------------------
 * Function:    test_fapl_mpio_dup
 *
 * Purpose:     Test if fapl_mpio property list keeps a duplicate of the
 *              communicator and INFO objects given when set; and returns
 *              duplicates of its components when H5Pget_fapl_mpio is called.
 *
 * Return:      Success:    None
 *              Failure:    Abort
 *
 * Programmer:  Albert Cheng
 *              January 9, 2003
 *
 *-------------------------------------------------------------------------
 */
void
test_fapl_mpio_dup(void)
{
    int      mpi_size, mpi_rank;
    MPI_Comm comm, comm_tmp;
    int      mpi_size_old, mpi_rank_old;
    int      mpi_size_tmp, mpi_rank_tmp;
    MPI_Info info     = MPI_INFO_NULL;
    MPI_Info info_tmp = MPI_INFO_NULL;
    int      mrc;    /* MPI return value */
    hid_t    acc_pl; /* File access properties */
    herr_t   ret;    /* HDF5 return value */
    int      nkeys, nkeys_tmp;

    if (VERBOSE_MED)
        HDprintf("Verify fapl_mpio duplicates communicator and INFO objects\n");

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    if (VERBOSE_MED)
        HDprintf("rank/size of MPI_COMM_WORLD are %d/%d\n", mpi_rank, mpi_size);

    /* Create a new communicator that has the same processes as MPI_COMM_WORLD.
     * Use MPI_Comm_split because it is simplier than MPI_Comm_create
     */
    mrc = MPI_Comm_split(MPI_COMM_WORLD, 0, 0, &comm);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_split");
    MPI_Comm_size(comm, &mpi_size_old);
    MPI_Comm_rank(comm, &mpi_rank_old);
    if (VERBOSE_MED)
        HDprintf("rank/size of comm are %d/%d\n", mpi_rank_old, mpi_size_old);

    /* create a new INFO object with some trivial information. */
    mrc = MPI_Info_create(&info);
    VRFY((mrc == MPI_SUCCESS), "MPI_Info_create");
    mrc = MPI_Info_set(info, "hdf_info_name", "XYZ");
    VRFY((mrc == MPI_SUCCESS), "MPI_Info_set");
    if (MPI_INFO_NULL != info) {
        mrc = MPI_Info_get_nkeys(info, &nkeys);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_get_nkeys");
    }
    if (VERBOSE_MED)
        h5_dump_info_object(info);

    acc_pl = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((acc_pl >= 0), "H5P_FILE_ACCESS");

    ret = H5Pset_fapl_mpio(acc_pl, comm, info);
    VRFY((ret >= 0), "");

    /* Case 1:
     * Free the created communicator and INFO object.
     * Check if the access property list is still valid and can return
     * valid communicator and INFO object.
     */
    mrc = MPI_Comm_free(&comm);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_free");
    if (MPI_INFO_NULL != info) {
        mrc = MPI_Info_free(&info);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_free");
    }

    ret = H5Pget_fapl_mpio(acc_pl, &comm_tmp, &info_tmp);
    VRFY((ret >= 0), "H5Pget_fapl_mpio");
    MPI_Comm_size(comm_tmp, &mpi_size_tmp);
    MPI_Comm_rank(comm_tmp, &mpi_rank_tmp);
    if (VERBOSE_MED)
        HDprintf("After H5Pget_fapl_mpio: rank/size of comm are %d/%d\n", mpi_rank_tmp, mpi_size_tmp);
    VRFY((mpi_size_tmp == mpi_size), "MPI_Comm_size");
    VRFY((mpi_rank_tmp == mpi_rank), "MPI_Comm_rank");
    if (MPI_INFO_NULL != info_tmp) {
        mrc = MPI_Info_get_nkeys(info_tmp, &nkeys_tmp);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_get_nkeys");
        VRFY((nkeys_tmp == nkeys), "new and old nkeys equal");
    }
    if (VERBOSE_MED)
        h5_dump_info_object(info_tmp);

    /* Case 2:
     * Free the retrieved communicator and INFO object.
     * Check if the access property list is still valid and can return
     * valid communicator and INFO object.
     * Also verify the NULL argument option.
     */
    mrc = MPI_Comm_free(&comm_tmp);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_free");
    if (MPI_INFO_NULL != info_tmp) {
        mrc = MPI_Info_free(&info_tmp);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_free");
    }

    /* check NULL argument options. */
    ret = H5Pget_fapl_mpio(acc_pl, &comm_tmp, NULL);
    VRFY((ret >= 0), "H5Pget_fapl_mpio Comm only");
    mrc = MPI_Comm_free(&comm_tmp);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_free");

    ret = H5Pget_fapl_mpio(acc_pl, NULL, &info_tmp);
    VRFY((ret >= 0), "H5Pget_fapl_mpio Info only");
    if (MPI_INFO_NULL != info_tmp) {
        mrc = MPI_Info_free(&info_tmp);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_free");
    }

    ret = H5Pget_fapl_mpio(acc_pl, NULL, NULL);
    VRFY((ret >= 0), "H5Pget_fapl_mpio neither");

    /* now get both and check validity too. */
    /* Donot free the returned objects which are used in the next case. */
    ret = H5Pget_fapl_mpio(acc_pl, &comm_tmp, &info_tmp);
    VRFY((ret >= 0), "H5Pget_fapl_mpio");
    MPI_Comm_size(comm_tmp, &mpi_size_tmp);
    MPI_Comm_rank(comm_tmp, &mpi_rank_tmp);
    if (VERBOSE_MED)
        HDprintf("After second H5Pget_fapl_mpio: rank/size of comm are %d/%d\n", mpi_rank_tmp, mpi_size_tmp);
    VRFY((mpi_size_tmp == mpi_size), "MPI_Comm_size");
    VRFY((mpi_rank_tmp == mpi_rank), "MPI_Comm_rank");
    if (MPI_INFO_NULL != info_tmp) {
        mrc = MPI_Info_get_nkeys(info_tmp, &nkeys_tmp);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_get_nkeys");
        VRFY((nkeys_tmp == nkeys), "new and old nkeys equal");
    }
    if (VERBOSE_MED)
        h5_dump_info_object(info_tmp);

    /* Case 3:
     * Close the property list and verify the retrieved communicator and INFO
     * object are still valid.
     */
    H5Pclose(acc_pl);
    MPI_Comm_size(comm_tmp, &mpi_size_tmp);
    MPI_Comm_rank(comm_tmp, &mpi_rank_tmp);
    if (VERBOSE_MED)
        HDprintf("After Property list closed: rank/size of comm are %d/%d\n", mpi_rank_tmp, mpi_size_tmp);
    if (MPI_INFO_NULL != info_tmp) {
        mrc = MPI_Info_get_nkeys(info_tmp, &nkeys_tmp);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_get_nkeys");
    }
    if (VERBOSE_MED)
        h5_dump_info_object(info_tmp);

    /* clean up */
    mrc = MPI_Comm_free(&comm_tmp);
    VRFY((mrc == MPI_SUCCESS), "MPI_Comm_free");
    if (MPI_INFO_NULL != info_tmp) {
        mrc = MPI_Info_free(&info_tmp);
        VRFY((mrc == MPI_SUCCESS), "MPI_Info_free");
    }
} /* end test_fapl_mpio_dup() */
