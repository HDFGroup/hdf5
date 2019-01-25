/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * A test suite to test HDF5's collective metadata read capabilities, as enabled
 * by making a call to H5Pset_all_coll_metadata_ops().
 */

#include "testphdf5.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Define the non-participating process as the "last"
 * rank to avoid any weirdness potentially caused by
 * an if (mpi_rank == 0) check.
 */
#define PARTIAL_NO_SELECTION_NO_SEL_PROCESS (mpi_rank == mpi_size - 1)
#define PARTIAL_NO_SELECTION_DATASET_NAME "partial_no_selection_dset"
#define PARTIAL_NO_SELECTION_DATASET_NDIMS 2
#define PARTIAL_NO_SELECTION_Y_DIM_SCALE 5
#define PARTIAL_NO_SELECTION_X_DIM_SCALE 5

#define MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS 2

#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_NO_SEL_PROCESS (mpi_rank == mpi_size - 1)
#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DATASET_NAME   "linked_chunk_io_sort_chunk_issue"
#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_Y_DIM_SCALE    20000
#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE     1
#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS           1

/*
 * A test for issue HDFFV-10501. A parallel hang was reported which occurred
 * in linked-chunk I/O when collective metadata reads are enabled and some ranks
 * do not have any selection in a dataset's dataspace, while others do. The ranks
 * which have no selection during the read/write operation called H5D__chunk_addrmap()
 * to retrieve the lowest chunk address, since we require that the read/write be done
 * in strictly non-decreasing order of chunk address. For version 1 and 2 B-trees,
 * this caused the non-participating ranks to issue a collective MPI_Bcast() call
 * which the other ranks did not issue, thus causing a hang.
 * 
 * However, since these ranks are not actually reading/writing anything, this call
 * can simply be removed and the address used for the read/write can be set to an
 * arbitrary number (0 was chosen).
 */
void test_partial_no_selection_coll_md_read(void)
{
    const char *filename;
    hsize_t    *dataset_dims = NULL;
    hsize_t     max_dataset_dims[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     sel_dims[1];
    hsize_t     chunk_dims[PARTIAL_NO_SELECTION_DATASET_NDIMS] = { PARTIAL_NO_SELECTION_Y_DIM_SCALE, PARTIAL_NO_SELECTION_X_DIM_SCALE };
    hsize_t     start[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     stride[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     count[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     block[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hid_t       file_id = H5I_INVALID_HID;
    hid_t       fapl_id = H5I_INVALID_HID;
    hid_t       dset_id = H5I_INVALID_HID;
    hid_t       dcpl_id = H5I_INVALID_HID;
    hid_t       dxpl_id = H5I_INVALID_HID;
    hid_t       fspace_id = H5I_INVALID_HID;
    hid_t       mspace_id = H5I_INVALID_HID;
    int         mpi_rank, mpi_size;
    void       *data = NULL;
    void       *read_buf = NULL;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    filename = GetTestParameters();

    fapl_id = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist succeeded");

    /*
     * Even though the testphdf5 framework currently sets collective metadata reads
     * on the FAPL, we call it here just to be sure this is futureproof, since
     * demonstrating this issue relies upon it.
     */
    VRFY((H5Pset_all_coll_metadata_ops(fapl_id, true) >= 0), "Set collective metadata reads succeeded");

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    dataset_dims = HDmalloc(PARTIAL_NO_SELECTION_DATASET_NDIMS * sizeof(*dataset_dims));
    VRFY((dataset_dims != NULL), "malloc succeeded");

    dataset_dims[0] = PARTIAL_NO_SELECTION_Y_DIM_SCALE * mpi_size;
    dataset_dims[1] = PARTIAL_NO_SELECTION_X_DIM_SCALE * mpi_size;
    max_dataset_dims[0] = H5S_UNLIMITED;
    max_dataset_dims[1] = H5S_UNLIMITED;

    fspace_id = H5Screate_simple(PARTIAL_NO_SELECTION_DATASET_NDIMS, dataset_dims, max_dataset_dims);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    /*
     * Set up chunking on the dataset in order to reproduce the problem.
     */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_chunk(dcpl_id, PARTIAL_NO_SELECTION_DATASET_NDIMS, chunk_dims) >= 0), "H5Pset_chunk succeeded");

    dset_id = H5Dcreate2(file_id, PARTIAL_NO_SELECTION_DATASET_NAME, H5T_NATIVE_INT, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");

    /*
     * Setup hyperslab selection to split the dataset among the ranks.
     *
     * The ranks will write rows across the dataset.
     */
    start[0] = PARTIAL_NO_SELECTION_Y_DIM_SCALE * mpi_rank;
    start[1] = 0;
    stride[0] = PARTIAL_NO_SELECTION_Y_DIM_SCALE;
    stride[1] = PARTIAL_NO_SELECTION_X_DIM_SCALE;
    count[0] = 1;
    count[1] = mpi_size;
    block[0] = PARTIAL_NO_SELECTION_Y_DIM_SCALE;
    block[1] = PARTIAL_NO_SELECTION_X_DIM_SCALE;

    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) >= 0), "H5Sselect_hyperslab succeeded");

    sel_dims[0] = count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE);

    mspace_id = H5Screate_simple(1, sel_dims, NULL);
    VRFY((mspace_id >= 0), "H5Screate_simple succeeded");

    data = HDcalloc(1, count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) * sizeof(int));
    VRFY((data != NULL), "calloc succeeded");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "H5Pcreate succeeded");

    /*
     * Enable collective access for the data transfer.
     */
    VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0), "H5Pset_dxpl_mpio succeeded");

    VRFY((H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl_id, data) >= 0), "H5Dwrite succeeded");

    VRFY((H5Fflush(file_id, H5F_SCOPE_GLOBAL) >= 0), "H5Fflush succeeded");

    /*
     * Ensure that linked-chunk I/O is performed since this is
     * the particular code path where the issue lies and we don't
     * want the library doing multi-chunk I/O behind our backs.
     */
    VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, H5FD_MPIO_CHUNK_ONE_IO) >= 0), "H5Pset_dxpl_mpio_chunk_opt succeeded");

    read_buf = HDmalloc(count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) * sizeof(int));
    VRFY((read_buf != NULL), "malloc succeeded");

    /*
     * Make sure to call H5Sselect_none() on the non-participating process.
     */
    if (PARTIAL_NO_SELECTION_NO_SEL_PROCESS) {
        VRFY((H5Sselect_none(fspace_id) >= 0), "H5Sselect_none succeeded");
        VRFY((H5Sselect_none(mspace_id) >= 0), "H5Sselect_none succeeded");
    }

    /*
     * Finally have each rank read their section of data back from the dataset.
     */
    VRFY((H5Dread(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl_id, read_buf) >= 0), "H5Dread succeeded");

    /*
     * Check data integrity just to be sure.
     */
    if (!PARTIAL_NO_SELECTION_NO_SEL_PROCESS) {
        VRFY((!HDmemcmp(data, read_buf, count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) * sizeof(int))), "memcmp succeeded");
    }

    if (dataset_dims) {
        HDfree(dataset_dims);
        dataset_dims = NULL;
    }

    if (data) {
        HDfree(data);
        data = NULL;
    }

    if (read_buf) {
        HDfree(read_buf);
        read_buf = NULL;
    }

    VRFY((H5Sclose(fspace_id) >= 0), "H5Sclose succeeded");
    VRFY((H5Sclose(mspace_id) >= 0), "H5Sclose succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "H5Dclose succeeded");
    VRFY((H5Pclose(fapl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");
}

/*
 * A test for HDFFV-10562 which attempts to verify that using multi-chunk
 * I/O with collective metadata reads enabled doesn't causes issues due to
 * collective metadata reads being made only by process 0 in H5D__chunk_addrmap().
 *
 * Failure in this test may either cause a hang, or, due to how the MPI calls
 * pertaining to this issue might mistakenly match up, may cause an MPI error
 * message similar to:
 *
 *  #008: H5Dmpio.c line 2546 in H5D__obtain_mpio_mode(): MPI_BCast failed
 *  major: Internal error (too specific to document in detail)
 *  minor: Some MPI function failed
 *  #009: H5Dmpio.c line 2546 in H5D__obtain_mpio_mode(): Message truncated, error stack:
 *PMPI_Bcast(1600)..................: MPI_Bcast(buf=0x1df98e0, count=18, MPI_BYTE, root=0, comm=0x84000006) failed
 *MPIR_Bcast_impl(1452).............:
 *MPIR_Bcast(1476)..................:
 *MPIR_Bcast_intra(1249)............:
 *MPIR_SMP_Bcast(1088)..............:
 *MPIR_Bcast_binomial(239)..........:
 *MPIDI_CH3U_Receive_data_found(131): Message from rank 0 and tag 2 truncated; 2616 bytes received but buffer size is 18
 *  major: Internal error (too specific to document in detail)
 *  minor: MPI Error String
 *
 */
void test_multi_chunk_io_addrmap_issue(void)
{
    const char *filename;
    hsize_t start[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t stride[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t count[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t block[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t dims[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS] = {10, 5};
    hsize_t chunk_dims[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS] = {5, 5};
    hsize_t max_dims[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hid_t file_id = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;
    hid_t dset_id = H5I_INVALID_HID;
    hid_t dcpl_id = H5I_INVALID_HID;
    hid_t dxpl_id = H5I_INVALID_HID;
    hid_t space_id = H5I_INVALID_HID;
    void *read_buf = NULL;
    int mpi_rank;
    int data[5][5] = { {0, 1, 2, 3, 4},
                       {0, 1, 2, 3, 4},
                       {0, 1, 2, 3, 4},
                       {0, 1, 2, 3, 4},
                       {0, 1, 2, 3, 4} };

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    filename = GetTestParameters();

    fapl_id = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist succeeded");

    /*
     * Even though the testphdf5 framework currently sets collective metadata reads
     * on the FAPL, we call it here just to be sure this is futureproof, since
     * demonstrating this issue relies upon it.
     */
    VRFY((H5Pset_all_coll_metadata_ops(fapl_id, true) >= 0), "Set collective metadata reads succeeded");

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    space_id = H5Screate_simple(MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS, dims, max_dims);
    VRFY((space_id >= 0), "H5Screate_simple succeeded");

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_chunk(dcpl_id, MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS, chunk_dims) >= 0), "H5Pset_chunk succeeded");

    dset_id = H5Dcreate2(file_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0), "H5Pset_dxpl_mpio succeeded");
    VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, H5FD_MPIO_CHUNK_MULTI_IO) >= 0), "H5Pset_dxpl_mpio_chunk_opt succeeded");

    start[1] = 0;
    stride[0] = stride[1] = 1;
    count[0] = count[1] = 5;
    block[0] = block[1] = 1;

    if (mpi_rank == 0)
        start[0] = 0;
    else
        start[0] = 5;

    VRFY((H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, stride, count, block) >= 0), "H5Sselect_hyperslab succeeded");
    if (mpi_rank != 0)
        VRFY((H5Sselect_none(space_id) >= 0), "H5Sselect_none succeeded");

    VRFY((H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, space_id, dxpl_id, data) >= 0), "H5Dwrite succeeded");

    VRFY((H5Fflush(file_id, H5F_SCOPE_GLOBAL) >= 0), "H5Fflush succeeded");

    read_buf = HDmalloc(50 * sizeof(int));
    VRFY((read_buf != NULL), "malloc succeeded");

    VRFY((H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id, read_buf) >= 0), "H5Dread succeeded");

    if (read_buf) {
        HDfree(read_buf);
        read_buf = NULL;
    }

    VRFY((H5Sclose(space_id) >= 0), "H5Sclose succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "H5Dclose succeeded");
    VRFY((H5Pclose(fapl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");
}

/*
 * A test for HDFFV-10562 which attempts to verify that using linked-chunk
 * I/O with collective metadata reads enabled doesn't cause issues due to
 * collective metadata reads being made only by process 0 in H5D__sort_chunk().
 *
 * NOTE: Due to the way that the threshold value which pertains to this test
 * is currently calculated within HDF5, there are several conditions that this
 * test must maintain. Refer to the function H5D__sort_chunk in H5Dmpio.c for
 * a better idea of why.
 *
 * Condition 1: We need to make sure that the test always selects every single
 * chunk in the dataset. It is fine if the selection is split up among multiple
 * ranks, but their combined selection must cover the whole dataset.
 *
 * Condition 2: The number of chunks in the dataset divided by the number of MPI
 * ranks must exceed or equal 10000. In other words, each MPI rank must be
 * responsible for 10000 or more unique chunks.
 *
 * Condition 3: This test will currently only be reliably reproducable for 2 or 3
 * MPI ranks. The threshold value calculated reduces to a constant 100 / mpi_size,
 * and is compared against a default value of 30%.
 *
 * Failure in this test may either cause a hang, or, due to how the MPI calls
 * pertaining to this issue might mistakenly match up, may cause an MPI error
 * message similar to:
 *
 *  #008: H5Dmpio.c line 2338 in H5D__sort_chunk(): MPI_BCast failed
 *  major: Internal error (too specific to document in detail)
 *  minor: Some MPI function failed
 *  #009: H5Dmpio.c line 2338 in H5D__sort_chunk(): Other MPI error, error stack:
 *PMPI_Bcast(1600)........: MPI_Bcast(buf=0x7eae610, count=320000, MPI_BYTE, root=0, comm=0x84000006) failed
 *MPIR_Bcast_impl(1452)...:
 *MPIR_Bcast(1476)........:
 *MPIR_Bcast_intra(1249)..:
 *MPIR_SMP_Bcast(1088)....:
 *MPIR_Bcast_binomial(250): message sizes do not match across processes in the collective routine: Received 2096 but expected 320000
 *  major: Internal error (too specific to document in detail)
 *  minor: MPI Error String
 */
void test_link_chunk_io_sort_chunk_issue(void)
{
    const char *filename;
    hsize_t *dataset_dims = NULL;
    hsize_t  max_dataset_dims[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t  sel_dims[1];
    hsize_t  chunk_dims[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS] = { LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS };
    hsize_t  start[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t  stride[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t  count[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t  block[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hid_t    file_id = H5I_INVALID_HID;
    hid_t    fapl_id = H5I_INVALID_HID;
    hid_t    dset_id = H5I_INVALID_HID;
    hid_t    dcpl_id = H5I_INVALID_HID;
    hid_t    dxpl_id = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    int      mpi_rank, mpi_size;
    void    *data = NULL;
    void    *read_buf = NULL;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    filename = GetTestParameters();

    fapl_id = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist succeeded");

    /*
     * Even though the testphdf5 framework currently sets collective metadata reads
     * on the FAPL, we call it here just to be sure this is futureproof, since
     * demonstrating this issue relies upon it.
     */
    VRFY((H5Pset_all_coll_metadata_ops(fapl_id, true) >= 0), "Set collective metadata reads succeeded");

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    dataset_dims = HDmalloc(LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS * sizeof(*dataset_dims));
    VRFY((dataset_dims != NULL), "malloc succeeded");

    dataset_dims[0] = LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE * mpi_size * LINK_CHUNK_IO_SORT_CHUNK_ISSUE_Y_DIM_SCALE;
    max_dataset_dims[0] = H5S_UNLIMITED;

    fspace_id = H5Screate_simple(LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS, dataset_dims, max_dataset_dims);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    /*
     * Set up chunking on the dataset in order to reproduce the problem.
     */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_chunk(dcpl_id, LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS, chunk_dims) >= 0), "H5Pset_chunk succeeded");

    dset_id = H5Dcreate2(file_id, LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DATASET_NAME, H5T_NATIVE_INT, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");

    /*
     * Setup hyperslab selection to split the dataset among the ranks.
     *
     * The ranks will write rows across the dataset.
     */
    stride[0] = LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE;
    count[0] = (dataset_dims[0] / LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE) / mpi_size;
    start[0] = count[0] * mpi_rank;
    block[0] = LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE;

    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) >= 0), "H5Sselect_hyperslab succeeded");

    sel_dims[0] = count[0] * (LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE);

    mspace_id = H5Screate_simple(1, sel_dims, NULL);
    VRFY((mspace_id >= 0), "H5Screate_simple succeeded");

    data = HDcalloc(1, count[0] * (LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE) * sizeof(int));
    VRFY((data != NULL), "calloc succeeded");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "H5Pcreate succeeded");

    /*
     * Enable collective access for the data transfer.
     */
    VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0), "H5Pset_dxpl_mpio succeeded");

    VRFY((H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl_id, data) >= 0), "H5Dwrite succeeded");

    VRFY((H5Fflush(file_id, H5F_SCOPE_GLOBAL) >= 0), "H5Fflush succeeded");

    /*
     * Ensure that linked-chunk I/O is performed since this is
     * the particular code path where the issue lies and we don't
     * want the library doing multi-chunk I/O behind our backs.
     */
    VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, H5FD_MPIO_CHUNK_ONE_IO) >= 0), "H5Pset_dxpl_mpio_chunk_opt succeeded");

    read_buf = HDmalloc(count[0] * (LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE) * sizeof(int));
    VRFY((read_buf != NULL), "malloc succeeded");

    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) >= 0), "H5Sselect_hyperslab succeeded");

    sel_dims[0] = count[0] * (LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE);

    VRFY((H5Sclose(mspace_id) >= 0), "H5Sclose succeeded");

    mspace_id = H5Screate_simple(1, sel_dims, NULL);
    VRFY((mspace_id >= 0), "H5Screate_simple succeeded");

    read_buf = HDrealloc(read_buf, count[0] * (LINK_CHUNK_IO_SORT_CHUNK_ISSUE_CHUNK_SIZE) * sizeof(int));
    VRFY((read_buf != NULL), "realloc succeeded");

    /*
     * Finally have each rank read their section of data back from the dataset.
     */
    VRFY((H5Dread(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl_id, read_buf) >= 0), "H5Dread succeeded");

    if (dataset_dims) {
        HDfree(dataset_dims);
        dataset_dims = NULL;
    }

    if (data) {
        HDfree(data);
        data = NULL;
    }

    if (read_buf) {
        HDfree(read_buf);
        read_buf = NULL;
    }

    VRFY((H5Sclose(fspace_id) >= 0), "H5Sclose succeeded");
    VRFY((H5Sclose(mspace_id) >= 0), "H5Sclose succeeded");
    VRFY((H5Pclose(dcpl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Pclose(dxpl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Dclose(dset_id) >= 0), "H5Dclose succeeded");
    VRFY((H5Pclose(fapl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");
}
