/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * A test suite to test HDF5's collective metadata read and write capabilities,
 * as enabled by making a call to H5Pset_all_coll_metadata_ops() and/or
 * H5Pset_coll_metadata_write().
 */

#include "hdf5.h"
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
#define PARTIAL_NO_SELECTION_DATASET_NAME   "partial_no_selection_dset"
#define PARTIAL_NO_SELECTION_DATASET_NDIMS  2
#define PARTIAL_NO_SELECTION_Y_DIM_SCALE    5
#define PARTIAL_NO_SELECTION_X_DIM_SCALE    5

#define MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS 2

#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_COLL_THRESH_NUM 10000
#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DATASET_NAME    "linked_chunk_io_sort_chunk_issue"
#define LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS            1

#define COLL_GHEAP_WRITE_ATTR_NELEMS 10
#define COLL_GHEAP_WRITE_ATTR_NAME   "coll_gheap_write_attr"
#define COLL_GHEAP_WRITE_ATTR_DIMS   1

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
void
test_partial_no_selection_coll_md_read(void)
{
    const char *filename;
    hsize_t    *dataset_dims = NULL;
    hsize_t     max_dataset_dims[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     sel_dims[1];
    hsize_t     chunk_dims[PARTIAL_NO_SELECTION_DATASET_NDIMS] = {PARTIAL_NO_SELECTION_Y_DIM_SCALE,
                                                              PARTIAL_NO_SELECTION_X_DIM_SCALE};
    hsize_t     start[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     stride[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     count[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hsize_t     block[PARTIAL_NO_SELECTION_DATASET_NDIMS];
    hid_t       file_id   = H5I_INVALID_HID;
    hid_t       fapl_id   = H5I_INVALID_HID;
    hid_t       dset_id   = H5I_INVALID_HID;
    hid_t       dcpl_id   = H5I_INVALID_HID;
    hid_t       dxpl_id   = H5I_INVALID_HID;
    hid_t       fspace_id = H5I_INVALID_HID;
    hid_t       mspace_id = H5I_INVALID_HID;
    int         mpi_rank, mpi_size;
    void       *data     = NULL;
    void       *read_buf = NULL;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        if (MAINPROCESS) {
            puts("SKIPPED");
            printf("    API functions for basic file, dataset or file flush aren't supported with this "
                   "connector\n");
            fflush(stdout);
        }

        return;
    }

    filename = PARATESTFILE /* GetTestParameters() */;

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

    dataset_dims[0]     = (hsize_t)PARTIAL_NO_SELECTION_Y_DIM_SCALE * (hsize_t)mpi_size;
    dataset_dims[1]     = (hsize_t)PARTIAL_NO_SELECTION_X_DIM_SCALE * (hsize_t)mpi_size;
    max_dataset_dims[0] = H5S_UNLIMITED;
    max_dataset_dims[1] = H5S_UNLIMITED;

    fspace_id = H5Screate_simple(PARTIAL_NO_SELECTION_DATASET_NDIMS, dataset_dims, max_dataset_dims);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    /*
     * Set up chunking on the dataset in order to reproduce the problem.
     */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_chunk(dcpl_id, PARTIAL_NO_SELECTION_DATASET_NDIMS, chunk_dims) >= 0),
         "H5Pset_chunk succeeded");

    dset_id = H5Dcreate2(file_id, PARTIAL_NO_SELECTION_DATASET_NAME, H5T_NATIVE_INT, fspace_id, H5P_DEFAULT,
                         dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");

    /*
     * Setup hyperslab selection to split the dataset among the ranks.
     *
     * The ranks will write rows across the dataset.
     */
    start[0]  = (hsize_t)PARTIAL_NO_SELECTION_Y_DIM_SCALE * (hsize_t)mpi_rank;
    start[1]  = 0;
    stride[0] = PARTIAL_NO_SELECTION_Y_DIM_SCALE;
    stride[1] = PARTIAL_NO_SELECTION_X_DIM_SCALE;
    count[0]  = 1;
    count[1]  = (hsize_t)mpi_size;
    block[0]  = PARTIAL_NO_SELECTION_Y_DIM_SCALE;
    block[1]  = PARTIAL_NO_SELECTION_X_DIM_SCALE;

    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "H5Sselect_hyperslab succeeded");

    sel_dims[0] = count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE);

    mspace_id = H5Screate_simple(1, sel_dims, NULL);
    VRFY((mspace_id >= 0), "H5Screate_simple succeeded");

    data = HDcalloc(1, count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) *
                           sizeof(int));
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
    VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, H5FD_MPIO_CHUNK_ONE_IO) >= 0),
         "H5Pset_dxpl_mpio_chunk_opt succeeded");

    read_buf = HDmalloc(count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) *
                        sizeof(int));
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
    VRFY((H5Dread(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl_id, read_buf) >= 0),
         "H5Dread succeeded");

    /*
     * Check data integrity just to be sure.
     */
    if (!PARTIAL_NO_SELECTION_NO_SEL_PROCESS) {
        VRFY((!HDmemcmp(data, read_buf,
                        count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) *
                            sizeof(int))),
             "memcmp succeeded");
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
 *PMPI_Bcast(1600)..................: MPI_Bcast(buf=0x1df98e0, count=18, MPI_BYTE, root=0, comm=0x84000006)
 *failed MPIR_Bcast_impl(1452).............: MPIR_Bcast(1476)..................:
 *MPIR_Bcast_intra(1249)............:
 *MPIR_SMP_Bcast(1088)..............:
 *MPIR_Bcast_binomial(239)..........:
 *MPIDI_CH3U_Receive_data_found(131): Message from rank 0 and tag 2 truncated; 2616 bytes received but buffer
 *size is 18 major: Internal error (too specific to document in detail) minor: MPI Error String
 *
 */
void
test_multi_chunk_io_addrmap_issue(void)
{
    const char *filename;
    hsize_t     start[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t     stride[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t     count[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t     block[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS];
    hsize_t     dims[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS]       = {10, 5};
    hsize_t     chunk_dims[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS] = {5, 5};
    hsize_t     max_dims[MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS]   = {H5S_UNLIMITED, H5S_UNLIMITED};
    hid_t       file_id                                       = H5I_INVALID_HID;
    hid_t       fapl_id                                       = H5I_INVALID_HID;
    hid_t       dset_id                                       = H5I_INVALID_HID;
    hid_t       dcpl_id                                       = H5I_INVALID_HID;
    hid_t       dxpl_id                                       = H5I_INVALID_HID;
    hid_t       space_id                                      = H5I_INVALID_HID;
    void       *read_buf                                      = NULL;
    int         mpi_rank;
    int data[5][5] = {{0, 1, 2, 3, 4}, {0, 1, 2, 3, 4}, {0, 1, 2, 3, 4}, {0, 1, 2, 3, 4}, {0, 1, 2, 3, 4}};

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        if (MAINPROCESS) {
            puts("SKIPPED");
            printf("    API functions for basic file, dataset or file flush aren't supported with this "
                   "connector\n");
            fflush(stdout);
        }

        return;
    }

    filename = PARATESTFILE /* GetTestParameters() */;

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

    VRFY((H5Pset_chunk(dcpl_id, MULTI_CHUNK_IO_ADDRMAP_ISSUE_DIMS, chunk_dims) >= 0),
         "H5Pset_chunk succeeded");

    dset_id = H5Dcreate2(file_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_id >= 0), "H5Pcreate succeeded");

    VRFY((H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE) >= 0), "H5Pset_dxpl_mpio succeeded");
    VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, H5FD_MPIO_CHUNK_MULTI_IO) >= 0),
         "H5Pset_dxpl_mpio_chunk_opt succeeded");

    start[1]  = 0;
    stride[0] = stride[1] = 1;
    count[0] = count[1] = 5;
    block[0] = block[1] = 1;

    if (mpi_rank == 0)
        start[0] = 0;
    else
        start[0] = 5;

    VRFY((H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "H5Sselect_hyperslab succeeded");
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
 * is currently calculated within HDF5, the following two conditions must be
 * true to trigger the issue:
 *
 * Condition 1: A certain threshold ratio must be met in order to have HDF5
 * obtain all chunk addresses collectively inside H5D__sort_chunk(). This is
 * given by the following:
 *
 *     (sum_chunk * 100) / (dataset_nchunks * mpi_size) >= 30%
 *
 * where:
 *     * `sum_chunk` is the combined sum of the number of chunks selected in
 *       the dataset by all ranks (chunks selected by more than one rank count
 *       individually toward the sum for each rank selecting that chunk)
 *     * `dataset_nchunks` is the number of chunks in the dataset (selected
 *       or not)
 *     * `mpi_size` is the size of the MPI Communicator
 *
 * Condition 2: `sum_chunk` divided by `mpi_size` must exceed or equal a certain
 * threshold (as of this writing, 10000).
 *
 * To satisfy both these conditions, we #define a macro,
 * LINK_CHUNK_IO_SORT_CHUNK_ISSUE_COLL_THRESH_NUM, which corresponds to the
 * value of the H5D_ALL_CHUNK_ADDR_THRES_COL_NUM macro in H5Dmpio.c (the
 * 10000 threshold from condition 2). We then create a dataset of that many
 * chunks and have each MPI rank write to and read from a piece of every single
 * chunk in the dataset. This ensures chunk utilization is the max possible
 * and exceeds our 30% target ratio, while always exactly matching the numeric
 * chunk threshold value of condition 2.
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
 *MPIR_Bcast_binomial(250): message sizes do not match across processes in the collective routine: Received
 *2096 but expected 320000 major: Internal error (too specific to document in detail) minor: MPI Error String
 */
void
test_link_chunk_io_sort_chunk_issue(void)
{
    const char *filename;
    hsize_t     dataset_dims[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t     sel_dims[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t     chunk_dims[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t     start[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t     stride[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t     count[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hsize_t     block[LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS];
    hid_t       file_id   = H5I_INVALID_HID;
    hid_t       fapl_id   = H5I_INVALID_HID;
    hid_t       dset_id   = H5I_INVALID_HID;
    hid_t       dcpl_id   = H5I_INVALID_HID;
    hid_t       dxpl_id   = H5I_INVALID_HID;
    hid_t       fspace_id = H5I_INVALID_HID;
    hid_t       mspace_id = H5I_INVALID_HID;
    int         mpi_rank, mpi_size;
    void       *data     = NULL;
    void       *read_buf = NULL;

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        if (MAINPROCESS) {
            puts("SKIPPED");
            printf("    API functions for basic file, dataset or file flush aren't supported with this "
                   "connector\n");
            fflush(stdout);
        }

        return;
    }

    filename = PARATESTFILE /* GetTestParameters() */;

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

    /*
     * Create a one-dimensional dataset of exactly LINK_CHUNK_IO_SORT_CHUNK_ISSUE_COLL_THRESH_NUM
     * chunks, where every rank writes to a piece of every single chunk to keep utilization high.
     */
    dataset_dims[0] = (hsize_t)mpi_size * (hsize_t)LINK_CHUNK_IO_SORT_CHUNK_ISSUE_COLL_THRESH_NUM;

    fspace_id = H5Screate_simple(LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS, dataset_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    /*
     * Set up chunking on the dataset in order to reproduce the problem.
     */
    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl_id >= 0), "H5Pcreate succeeded");

    /* Chunk size is equal to MPI size since each rank writes to a piece of every chunk */
    chunk_dims[0] = (hsize_t)mpi_size;

    VRFY((H5Pset_chunk(dcpl_id, LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DIMS, chunk_dims) >= 0),
         "H5Pset_chunk succeeded");

    dset_id = H5Dcreate2(file_id, LINK_CHUNK_IO_SORT_CHUNK_ISSUE_DATASET_NAME, H5T_NATIVE_INT, fspace_id,
                         H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    VRFY((dset_id >= 0), "H5Dcreate2 succeeded");

    /*
     * Setup hyperslab selection to split the dataset among the ranks.
     */
    start[0]  = (hsize_t)mpi_rank;
    stride[0] = (hsize_t)mpi_size;
    count[0]  = LINK_CHUNK_IO_SORT_CHUNK_ISSUE_COLL_THRESH_NUM;
    block[0]  = 1;

    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "H5Sselect_hyperslab succeeded");

    sel_dims[0] = count[0];

    mspace_id = H5Screate_simple(1, sel_dims, NULL);
    VRFY((mspace_id >= 0), "H5Screate_simple succeeded");

    data = HDcalloc(1, count[0] * sizeof(int));
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
    VRFY((H5Pset_dxpl_mpio_chunk_opt(dxpl_id, H5FD_MPIO_CHUNK_ONE_IO) >= 0),
         "H5Pset_dxpl_mpio_chunk_opt succeeded");

    read_buf = HDmalloc(count[0] * sizeof(int));
    VRFY((read_buf != NULL), "malloc succeeded");

    VRFY((H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) >= 0),
         "H5Sselect_hyperslab succeeded");

    sel_dims[0] = count[0];

    VRFY((H5Sclose(mspace_id) >= 0), "H5Sclose succeeded");

    mspace_id = H5Screate_simple(1, sel_dims, NULL);
    VRFY((mspace_id >= 0), "H5Screate_simple succeeded");

    /*
     * Finally have each rank read their section of data back from the dataset.
     */
    VRFY((H5Dread(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl_id, read_buf) >= 0),
         "H5Dread succeeded");

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
 * A test for GitHub issue #2433 which causes a collective metadata write
 * of global heap data. This test is meant to ensure that global heap data
 * gets correctly mapped as raw data during a collective metadata write
 * using vector I/O.
 *
 * An assertion exists in the library that should be triggered if global
 * heap data is not correctly mapped as raw data.
 */
void
test_collective_global_heap_write(void)
{
    const char *filename;
    hsize_t     attr_dims[COLL_GHEAP_WRITE_ATTR_DIMS];
    hid_t       file_id   = H5I_INVALID_HID;
    hid_t       fapl_id   = H5I_INVALID_HID;
    hid_t       attr_id   = H5I_INVALID_HID;
    hid_t       vl_type   = H5I_INVALID_HID;
    hid_t       fspace_id = H5I_INVALID_HID;
    hvl_t       vl_data;
    int         mpi_rank, mpi_size;
    int         data_buf[COLL_GHEAP_WRITE_ATTR_NELEMS];

    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        if (MAINPROCESS) {
            puts("SKIPPED");
            printf("    API functions for basic file, dataset or file flush aren't supported with this "
                   "connector\n");
            fflush(stdout);
        }

        return;
    }

    filename = PARATESTFILE /* GetTestParameters() */;

    fapl_id = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type);
    VRFY((fapl_id >= 0), "create_faccess_plist succeeded");

    /*
     * Even though the testphdf5 framework currently sets collective metadata
     * writes on the FAPL, we call it here just to be sure this is futureproof,
     * since demonstrating this issue relies upon it.
     */
    VRFY((H5Pset_coll_metadata_write(fapl_id, true) >= 0), "Set collective metadata writes succeeded");

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    VRFY((file_id >= 0), "H5Fcreate succeeded");

    attr_dims[0] = 1;

    fspace_id = H5Screate_simple(COLL_GHEAP_WRITE_ATTR_DIMS, attr_dims, NULL);
    VRFY((fspace_id >= 0), "H5Screate_simple succeeded");

    vl_type = H5Tvlen_create(H5T_NATIVE_INT);
    VRFY((vl_type >= 0), "H5Tvlen_create succeeded");

    vl_data.len = COLL_GHEAP_WRITE_ATTR_NELEMS;
    vl_data.p   = data_buf;

    /*
     * Create a variable-length attribute that will get written to the global heap
     */
    attr_id = H5Acreate2(file_id, COLL_GHEAP_WRITE_ATTR_NAME, vl_type, fspace_id, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((attr_id >= 0), "H5Acreate2 succeeded");

    for (size_t i = 0; i < COLL_GHEAP_WRITE_ATTR_NELEMS; i++)
        data_buf[i] = (int)i;

    VRFY((H5Awrite(attr_id, vl_type, &vl_data) >= 0), "H5Awrite succeeded");

    VRFY((H5Sclose(fspace_id) >= 0), "H5Sclose succeeded");
    VRFY((H5Tclose(vl_type) >= 0), "H5Sclose succeeded");
    VRFY((H5Aclose(attr_id) >= 0), "H5Aclose succeeded");
    VRFY((H5Pclose(fapl_id) >= 0), "H5Pclose succeeded");
    VRFY((H5Fclose(file_id) >= 0), "H5Fclose succeeded");
}
