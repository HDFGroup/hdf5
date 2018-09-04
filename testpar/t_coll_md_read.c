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
    hid_t       file_id = -1;
    hid_t       fapl_id = -1;
    hid_t       dset_id = -1;
    hid_t       dcpl_id = -1;
    hid_t       dxpl_id = -1;
    hid_t       fspace_id = -1;
    hid_t       mspace_id = -1;
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

    dataset_dims = malloc(PARTIAL_NO_SELECTION_DATASET_NDIMS * sizeof(*dataset_dims));
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

    data = calloc(1, count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) * sizeof(int));
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

    read_buf = malloc(count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) * sizeof(int));
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
        VRFY((!memcmp(data, read_buf, count[1] * (PARTIAL_NO_SELECTION_Y_DIM_SCALE * PARTIAL_NO_SELECTION_X_DIM_SCALE) * sizeof(int))), "memcmp succeeded");
    }

    if (dataset_dims) {
        free(dataset_dims);
        dataset_dims = NULL;
    }

    if (data) {
        free(data);
        data = NULL;
    }

    if (read_buf) {
        free(read_buf);
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
