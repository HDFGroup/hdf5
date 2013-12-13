/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

#define NX1 32
#define NY1 64

#define NX2 128
#define NY2 256

/* Local sum and global sum */
const char *split_sum_script =
        "import numpy as np\n"
        "def split(array):\n"
        "  print 'Split sum: ' + str(array.sum())\n"
        "  return np.array([array.sum(), array.size])\n";

const char *combine_sum_script =
        "import numpy as np\n"
        "def combine(arrays):\n"
        "  global_sum = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "  print 'Combined sum: ' + str(global_sum)\n"
        "  return np.array([global_sum, len(arrays)])\n";

int main(int argc, char **argv) {
    const char file_name[]="eff_file.h5";
    hid_t       tid1, rid1, trspl_id;
    hid_t       file_id, group_id, dataset_id, dataspace_id;  /* identifiers */
    hsize_t     dims[2];
    herr_t      status;
    int         i, j, dset1_data[NX1][NY1], dset2_data[NX2][NY2];
    herr_t ret;
    void *dset_token1;
    size_t token_size1;

    uint64_t version;
    uint64_t trans_num;

    double query_limit = 39.1;
    hid_t query_id;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_reqs[2];

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Initialize the first dataset. */
    for (i = 0; i < NX1; i++)
       for (j = 0; j < NY1; j++)
          dset1_data[i][j] = j + 1;

    /* Initialize the second dataset. */
    for (i = 0; i < NX2; i++)
       for (j = 0; j < NY2; j++)
          dset2_data[i][j] = j + 1;

    /* Open an existing file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL);

    /* acquire container version 0 - EXACT. */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);

    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid1, trspl_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Create the data space for the first dataset. */
    dims[0] = NX1;
    dims[1] = NY1;
    dataspace_id = H5Screate_simple(2, dims, NULL);

    if(0 == my_rank) {
        /* Create a dataset. */
        dataset_id = H5Dcreate_ff(file_id, "D1", H5T_NATIVE_INT, dataspace_id,
                                  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL);
        assert(dataset_id);

        /* get the token size of each dset */
        ret = H5Oget_token(dataset_id, NULL, &token_size1);
        assert(0 == ret);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);

        /* get the token buffer */
        ret = H5Oget_token(dataset_id, dset_token1, &token_size1);
        assert(0 == ret);

        /* bcast the token sizes and the tokens */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Waitall(2, mpi_reqs, MPI_STATUS_IGNORE);
    }
    else {
        /* recieve the token size */
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Waitall(1, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffer for token */
        dset_token1 = malloc(token_size1);

        /* recieve the token */
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Waitall(1, mpi_reqs, MPI_STATUS_IGNORE);

        dataset_id = H5Oopen_by_token(dset_token1, rid1, H5_EVENT_STACK_NULL);
    }

    free(dset_token1);

    /* write data to datasets */
    dataspace_id = H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, ,);

    /* Write the first dataset. */
    status = H5Dwrite_ff(dataset_id, H5T_NATIVE_INT, H5S_ALL, dataspace_id, H5P_DEFAULT,
            dset1_data, tid1, H5_EVENT_STACK_NULL);

    /* Close the data space for the first dataset. */
    status = H5Sclose(dataspace_id);

    /* Close the first dataset. */
    status = H5Dclose_ff(dataset_id, H5_EVENT_STACK_NULL);

    /* Finish transaction 1. */
    ret = H5TRfinish(tid1, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    /* release container version 0. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5TRclose(tid1);
    assert(0 == ret);

    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the file. */
    status = H5Fclose_ff(file_id, H5_EVENT_STACK_NULL);

//    /* Open an existing group of the specified file. */
//    group_id = H5Gcreate_ff(file_id, "/G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
//
//    /* Create the data space for the second dataset. */
//    dims[0] = NX2;
//    dims[1] = NY2;
//    dataspace_id = H5Screate_simple(2, dims, NULL);
//
//    /* Create the second dataset in group "/G1/G2". */
//    dataset_id = H5Dcreate_ff(group_id, "D2", H5T_NATIVE_INT, dataspace_id,
//                           H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
//
//    /* Write the second dataset. */
//    status = H5Dwrite_ff(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
//                      dset2_data, tid1, e_stack);
//
//    /* Close the data space for the second dataset. */
//    status = H5Sclose(dataspace_id);
//
//    /* Close the second dataset */
//    status = H5Dclose_ff(dataset_id, e_stack);
//
//    /* Close the group. */
//    status = H5Gclose_ff(group_id, e_stack);

    MPI_Barrier(MPI_COMM_WORLD);

    if(0 == my_rank) {
        query_id = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN, H5T_NATIVE_DOUBLE, &query_limit);

        H5ASexecute(file_name, "D1", query_id, split_script, combine_script, H5_EVENT_STACK_NULL);

        ret = H5Qclose(query_id);
        assert(0 == ret);
    }

    EFF_finalize();
    MPI_Finalize();

    return 0;
}
