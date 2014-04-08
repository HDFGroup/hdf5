/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
 */

#include <hdf5.h>

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

/* #define USE_NATIVE */

#define NTUPLES 256

static int my_rank = 0, my_size = 1;

/* Local sum and global sum */
const char *split_script =
        "import numpy as np\n"
        "def split(array):\n"
        "  print '--------------------'\n"
        "  print 'Split sum: ' + str(array.sum())\n"
        "  print 'Split average: ' + str(np.average(array))\n"
        "  print '--------------------'\n"
        "  return np.array([array.sum(), np.average(array)])\n";

const char *combine_script =
        "import numpy as np\n"
        "def combine(arrays):\n"
        "  global_sum = 0\n"
        "  global_average = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "    global_average += a[1]\n"
        "  global_average /= len(arrays)\n"
        "  print '--------------------'\n"
        "  print 'Combined sum: ' + str(global_sum)\n"
        "  print 'Combined average: ' + str(global_average)\n"
        "  print '--------------------'\n"
        "  return np.array([global_sum, global_average])\n";

static void
write_dataset(const char *file_name, const char *dataset_name,
        hsize_t total, hsize_t ncomponents, hid_t datatype_id,
        hsize_t ntuples, hsize_t start, void *buf)
{
    hid_t       file_id, dataset_id;
    hid_t       file_space_id, mem_space_id;
    hid_t       tid1, rid1, trspl_id;
    hid_t       fapl_id;
    hsize_t     dims[2] = {total, ncomponents};
    hsize_t     offset[2] = {start, 0};
    hsize_t     count[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    uint64_t    version;
    herr_t      ret;
    void       *dset_token1;
    size_t      token_size1;
    MPI_Request mpi_reqs[2];

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id,
                           H5_EVENT_STACK_NULL);
    assert(file_id > 0);

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

    /* acquire container version 1 - EXACT. */
    if(0 == my_rank) {
        version = 1;
        rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
        assert(1 == version);
    }
    MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
    if(0 != my_rank) {
        assert(version == 1);
        rid1 = H5RCcreate(file_id, version);
    }

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)2);
    assert(tid1);

    trspl_id = H5Pcreate(H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, (unsigned int) my_size);
    assert(0 == ret);
    ret = H5TRstart(tid1, trspl_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Create the data space for the first dataset. */
    file_space_id = H5Screate_simple(rank, dims, NULL);
    assert(file_space_id);

    if(0 == my_rank) {
        /* Create a dataset. */
        dataset_id = H5Dcreate_ff(file_id, dataset_name, datatype_id, file_space_id,
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
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[0]);
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[1]);
        MPI_Waitall(2, mpi_reqs, MPI_STATUS_IGNORE);
    }
    else {
        /* recieve the token size */
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[0]);
        MPI_Waitall(1, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffer for token */
        dset_token1 = malloc(token_size1);

        /* recieve the token */
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[0]);
        MPI_Waitall(1, mpi_reqs, MPI_STATUS_IGNORE);

        dataset_id = H5Oopen_by_token(dset_token1, tid1, H5_EVENT_STACK_NULL);
    }
    free(dset_token1);

    mem_space_id = H5Screate_simple(rank, count, NULL);
    assert(mem_space_id);

    /* write data to datasets */
    ret = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
           NULL, count, NULL);
    assert(0 == ret);

    ret = H5Dwrite_ff(dataset_id, datatype_id, mem_space_id, file_space_id,
                      H5P_DEFAULT, buf, tid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    /* Close the data space for the first dataset. */
    ret = H5Sclose(mem_space_id);
    assert(0 == ret);

    ret = H5Dclose_ff(dataset_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5Sclose(file_space_id);
    assert(0 == ret);

    /* Finish transaction 2. */
    ret = H5TRfinish(tid1, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    MPI_Barrier(MPI_COMM_WORLD);    
    if(0 == my_rank) {
        /* release container version 1. */
        ret = H5RCrelease(rid1, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    ret = H5RCclose(rid1);
    assert(0 == ret);

    ret = H5TRclose(tid1);
    assert(0 == ret);

    ret = H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);
    assert(0 == ret);
}

static void
ship_analysis(const char *file_name, const char *dataset_name)
{
    double lower_bound1 = 39.1, upper_bound1 = 42.1;
    int lower_bound2 = 295, upper_bound2 = 298;
    hid_t  query_id1, query_id2, query_id3, query_id4, query_id5, query_id6;
    hid_t query_id;
    herr_t ret;

    /* Create a simple query */
    /* query = (39.1 < x < 42.1) || (295 < x < 298) */
    query_id1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_DOUBLE, &lower_bound1);
    assert(query_id1);

    query_id2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_DOUBLE, &upper_bound1);
    assert(query_id2);

    query_id3 = H5Qcombine(query_id1, H5Q_COMBINE_AND, query_id2);
    assert(query_id3);

    query_id4 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_INT, &lower_bound2);
    assert(query_id4);

    query_id5 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_INT, &upper_bound2);
    assert(query_id5);

    query_id6 = H5Qcombine(query_id4, H5Q_COMBINE_AND, query_id5);
    assert(query_id6);

    query_id = H5Qcombine(query_id3, H5Q_COMBINE_OR, query_id6);
    assert(query_id);

    /* Issue an anlysis shipping request */
    ret = H5ASexecute(file_name, dataset_name, query_id, split_script, combine_script,
            H5_EVENT_STACK_NULL);
    assert(0 == ret);

    H5Qclose(query_id);
    H5Qclose(query_id6);
    H5Qclose(query_id5);
    H5Qclose(query_id4);
    H5Qclose(query_id3);
    H5Qclose(query_id2);
    H5Qclose(query_id1);
}

int
main(int argc, char **argv)
{
    const char *file_name="eff_analysis_file.h5";
    const char *dataset_name="D1";
    hsize_t ntuples = NTUPLES;
    hsize_t ncomponents = 3;
    hsize_t start, total;
    int *data;
    hsize_t i, j;

    int provided;

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

    start = ntuples * (hsize_t) my_rank;
    total = ntuples * (hsize_t) my_size;

    /* Initialize the dataset. */
    data = (int *) malloc(sizeof(int) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++) {
       for (j = 0; j < ncomponents; j++) {
          data[ncomponents * i + j] = my_rank * ntuples + i;
       }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    write_dataset(file_name, dataset_name, total, ncomponents, H5T_NATIVE_INT,
            ntuples, start, data);

    MPI_Barrier(MPI_COMM_WORLD);

    free(data);

    if(0 == my_rank) {
        ship_analysis(file_name, dataset_name);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    EFF_finalize();
    MPI_Finalize();

    return 0;
}
