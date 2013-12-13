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
        "  print 'Split sum: ' + str(array.sum())\n"
        "  return np.array([array.sum(), array.size])\n";

const char *combine_script =
        "import numpy as np\n"
        "def combine(arrays):\n"
        "  global_sum = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "  print 'Combined sum: ' + str(global_sum)\n"
        "  return np.array([global_sum, len(arrays)])\n";

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
#ifdef USE_NATIVE
    H5Pset_fapl_mpio(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);
#else
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);
#endif

    /* Open an existing file. */
#ifdef USE_NATIVE
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
#else
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id,
            H5_EVENT_STACK_NULL);
#endif

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

#ifndef USE_NATIVE
    /* acquire container version 0 - EXACT. */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);

    trspl_id = H5Pcreate(H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, (unsigned int) my_size);
    assert(0 == ret);
    ret = H5TRstart(tid1, trspl_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);
#endif

    /* Create the data space for the first dataset. */
    file_space_id = H5Screate_simple(rank, dims, NULL);
    assert(file_space_id);

#ifdef USE_NATIVE
    dataset_id = H5Dcreate(file_id, dataset_name, datatype_id, file_space_id,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset_id);
#else
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

        dataset_id = H5Oopen_by_token(dset_token1, rid1, H5_EVENT_STACK_NULL);
    }
    free(dset_token1);
#endif

    mem_space_id = H5Screate_simple(rank, count, NULL);
    assert(mem_space_id);

    /* write data to datasets */
    ret = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
           NULL, count, NULL);
    assert(0 == ret);

    /* Write the first dataset. */
#ifdef USE_NATIVE
    ret = H5Dwrite(dataset_id, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf);
    assert(0 == ret);
#else
    ret = H5Dwrite_ff(dataset_id, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf, tid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);
#endif

    /* Close the data space for the first dataset. */
    ret = H5Sclose(mem_space_id);
    assert(0 == ret);

    /* Close the first dataset. */
#ifdef USE_NATIVE
    ret = H5Dclose(dataset_id);
    assert(0 == ret);
#else
    ret = H5Dclose_ff(dataset_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
#endif

    ret = H5Sclose(file_space_id);
    assert(0 == ret);

#ifndef USE_NATIVE
    /* Finish transaction 1. */
    ret = H5TRfinish(tid1, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    /* release container version 0. */
    ret = H5RCrelease(rid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5RCclose(rid1);
    assert(0 == ret);

    ret = H5TRclose(tid1);
    assert(0 == ret);
#endif

    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the file. */
#ifdef USE_NATIVE
    ret = H5Fclose(file_id);
    assert(0 == ret);
#else
    ret = H5Fclose_ff(file_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
#endif
}

static void
ship_analysis(const char *file_name, const char *dataset_name)
{
    double query_limit = 39.1;
    hid_t  query_id;
    herr_t ret;

    /* Create a simple query */
    query_id = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_DOUBLE, &query_limit);
    assert(query_id);

    /* Issue an anlysis shipping request */
    ret = H5ASexecute(file_name, dataset_name, query_id, split_script, combine_script,
            H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5Qclose(query_id);
    assert(0 == ret);
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

#ifndef USE_NATIVE
    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);
#endif

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    start = ntuples * (hsize_t) my_rank;
    total = ntuples * (hsize_t) my_size;

    /* Initialize the dataset. */
    data = (int *) malloc(sizeof(int) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++)
       for (j = 0; j < ncomponents; j++)
          data[ncomponents * i + j] = ncomponents * i + j;

    MPI_Barrier(MPI_COMM_WORLD);

    write_dataset(file_name, dataset_name, total, ncomponents, H5T_NATIVE_INT,
            ntuples, start, data);

    MPI_Barrier(MPI_COMM_WORLD);

    free(data);

#ifndef USE_NATIVE
    if(0 == my_rank) {
        ship_analysis(file_name, dataset_name);
    }

    EFF_finalize();
#endif
    MPI_Finalize();

    return 0;
}
