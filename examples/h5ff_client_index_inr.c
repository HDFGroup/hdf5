/* 
 * h5ff_client_index.c: Client side test for index routines.
 */

#include <hdf5.h>

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define NTUPLES 256
static int my_rank = 0, my_size = 1;

static void
query_and_view(hid_t file_id, hid_t dataset_id, hid_t rcxt_id, hid_t estack_id)
{
    float query_lb, query_ub;
    hid_t  query_id1, query_id2;
    hid_t query_id;
    herr_t ret;
    double t1, t2;

    /* Create a simple query */
    /* query = (39.1 < x < 42.1) || (295 < x < 298) */
    query_lb = (my_rank == 0) ? 38.8f : 295.f;
    query_ub = (my_rank == 0) ? 42.1f : 298.f;

    query_id1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_FLOAT, &query_lb);
    assert(query_id1);

    query_id2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_FLOAT, &query_ub);
    assert(query_id2);

    query_id = H5Qcombine(query_id1, H5Q_COMBINE_AND, query_id2);
    assert(query_id);

//    view_id = H5Vcreate_ff(dataset_id, query_id, H5P_DEFAULT, rid2,
//            estack_id);
//    assert(view_id > 0);

    t1 = MPI_Wtime();
    H5Dquery_ff(dataset_id, query_id, -1, rcxt_id);
    t2 = MPI_Wtime();

    printf("Query time: %lf ms\n", (t2 - t1) * 1000);
    /* TODO use view_id for analysis shipping etc */

//    H5Vclose(view_id);

    H5Qclose(query_id);
    H5Qclose(query_id2);
    H5Qclose(query_id1);
}

int
main(int argc, char **argv)
{
    unsigned plugin_id;
    char file_name[50];
    char dataset_name[64];
    hsize_t ntuples = NTUPLES;
    hsize_t ncomponents = 3;
    hsize_t start, total;
    float *data;
    hid_t file_id, fapl_id, dataset_id;
    hid_t file_space_id;
    hid_t tid2, rid1, trspl_id, rc_id;
    uint64_t version;
    void *dset_token;
    size_t token_size;
    hid_t estack_id = H5_EVENT_STACK_NULL;
    herr_t ret;
    hsize_t dims[2];
    int rank;
    hsize_t i, j;
    int n;

    sprintf(file_name, "%s_%s", getenv("USER"), "eff_file_index.h5");

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);

    if (argc < 2) {
        if (my_rank ==0) printf("Usage: %s <plugin_id>\n", argv[0]);
        exit(0);
    }
    plugin_id = atoi(argv[1]);

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* In this example write one dataset per process */
    memset(dataset_name, '\0', 64);
    sprintf(dataset_name, "indexed_dataset");

    start = ntuples * (hsize_t) my_rank;
    total = ntuples * (hsize_t) my_size;
    dims[0] = total;
    dims[1] = ncomponents;
    rank = (ncomponents == 1) ? 1 : 2;

    data = (float *) malloc(sizeof(float) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++) {
       for (j = 0; j < ncomponents; j++) {
          data[ncomponents * i + j] = (float) (((hsize_t) my_rank) * ntuples + i);
       }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Open an existing file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id,
            estack_id);

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

    /* acquire container version 1 - EXACT. */
    if(0 == my_rank) {
        version = 1;
        rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, estack_id);
        assert(1 == version);
    }
    MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
    if(0 != my_rank) {
        assert(version == 1);
        rid1 = H5RCcreate(file_id, version);
    }

    /* create transaction object */
    tid2 = H5TRcreate(file_id, rid1, (uint64_t)2);
    assert(tid2);

    trspl_id = H5Pcreate(H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, (unsigned int) my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, estack_id);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Create the data space for the first dataset. */
    file_space_id = H5Screate_simple(rank, dims, NULL);
    assert(file_space_id);

    if(0 == my_rank) {
        /* Create a dataset. */
        dataset_id = H5Dcreate_ff(file_id, dataset_name, H5T_NATIVE_FLOAT, file_space_id,
                                  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid2, estack_id);
        assert(dataset_id);

        /* get the token size of each dset */
        ret = H5Oget_token(dataset_id, NULL, &token_size);
        assert(0 == ret);
        /* allocate buffers for each token */
        dset_token = malloc(token_size);
        /* get the token buffer */
        ret = H5Oget_token(dataset_id, dset_token, &token_size);
        assert(0 == ret);
    }

    /* bcast the token sizes and the tokens */ 
    MPI_Bcast(&token_size, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD);

    if(0 != my_rank)
        dset_token = malloc(token_size);

    MPI_Bcast(dset_token, token_size, MPI_BYTE, 0, MPI_COMM_WORLD);

    if(0 != my_rank)
        dataset_id = H5Oopen_by_token(dset_token, tid2, estack_id);

    /* Add indexing information */
    //ret = H5Xcreate_ff(file_id, plugin_id, dataset_id, H5P_DEFAULT, tid2, estack_id);
    //assert(0 == ret);

    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, estack_id);
    assert(0 == ret);
    H5TRclose(tid2);

    if(0 == my_rank) {
        ret = H5RCrelease(rid1, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }
    H5RCclose(rid1);

    MPI_Barrier(MPI_COMM_WORLD);

    /* do incremental updates */
    for(n=0; n<my_size ; n++) {
        if(my_rank == n) {
            hid_t tid, rid, mem_space_id;
            hsize_t count[2] = {ntuples, ncomponents};
            hsize_t offset[2] = {start, 0};

            version = (uint64_t)(2+n);
            rid = H5RCacquire(file_id, &version, H5P_DEFAULT, estack_id);
            assert((uint64_t)(2+n) == version);

            /* create transaction object */
            tid = H5TRcreate(file_id, rid, (uint64_t)(3+n));
            assert(tid);
            ret = H5TRstart(tid, H5P_DEFAULT, estack_id);
            assert(0 == ret);

            fprintf(stderr, "Rank %d Doing its Updates with TR %d RC %d\n",
                    my_rank, 3+n, 2+n);

            mem_space_id = H5Screate_simple(rank, count, NULL);
            assert(mem_space_id);
            /* write data to datasets */
            ret = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
                                      NULL, count, NULL);
            assert(0 == ret);
            ret = H5Dwrite_ff(dataset_id, H5T_NATIVE_FLOAT, mem_space_id, file_space_id,
                              H5P_DEFAULT, data, tid, estack_id);
            assert(0 == ret);
            ret = H5Sclose(mem_space_id);
            assert(0 == ret);

            ret = H5TRfinish(tid, H5P_DEFAULT, NULL, estack_id);
            assert(0 == ret);
            H5TRclose(tid);
            ret = H5RCrelease(rid, H5_EVENT_STACK_NULL);
            assert(0 == ret);
            ret = H5RCclose(rid);
        }
        MPI_Barrier(MPI_COMM_WORLD);
    }

    /* acquire last container version - EXACT. */
    if(0 == my_rank) {
        fprintf(stderr, "Rank 0 acquiring latest context: %d\n", 2+my_size);
        version = (uint64_t)(2+my_size);
        rc_id = H5RCacquire(file_id, &version, H5P_DEFAULT, estack_id);
        assert((uint64_t)(2+my_size) == version);
    }
    MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
    if(0 != my_rank) {
        assert(version == (uint64_t)(2+my_size));
        rc_id = H5RCcreate(file_id, version);
    }

    query_and_view(file_id, dataset_id, rc_id, estack_id);

    MPI_Barrier(MPI_COMM_WORLD);

    if(0 == my_rank) {
        ret = H5RCrelease(rc_id, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }
    H5RCclose(rc_id);

    /* Close. */
    H5Sclose(file_space_id);
    ret = H5Dclose_ff(dataset_id, estack_id);
    assert(0 == ret);
    ret = H5Fclose_ff(file_id, 1, estack_id);
    assert(0 == ret);

    free(data);

    MPI_Barrier(MPI_COMM_WORLD);

    EFF_finalize();

    MPI_Finalize();

    return 0;
}
