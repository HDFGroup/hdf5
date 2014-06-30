/* 
 * h5ff_client_index.c: Client side test for index routines.
 */

#include <hdf5.h>

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

/* define filename for this app, and max size after username prepended */
#define FILENAME_APP "eff_index.h5"
#define NAME_SIZE 64

#define NTUPLES 256
static int my_rank = 0, my_size = 1;

static herr_t
write_dataset(hid_t file_id, const char *dataset_name,
        hsize_t total, hsize_t ncomponents, hid_t datatype_id,
        hsize_t ntuples, hsize_t start, void *buf, uint64_t req_version,
        hid_t estack_id)
{
    hid_t       dataset_id;
    hid_t       file_space_id, mem_space_id;
    hid_t       trans_id, rcxt_id = -1, trspl_id;
    hsize_t     dims[2] = {total, ncomponents};
    /* hsize_t     offset[2] = {start, 0}; */
    hsize_t     count[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    uint64_t    version = req_version;
    herr_t      ret;

    (void) start;

    /* acquire container version 1 - EXACT. */
    if(0 == my_rank) {
        rcxt_id = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);
    if (0 != my_rank) {
        rcxt_id = H5RCcreate(file_id, version);
    }
    assert(req_version == version);

    /* create transaction object */
    trans_id = H5TRcreate(file_id, rcxt_id, version + 1);
    assert(trans_id);

    trspl_id = H5Pcreate(H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, (unsigned int) my_size);
    assert(0 == ret);
    ret = H5TRstart(trans_id, trspl_id, estack_id);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Create the data space for the first dataset. */
    file_space_id = H5Screate_simple(rank, dims, NULL);
    assert(file_space_id);

    /* Create a dataset. */
    dataset_id = H5Dcreate_ff(file_id, dataset_name, datatype_id, file_space_id,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, trans_id, estack_id);
    assert(dataset_id);

    mem_space_id = H5Screate_simple(rank, count, NULL);
    assert(mem_space_id);

    /* Write the first dataset. */
    ret = H5Dwrite_ff(dataset_id, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf, trans_id, estack_id);
    assert(0 == ret);

    /* Close the data space for the first dataset. */
    ret = H5Sclose(mem_space_id);
    assert(0 == ret);

    /* Close the first dataset. */
    ret = H5Dclose_ff(dataset_id, estack_id);
    assert(0 == ret);

    ret = H5Sclose(file_space_id);
    assert(0 == ret);

    /* Finish transaction 0. */
    ret = H5TRfinish(trans_id, H5P_DEFAULT, NULL, estack_id);
    assert(0 == ret);

    /* release container version 0. */
    if (my_rank == 0) {
        ret = H5RCrelease(rcxt_id, estack_id);
        assert(0 == ret);
    }

    ret = H5RCclose(rcxt_id);
    assert(0 == ret);

    ret = H5TRclose(trans_id);
    assert(0 == ret);

    return ret;
}

static herr_t
create_index(hid_t file_id, const char *dataset_name, unsigned plugin_id,
        uint64_t req_version, hid_t estack_id)
{
    hid_t dataset_id, trans_id, rcxt_id = -1;
    hid_t trspl_id;
    uint64_t version = req_version;
    herr_t ret;

    /* acquire container version 1 - EXACT. */
    if(0 == my_rank) {
        rcxt_id = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);
    if (0 != my_rank) {
        rcxt_id = H5RCcreate(file_id, version);
    }
    assert(req_version == version);

    /* create transaction object */
    trans_id = H5TRcreate(file_id, rcxt_id, version + 1);
    assert(trans_id);

    trspl_id = H5Pcreate(H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, (unsigned int) my_size);
    assert(0 == ret);
    ret = H5TRstart(trans_id, trspl_id, estack_id);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    dataset_id = H5Dopen_ff(file_id, dataset_name, H5P_DEFAULT, rcxt_id,
            estack_id);

    /* Add indexing information */
    ret = H5Xcreate_ff(file_id, plugin_id, dataset_id, H5P_DEFAULT,
            trans_id, estack_id);
    assert(0 == ret);

    /* Close the first dataset. */
    ret = H5Dclose_ff(dataset_id, estack_id);
    assert(0 == ret);

    /* Finish transaction 0. */
    ret = H5TRfinish(trans_id, H5P_DEFAULT, NULL, estack_id);
    assert(0 == ret);

    /* release container version 0. */
    if (my_rank == 0) {
        ret = H5RCrelease(rcxt_id, estack_id);
        assert(0 == ret);
    }

    ret = H5RCclose(rcxt_id);
    assert(0 == ret);

    ret = H5TRclose(trans_id);
    assert(0 == ret);

    return ret;
}

static herr_t
write_incr(hid_t file_id, const char *dataset_name,
        hsize_t total, hsize_t ncomponents,
        hsize_t ntuples, hsize_t start, void *buf, uint64_t req_version,
        hid_t estack_id)
{
    hid_t       dataset_id;
    hid_t       file_space_id;
//    hsize_t     dims[2] = {total, ncomponents};
    hsize_t     offset[2] = {start, 0};
    hsize_t     count[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    herr_t      ret;
    uint64_t    version = req_version;
    int n;

    (void) total;

    /* do incremental updates */
    for (n = 0; n < my_size; n++) {
        if (my_rank == n) {
            hid_t tid, rid, mem_space_id;

            version = (uint64_t) (req_version + (uint64_t) n);
            rid = H5RCacquire(file_id, &version, H5P_DEFAULT, estack_id);
            assert((uint64_t )(req_version + (uint64_t) n) == version);

            dataset_id = H5Dopen_ff(file_id, dataset_name, H5P_DEFAULT, rid, estack_id);
            assert(dataset_id);

            file_space_id = H5Dget_space(dataset_id);
            assert(file_space_id);

            /* create transaction object */
            tid = H5TRcreate(file_id, rid, (uint64_t) (req_version + 1 + (uint64_t) n));
            assert(tid);
            ret = H5TRstart(tid, H5P_DEFAULT, estack_id);
            assert(0 == ret);

            fprintf(stderr, "Rank %d Doing its Updates with TR %lu RC %lu\n",
                    my_rank, req_version + 1 + (uint64_t) n,
                    req_version + (uint64_t) n);

            mem_space_id = H5Screate_simple(rank, count, NULL);
            assert(mem_space_id);
            /* write data to datasets */
            ret = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
            NULL, count, NULL);
            assert(0 == ret);
            ret = H5Dwrite_ff(dataset_id, H5T_NATIVE_FLOAT, mem_space_id,
                    file_space_id, H5P_DEFAULT, buf, tid, estack_id);
            assert(0 == ret);
            ret = H5Sclose(mem_space_id);
            assert(0 == ret);

            ret = H5TRfinish(tid, H5P_DEFAULT, NULL, estack_id);
            assert(0 == ret);
            H5TRclose(tid);
            ret = H5RCrelease(rid, H5_EVENT_STACK_NULL);
            assert(0 == ret);
            ret = H5RCclose(rid);

            /* Close the first dataset. */
            H5Sclose(file_space_id);
            ret = H5Dclose_ff(dataset_id, estack_id);
            assert(0 == ret);
        }
        MPI_Barrier(MPI_COMM_WORLD);
    }

    return ret;
}

static herr_t
query_and_view(hid_t file_id, const char *dataset_name, uint64_t req_version,
        hid_t estack_id)
{
    float query_lb, query_ub;
    hid_t  query_id1, query_id2;
    hid_t query_id;
    hid_t dataset_id;
    hid_t rcxt_id = -1;
    uint64_t version = req_version;
    herr_t ret;
    double t1, t2;

    /* Create a simple query */
    /* query = (39.1 < x < 42.1) || (295 < x < 298) */
    query_lb = (my_rank == 0) ? 38.8f : 295.f;
    query_ub = (my_rank == 0) ? 42.8f : 298.f;

    query_id1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_FLOAT, &query_lb);
    assert(query_id1);

    query_id2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_FLOAT, &query_ub);
    assert(query_id2);

    query_id = H5Qcombine(query_id1, H5Q_COMBINE_AND, query_id2);
    assert(query_id);

    if(0 == my_rank) {
        rcxt_id = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);
    if (0 != my_rank) {
        rcxt_id = H5RCcreate(file_id, version);
    }
    assert(req_version == version);

    if (0 == my_rank) {
        dataset_id = H5Dopen_ff(file_id, dataset_name, H5P_DEFAULT, rcxt_id,
                estack_id);

        t1 = MPI_Wtime();
        H5Dquery_ff(dataset_id, query_id, -1, rcxt_id);
        t2 = MPI_Wtime();

        printf("Query time: %lf ms\n", (t2 - t1) * 1000);

        ret = H5Dclose_ff(dataset_id, estack_id);
        assert(0 == ret);
    }

    /* release container version 2. */
    if (my_rank == 0) {
        ret = H5RCrelease(rcxt_id, estack_id);
        assert(0 == ret);
    }

    ret = H5RCclose(rcxt_id);
    assert(0 == ret);

    H5Qclose(query_id);
    H5Qclose(query_id2);
    H5Qclose(query_id1);

    return ret;
}

int
main(int argc, char **argv)
{
    unsigned plugin_id;
    char file_name[NAME_SIZE];
    char dataset_name[NAME_SIZE];
    hsize_t ntuples = NTUPLES;
    hsize_t ntuples_multiplier = 1;
    hsize_t ncomponents = 3;
    hsize_t start, total;
    float *data;
    hid_t file_id, fapl_id;
    hid_t estack_id = H5_EVENT_STACK_NULL;
    herr_t ret;
    uint64_t req_version;
    hsize_t i, j;
    int incr_update;

    sprintf(file_name, "%s_%s", getenv("USER"), "eff_file_index.h5");

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);

    if (argc < 3) {
        if (my_rank ==0) printf("Usage: %s <ntuples_multiplier> <plugin_id> <do incremental update>\n", argv[0]);
        exit(0);
    }
    ntuples_multiplier = (hsize_t) atoi(argv[1]);
    ntuples *= ntuples_multiplier;
    plugin_id = (unsigned) atoi(argv[2]);
    incr_update = atoi(argv[3]);

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* In this example write one dataset per process */
    memset(dataset_name, '\0', 64);
    sprintf(dataset_name, "D%d", my_rank);

    /* Initialize the dataset. */
    data = (float *) malloc(sizeof(float) * ncomponents * ntuples * (hsize_t) my_size);
    for (i = 0; i < ntuples * (hsize_t) my_size; i++) {
       for (j = 0; j < ncomponents; j++) {
           if (my_rank != 0)
               data[ncomponents * i + j] = 41.0f;
           else
               data[ncomponents * i + j] = (float) i;
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

    /* We write to separate datasets */
    start = 0;
    total = ntuples * (hsize_t) my_size;

    req_version = 1;
    write_dataset(file_id, dataset_name, total, ncomponents, H5T_NATIVE_FLOAT,
            ntuples * (hsize_t) my_size, start, data, req_version, estack_id);

    MPI_Barrier(MPI_COMM_WORLD);

    req_version++;
    create_index(file_id, dataset_name, plugin_id, req_version, estack_id);

    MPI_Barrier(MPI_COMM_WORLD);

    req_version++;
    query_and_view(file_id, dataset_name, req_version, estack_id);

    MPI_Barrier(MPI_COMM_WORLD);

    if (incr_update) {
        start = ntuples * (hsize_t) my_rank;
        total = ntuples * (hsize_t) my_size;

        write_incr(file_id, "D0", total, ncomponents, ntuples, start, data,
                req_version, estack_id);

        MPI_Barrier(MPI_COMM_WORLD);

        req_version += (uint64_t) my_size;
        query_and_view(file_id, "D0", req_version, estack_id);

        MPI_Barrier(MPI_COMM_WORLD);
    }

    /* Close the file. */
    ret = H5Fclose_ff(file_id, 1, estack_id);
    assert(0 == ret);

    free(data);

    MPI_Barrier(MPI_COMM_WORLD);

    EFF_finalize();

    MPI_Finalize();

    if (ret < 0) {

    }
    return 0;
}
