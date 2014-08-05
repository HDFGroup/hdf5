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
        hsize_t ntuples, hsize_t start, void *buf)
{
    hid_t       dataset_id;
    hid_t       file_space_id, mem_space_id;
    hsize_t     dims[2] = {total, ncomponents};
    /* hsize_t     offset[2] = {start, 0}; */
    hsize_t     count[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    herr_t      ret;

    (void) start;

    /* Create the data space for the first dataset. */
    file_space_id = H5Screate_simple(rank, dims, NULL);
    assert(file_space_id);

    /* Create a dataset. */
    dataset_id = H5Dcreate(file_id, dataset_name, datatype_id, file_space_id,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(dataset_id);

    mem_space_id = H5Screate_simple(rank, count, NULL);
    assert(mem_space_id);

    /* Write the first dataset. */
    ret = H5Dwrite(dataset_id, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf);
    assert(0 == ret);

    /* Close the data space for the first dataset. */
    ret = H5Sclose(mem_space_id);
    assert(0 == ret);

    /* Close the first dataset. */
    ret = H5Dclose(dataset_id);
    assert(0 == ret);

    ret = H5Sclose(file_space_id);
    assert(0 == ret);

    return ret;
}

static herr_t
create_index(hid_t file_id, const char *dataset_name, unsigned plugin_id)
{
    hid_t dataset_id;
    herr_t ret;

    dataset_id = H5Dopen(file_id, dataset_name, H5P_DEFAULT);

    /* Add indexing information */
    ret = H5Xcreate(file_id, plugin_id, dataset_id, H5P_DEFAULT);
    assert(0 == ret);

    /* Close the first dataset. */
    ret = H5Dclose(dataset_id);
    assert(0 == ret);

    return ret;
}

static herr_t
write_incr(hid_t file_id, const char *dataset_name,
        hsize_t total, hsize_t ncomponents,
        hsize_t ntuples, hsize_t start, void *buf)
{
    hid_t       dataset_id;
    hid_t       file_space_id;
//    hsize_t     dims[2] = {total, ncomponents};
    hsize_t     offset[2] = {start, 0};
    hsize_t     count[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    herr_t      ret;
    int n;

    (void) total;

    /* do incremental updates */
    for (n = 0; n < my_size; n++) {
        if (my_rank == n) {
            hid_t mem_space_id;

            dataset_id = H5Dopen(file_id, dataset_name, H5P_DEFAULT);
            assert(dataset_id);

            file_space_id = H5Dget_space(dataset_id);
            assert(file_space_id);

            mem_space_id = H5Screate_simple(rank, count, NULL);
            assert(mem_space_id);
            /* write data to datasets */
            ret = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
            NULL, count, NULL);
            assert(0 == ret);
            ret = H5Dwrite(dataset_id, H5T_NATIVE_FLOAT, mem_space_id,
                    file_space_id, H5P_DEFAULT, buf);
            assert(0 == ret);
            ret = H5Sclose(mem_space_id);
            assert(0 == ret);

            /* Close the first dataset. */
            H5Sclose(file_space_id);
            ret = H5Dclose(dataset_id);
            assert(0 == ret);
        }
        MPI_Barrier(MPI_COMM_WORLD);
    }

    return ret;
}

static herr_t
query_and_view(hid_t file_id, const char *dataset_name)
{
    float query_lb, query_ub;
    hid_t  query_id1, query_id2;
    hid_t query_id;
    hid_t dataset_id;
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

    if (0 == my_rank) {
        hsize_t start_coord[H5S_MAX_RANK + 1], end_coord[H5S_MAX_RANK + 1];
        hsize_t nelmts;
        hid_t space_id;

        dataset_id = H5Dopen(file_id, dataset_name, H5P_DEFAULT);

        t1 = MPI_Wtime();
        H5Dquery(dataset_id, query_id, &space_id);
        t2 = MPI_Wtime();

        H5Sget_select_bounds(space_id, start_coord, end_coord);
        nelmts = (hsize_t) H5Sget_select_npoints(space_id);
        printf("Create dataspace with %llu elements, bounds = [(%llu, %llu):(%llu, %llu)]\n",
                nelmts, start_coord[0], start_coord[1], end_coord[0], end_coord[1]);
        printf("Index query time: %lf ms\n", (t2 - t1) * 1000);

        H5Sclose(space_id);
        ret = H5Dclose(dataset_id);
        assert(0 == ret);
    }

    ret = H5Qclose(query_id);
    assert(0 == ret);
    ret = H5Qclose(query_id2);
    assert(0 == ret);
    ret = H5Qclose(query_id1);
    assert(0 == ret);

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
    herr_t ret;
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

//    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

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
//    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Open an existing file. */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

    /* We write to separate datasets */
    start = 0;
    total = ntuples * (hsize_t) my_size;

    write_dataset(file_id, dataset_name, total, ncomponents, H5T_NATIVE_FLOAT,
            ntuples * (hsize_t) my_size, start, data);

    MPI_Barrier(MPI_COMM_WORLD);

    create_index(file_id, dataset_name, plugin_id);

    MPI_Barrier(MPI_COMM_WORLD);

    query_and_view(file_id, dataset_name);

    MPI_Barrier(MPI_COMM_WORLD);

    if (incr_update) {
        start = ntuples * (hsize_t) my_rank;
        total = ntuples * (hsize_t) my_size;

        write_incr(file_id, "D0", total, ncomponents, ntuples, start, data);

        MPI_Barrier(MPI_COMM_WORLD);

        query_and_view(file_id, "D0");

        MPI_Barrier(MPI_COMM_WORLD);
    }

    /* Close the file. */
    ret = H5Fclose(file_id);
    assert(0 == ret);

    free(data);

    MPI_Barrier(MPI_COMM_WORLD);

    MPI_Finalize();

    if (ret < 0) {

    }
    return 0;
}
