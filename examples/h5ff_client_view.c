/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
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
write_dataset(const char *file_name, const char *dataset_name,
        hsize_t total, hsize_t ncomponents, hid_t datatype_id,
        hsize_t ntuples, hsize_t start, void *buf)
{
    hid_t       file_id, view_id;
    hid_t       did1, did2, did3, gid1;
    hid_t       file_space_id, mem_space_id;
    hid_t       tid1, rid1, rid2, trspl_id;
    hid_t       fapl_id;
    hsize_t     dims[2] = {total, ncomponents};
    hsize_t     offset[2] = {start, 0};
    hsize_t     count[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    uint64_t    version;
    herr_t      ret;
    void        *dset_token1, *dset_token2, *dset_token3;
    size_t      token_size1, token_size2, token_size3;
    double lower_bound1 = 39.1, upper_bound1 = 42.1;
    int lower_bound2 = 295, upper_bound2 = 298;
    hid_t  query_id1, query_id2, query_id3, query_id4, query_id5, query_id6;
    hid_t query_id;
    MPI_Request mpi_reqs[6];

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Open an existing file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id,
            H5_EVENT_STACK_NULL);

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

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

    /* Create the data space for the first dataset. */
    file_space_id = H5Screate_simple(rank, dims, NULL);
    assert(file_space_id);

    if(0 == my_rank) {
        /* create a group */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                            tid1, H5_EVENT_STACK_NULL);
        assert(gid1 > 0);

        /* Create a dataset. */
        did1 = H5Dcreate_ff(gid1, "D1", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL);
        assert(did1);

        /* Create a dataset. */
        did2 = H5Dcreate_ff(gid1, "D2", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL);
        assert(did2);

        /* Create a dataset. */
        did3 = H5Dcreate_ff(gid1, "D3", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL);
        assert(did3);

        ret = H5Gclose_ff(gid1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        /* get the token size of each dset */
        ret = H5Oget_token(did1, NULL, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, NULL, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(did3, NULL, &token_size3);
        assert(0 == ret);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);
        dset_token3 = malloc(token_size3);

        /* get the token buffer */
        ret = H5Oget_token(did1, dset_token1, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, dset_token2, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(did3, dset_token3, &token_size3);
        assert(0 == ret);

        /* bcast the token sizes and the tokens */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[2]);

        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[3]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[4]);
        MPI_Ibcast(dset_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[5]);
        MPI_Waitall(6, mpi_reqs, MPI_STATUS_IGNORE);
    }
    else {
        /* recieve the token size */
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffer for token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);
        dset_token3 = malloc(token_size3);

        /* recieve the token */
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[0]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[1]);
        MPI_Ibcast(dset_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD,
                &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        did1 = H5Oopen_by_token(dset_token1, tid1, H5_EVENT_STACK_NULL);
        did2 = H5Oopen_by_token(dset_token2, tid1, H5_EVENT_STACK_NULL);
        did3 = H5Oopen_by_token(dset_token3, tid1, H5_EVENT_STACK_NULL);
    }
    free(dset_token1);
    free(dset_token2);
    free(dset_token3);

    mem_space_id = H5Screate_simple(rank, count, NULL);
    assert(mem_space_id);

    /* write data to datasets */
    ret = H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, offset,
           NULL, count, NULL);
    assert(0 == ret);

    /* Write to the datasets. */
    ret = H5Dwrite_ff(did1, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf, tid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5Dwrite_ff(did2, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf, tid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5Dwrite_ff(did3, datatype_id, mem_space_id, file_space_id,
            H5P_DEFAULT, buf, tid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    /* Close the data space for the first dataset. */
    ret = H5Sclose(mem_space_id);
    assert(0 == ret);

    /* Finish transaction 1. */
    ret = H5TRfinish(tid1, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    MPI_Barrier(MPI_COMM_WORLD);
    /* acquire container version 1 - EXACT. */
    version = 1;
    rid2 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(rid2 > 0);
    assert(1 == version);

    /* release container version 0. */
    ret = H5RCrelease(rid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    MPI_Barrier(MPI_COMM_WORLD);

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

    /* create a view on D1 */
    view_id = H5Vcreate_ff(did1, query_id, H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
    assert(view_id > 0);

    {
        hsize_t attr_count, obj_count, reg_count;
        hssize_t num_points;
        hid_t region_space;
        int r_ndims;
        hsize_t r_dims[2];

        H5Qclose(query_id);
        ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5Vget_location_ff(view_id, &did1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5Vget_query(view_id, &query_id);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(0 == attr_count);
        assert(0 == obj_count);
        assert(1 == reg_count);

        ret = H5Vget_elem_regions_ff(view_id, 0, 1, &did1, 
                                     &region_space, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        r_ndims = H5Sget_simple_extent_dims(region_space, r_dims, NULL);

        assert(2 == r_ndims);
        assert(total == r_dims[0]);
        assert(ncomponents == r_dims[1]);

        num_points = H5Sget_select_elem_npoints(region_space);
        if(my_size > 1)
            assert(15 == num_points);
        else
            assert(9 == num_points);

        ret = H5Sclose(region_space);
        assert(0 == ret);
    }

    H5Vclose(view_id);

    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
    assert(gid1 > 0);

    /* create a view on all datasets under G1 */
    view_id = H5Vcreate_ff(gid1, query_id, H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
    assert(view_id > 0);

    {
        hsize_t attr_count, obj_count, reg_count, i;
        hssize_t num_points;
        hid_t regions[3];
        hid_t did[3];
        int r_ndims;
        hsize_t r_dims[2];

        H5Qclose(query_id);
        ret = H5Gclose_ff(gid1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5Vget_location_ff(view_id, &gid1, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        assert(gid1 > 0);

        ret = H5Vget_query(view_id, &query_id);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(0 == attr_count);
        assert(0 == obj_count);
        assert(3 == reg_count);

        ret = H5Vget_elem_regions_ff(view_id, 0, reg_count, did, regions, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        for(i=0 ; i<reg_count ; i++) {

            assert(did[i] > 0);
            ret = H5Dclose_ff(did[i], H5_EVENT_STACK_NULL);
            assert(0 == ret);

            r_ndims = H5Sget_simple_extent_dims(regions[i], r_dims, NULL);

            assert(2 == r_ndims);
            assert(total == r_dims[0]);
            assert(ncomponents == r_dims[1]);

            num_points = H5Sget_select_elem_npoints(regions[i]);
            if(my_size > 1)
                assert(15 == num_points);
            else
                assert(9 == num_points);

            ret = H5Sclose(regions[i]);
            assert(0 == ret);
        }
    }

    H5Vclose(view_id);

    H5Qclose(query_id);
    H5Qclose(query_id6);
    H5Qclose(query_id5);
    H5Qclose(query_id4);
    H5Qclose(query_id3);
    H5Qclose(query_id2);
    H5Qclose(query_id1);

    ret = H5Gclose_ff(gid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Dclose_ff(did2, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Dclose_ff(did3, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Sclose(file_space_id);
    assert(0 == ret);

    /* release container version 1. */
    ret = H5RCrelease(rid2, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);
    ret = H5TRclose(tid1);
    assert(0 == ret);

    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the file. */
    ret = H5Fclose_ff(file_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
}

int
main(int argc, char **argv)
{
    const char *file_name="eff_file_view.h5";
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

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
