/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
 */

#include <hdf5.h>

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define NTUPLES 32
#define NCOMP 3
static int my_rank = 0, my_size = 1;

static void
test_view(const char *file_name, void *buf)
{
    hid_t       file_id, view_id;
    hid_t       did1, did2, did3, did4, did5, did6;
    hid_t       gid1, gid2, gid3;
    hid_t       aid1, aid2, aid3;
    hid_t       file_space_id;
    hid_t       tid, rid1, rid2, trspl_id;
    hid_t       fapl_id;
    hsize_t     dims[2] = {NTUPLES, NCOMP};
    int         rank = 2;
    uint64_t    version;
    herr_t      ret;
    void        *dset_token1, *dset_token2, *dset_token3;
    size_t      token_size1, token_size2, token_size3;
    double      lower_bound1 = 9.1, upper_bound1 = 19.1;
    int lower_bound2 = 295, upper_bound2 = 298;
    hid_t  query_id1, query_id2, query_id3, query_id4, query_id5, query_id6;
    hid_t  query_id;
    hid_t  e_stack;
    H5ES_status_t status;
    size_t num_events = 0;
    hid_t datatype_id = H5T_NATIVE_INT;
    MPI_Request mpi_reqs[6];

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* create an event Queue for managing asynchronous requests. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* Open an existing file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id,
            H5_EVENT_STACK_NULL);

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

    if(0 == my_rank) {
        /* acquire container version 1 - EXACT. */
        version = 1;
        rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, e_stack);
        assert(1 == version);

        /* create transaction object */
        tid = H5TRcreate(file_id, rid1, (uint64_t)2);
        assert(tid);
        ret = H5TRstart(tid, H5P_DEFAULT, e_stack);
        assert(0 == ret);

        file_space_id = H5Screate_simple(rank, dims, NULL);
        assert(file_space_id);

        /* create a groups */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                            tid, e_stack);
        assert(gid1 > 0);
        aid1 = H5Acreate_ff(gid1, "ATTR1", datatype_id, file_space_id, 
                            H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(aid1);

        gid2 = H5Gcreate_ff(file_id, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                            tid, e_stack);
        assert(gid1 > 0);
        aid2 = H5Acreate_ff(gid2, "ATTR1", datatype_id, file_space_id, 
                            H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(aid2);

        gid3 = H5Gcreate_ff(file_id, "G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                            tid, e_stack);
        assert(gid1 > 0);
        aid3 = H5Acreate_ff(gid3, "ATTR3", datatype_id, file_space_id, 
                            H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(aid3);

        /* Create datasets */
        did1 = H5Dcreate_ff(gid1, "temp", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(did1);
        did2 = H5Dcreate_ff(gid1, "pres", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(did2);
        did3 = H5Dcreate_ff(gid2, "temp", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(did1);
        did4 = H5Dcreate_ff(gid2, "pres", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(did2);
        did5 = H5Dcreate_ff(gid3, "temp", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(did1);
        did6 = H5Dcreate_ff(gid3, "pres", datatype_id, file_space_id,
                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid, e_stack);
        assert(did2);

        /*write to attributes */
        ret = H5Awrite_ff(aid1, datatype_id, buf, tid, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Awrite_ff(aid2, datatype_id, buf, tid, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Awrite_ff(aid3, datatype_id, buf, tid, H5_EVENT_STACK_NULL);
        assert(ret == 0);

        /* Write to the datasets. */
        ret = H5Dwrite_ff(did1, datatype_id, H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, buf, tid, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        ret = H5Dwrite_ff(did2, datatype_id, H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, buf, tid, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        ret = H5Dwrite_ff(did3, datatype_id, H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, buf, tid, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        ret = H5Dwrite_ff(did4, datatype_id, H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, buf, tid, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        ret = H5Dwrite_ff(did5, datatype_id, H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, buf, tid, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        ret = H5Dwrite_ff(did6, datatype_id, H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, buf, tid, H5_EVENT_STACK_NULL);
        assert(0 == ret);


        ret = H5Gclose_ff(gid1, e_stack);
        assert(0 == ret);
        ret = H5Gclose_ff(gid2, e_stack);
        assert(0 == ret);
        ret = H5Gclose_ff(gid3, e_stack);
        assert(0 == ret);
        ret = H5Aclose_ff(aid1, e_stack);
        assert(0 == ret);
        ret = H5Aclose_ff(aid2, e_stack);
        assert(0 == ret);
        ret = H5Aclose_ff(aid3, e_stack);
        assert(0 == ret);
        ret = H5Dclose_ff(did1, e_stack);
        assert(0 == ret);
        ret = H5Dclose_ff(did2, e_stack);
        assert(0 == ret);
        ret = H5Dclose_ff(did3, e_stack);
        assert(0 == ret);
        ret = H5Dclose_ff(did4, e_stack);
        assert(0 == ret);
        ret = H5Dclose_ff(did5, e_stack);
        assert(0 == ret);
        ret = H5Dclose_ff(did6, e_stack);
        assert(0 == ret);

        ret = H5TRfinish(tid, H5P_DEFAULT, &rid2, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        ret = H5RCrelease(rid1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5RCclose(rid1);
        assert(0 == ret);
        ret = H5TRclose(tid);
        assert(0 == ret);

        ret = H5Sclose(file_space_id);
        assert(0 == ret);

        H5RCget_version(rid2, &version);
    }

    
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);
    assert(2 == version);

    if (my_rank != 0)
        rid2 = H5RCcreate(file_id, version);

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

    {
        hsize_t attr_count, obj_count, reg_count;
        hssize_t num_points;
        hid_t region_space;
        int r_ndims;
        hsize_t r_dims[2];

        did1 = H5Dopen_ff(file_id, "G1/temp", H5P_DEFAULT, rid2, e_stack);
        assert(did1);

        /* create a view on D1 */
        view_id = H5Vcreate_ff(did1, query_id3, H5P_DEFAULT, rid2, e_stack);
        assert(view_id > 0);

        H5Qclose(query_id3);
        ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5Vget_location_ff(view_id, &did1, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        ret = H5Vget_query(view_id, &query_id3);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(0 == attr_count);
        assert(0 == obj_count);
        assert(1 == reg_count);

        ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        ret = H5Vget_elem_regions_ff(view_id, 0, 1, &did1, 
                                     &region_space, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        r_ndims = H5Sget_simple_extent_dims(region_space, r_dims, NULL);

        assert(2 == r_ndims);
        assert(NTUPLES == r_dims[0]);
        assert(NCOMP == r_dims[1]);
        /*
        num_points = H5Sget_select_elem_npoints(region_space);
        if(my_size > 1)
            assert(15 == num_points);
        else
            assert(9 == num_points);
        */
        ret = H5Sclose(region_space);
        assert(0 == ret);

        ret = H5Dclose_ff(did1, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        H5Vclose(view_id);
    }
    MPI_Barrier(MPI_COMM_WORLD);

    query_id4 = H5Qcreate(H5Q_TYPE_LINK_NAME, H5Q_MATCH_EQUAL, "temp");
    assert(query_id4);
    query_id5 = H5Qcombine(query_id4, H5Q_COMBINE_AND, query_id3);
    assert(query_id5);

    {
        hsize_t attr_count, obj_count, reg_count, i;
        hssize_t num_points;
        hid_t regions[3];
        hid_t did[3], gid_temp;
        int r_ndims;
        hsize_t r_dims[2];

        /* create a view on all objects*/
        view_id = H5Vcreate_ff(file_id, query_id5, H5P_DEFAULT, rid2, e_stack);
        assert(view_id > 0);

        H5Qclose(query_id5);

        ret = H5Vget_location_ff(view_id, &gid_temp, e_stack);
        assert(0 == ret);
        assert(gid_temp > 0);

        ret = H5Gclose_ff(gid_temp, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        ret = H5Vget_query(view_id, &query_id5);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(0 == attr_count);
        assert(0 == obj_count);
        assert(3 == reg_count);

        ret = H5Vget_elem_regions_ff(view_id, 0, reg_count, did, regions, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        for(i=0 ; i<reg_count ; i++) {

            assert(did[i] > 0);
            ret = H5Dclose_ff(did[i], e_stack);
            assert(0 == ret);

            r_ndims = H5Sget_simple_extent_dims(regions[i], r_dims, NULL);

            assert(2 == r_ndims);
            assert(NTUPLES == r_dims[0]);
            assert(NCOMP == r_dims[1]);

            num_points = H5Sget_select_elem_npoints(regions[i]);
            /*
            if(my_size > 1)
                assert(15 == num_points);
            else
                assert(9 == num_points);
            */
            ret = H5Sclose(regions[i]);
            assert(0 == ret);
        }

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        H5Vclose(view_id);
    }

    H5Qclose(query_id5);
    H5Qclose(query_id4);
    H5Qclose(query_id3);
    H5Qclose(query_id2);
    H5Qclose(query_id1);
    MPI_Barrier(MPI_COMM_WORLD);

    query_id1 = H5Qcreate(H5Q_TYPE_LINK_NAME, H5Q_MATCH_NOT_EQUAL, "BLA");
    assert(query_id1);

    {
        hsize_t attr_count, obj_count, reg_count, i;
        hid_t objs[10], gid_temp;

        /* create a view on all objects*/
        view_id = H5Vcreate_ff(file_id, query_id1, H5P_DEFAULT, rid2, e_stack);
        assert(view_id > 0);

        H5Qclose(query_id1);

        ret = H5Vget_location_ff(view_id, &gid_temp, e_stack);
        assert(0 == ret);
        assert(gid_temp > 0);

        ret = H5Gclose_ff(gid_temp, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        ret = H5Vget_query(view_id, &query_id1);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(0 == attr_count);
        assert(10 == obj_count);
        assert(0 == reg_count);

        ret = H5Vget_objs_ff(view_id, 0, obj_count, objs, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        for(i=0 ; i<obj_count ; i++) {

            assert(objs[i] > 0);
            switch(H5Iget_type(objs[i])) {
            case H5I_DATASET:
                ret = H5Dclose_ff(objs[i], e_stack);
                assert(0 == ret);
                break;
            case H5I_GROUP:
                ret = H5Gclose_ff(objs[i], e_stack);
                assert(0 == ret);
                break;
            case H5I_ATTR:
                ret = H5Aclose_ff(objs[i], e_stack);
                assert(0 == ret);
                break;
            case H5I_MAP:
                ret = H5Mclose_ff(objs[i], e_stack);
                assert(0 == ret);
                break;
            default:
                assert(0);
            }
        }

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        H5Vclose(view_id);
    }

    H5Qclose(query_id1);
    MPI_Barrier(MPI_COMM_WORLD);

    query_id1 = H5Qcreate(H5Q_TYPE_ATTR_VALUE, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_DOUBLE, &lower_bound1);
    assert(query_id1);
    query_id2 = H5Qcreate(H5Q_TYPE_ATTR_VALUE, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_DOUBLE, &upper_bound1);
    assert(query_id2);
    query_id3 = H5Qcombine(query_id1, H5Q_COMBINE_AND, query_id2);
    assert(query_id3);

    {
        hsize_t attr_count, obj_count, reg_count, i;
        hid_t attrs[3];

        /* create a view */
        view_id = H5Vcreate_ff(file_id, query_id3, H5P_DEFAULT, rid2, e_stack);
        assert(view_id > 0);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        ret = H5Vget_query(view_id, &query_id3);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(3 == attr_count);
        assert(0 == obj_count);
        assert(0 == reg_count);

        ret = H5Vget_attrs_ff(view_id, 0, attr_count, attrs, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        for(i=0 ; i<attr_count ; i++) {
            assert(attrs[i] > 0);
            ret = H5Aclose_ff(attrs[i], e_stack);
            assert(ret == 0);
        }
        H5Vclose(view_id);
    }

    H5Qclose(query_id3);
    H5Qclose(query_id2);
    H5Qclose(query_id1);
    MPI_Barrier(MPI_COMM_WORLD);

    query_id1 = H5Qcreate(H5Q_TYPE_ATTR_VALUE, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_DOUBLE, &lower_bound1);
    assert(query_id1);
    query_id2 = H5Qcreate(H5Q_TYPE_ATTR_VALUE, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_DOUBLE, &upper_bound1);
    assert(query_id2);
    query_id3 = H5Qcombine(query_id1, H5Q_COMBINE_AND, query_id2);
    assert(query_id3);

    query_id4 = H5Qcreate(H5Q_TYPE_LINK_NAME, H5Q_MATCH_EQUAL, "G1");
    assert(query_id1);

    query_id5 = H5Qcombine(query_id4, H5Q_COMBINE_AND, query_id3);
    assert(query_id5);

    {
        hsize_t attr_count, obj_count, reg_count, i;
        hid_t attrs[2];

        /* create a view */
        view_id = H5Vcreate_ff(file_id, query_id5, H5P_DEFAULT, rid2, e_stack);
        assert(view_id > 0);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        ret = H5Vget_query(view_id, &query_id5);
        assert(0 == ret);

        ret = H5Vget_counts(view_id, &attr_count, &obj_count, &reg_count);
        assert(0 == ret);
        assert(1 == attr_count);
        assert(0 == obj_count);
        assert(0 == reg_count);

        ret = H5Vget_attrs_ff(view_id, 0, attr_count, attrs, e_stack);
        assert(0 == ret);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        for(i=0 ; i<attr_count ; i++) {
            assert(attrs[i] > 0);
            ret = H5Aclose_ff(attrs[i], e_stack);
            assert(ret == 0);
        }
        H5Vclose(view_id);
    }

    H5Qclose(query_id5);
    H5Qclose(query_id4);
    H5Qclose(query_id3);
    H5Qclose(query_id2);
    H5Qclose(query_id1);
    MPI_Barrier(MPI_COMM_WORLD);

    /* release container version 2. */
    if(0 == my_rank) {
        ret = H5RCrelease(rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    ret = H5RCclose(rid2);
    assert(0 == ret);

    ret = H5ESclose(e_stack);
    assert(ret == 0);

    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the file. */
    ret = H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);
    assert(0 == ret);
}

int
main(int argc, char **argv)
{
    char file_name[50];
    const char *dataset_name="D1";
    int *data;
    hsize_t i, j;

    int provided;

    sprintf(file_name, "%s_%s", getenv("USER"), "eff_file_view.h5");

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);
    MPI_Barrier(MPI_COMM_WORLD);

    /* Initialize the dataset. */
    data = (int *) malloc(sizeof(int) * NCOMP * NTUPLES);
    for (i = 0; i < NTUPLES; i++) {
       for (j = 0; j < NCOMP; j++) {
          data[NCOMP * i + j] = my_rank * NTUPLES + i;
       }
    }

    test_view(file_name, data);

    free(data);

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
