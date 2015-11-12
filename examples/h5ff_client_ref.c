/* 
 * h5ff_client_attr.c: Client side test for attribute routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"


int main(int argc, char **argv) {
    char file_name[50];
    hid_t file_id;
    hid_t gid1;
    hid_t sid, dtid;
    hid_t sid2;
    hid_t did1, map1;
    hid_t aid1, aid2, aid3, aid4, aid5;
    hid_t tid1, tid2, rid1, rid2, rid3;
    hid_t fapl_id, trspl_id;
    hid_t e_stack;
    htri_t exists1;
    htri_t exists2;

    uint64_t version;
    uint64_t trans_num;

    int *wdata1 = NULL, *wdata2 = NULL;
    int *rdata1 = NULL, *rdata2 = NULL;
    char str_data[128];
    const unsigned int nelem=60;
    hsize_t dims[1];

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req;

    uint32_t cs_scope = 0;
    size_t num_events = 0;
    H5ES_status_t status;
    unsigned int i = 0;
    herr_t ret;

    sprintf(file_name, "%s_%s", getenv("USER"), "eff_file_ref.h5");

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
    ret = H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);
    assert(0 == ret);

    /* set the metada data integrity checks to happend at transfer through mercury */
    cs_scope |= H5_CHECKSUM_TRANSFER;
    ret = H5Pset_metadata_integrity_scope(fapl_id, cs_scope);
    assert(ret == 0);

    /* create an event Queue for managing asynchronous requests. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* create the file. */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(file_id > 0);

    /* create 1-D dataspace with 60 elements */
    dims [0] = nelem;
    sid = H5Screate_simple(1, dims, NULL);

    dtid = H5Tcopy(H5T_STD_I32LE);

    /* start transaction 2 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        /* acquire container version 1 - EXACT.  
           This can be asynchronous, but here we need the acquired ID 
           right after the call to start the transaction so we make synchronous. */
        version = 1;
        rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);

        /* create transaction object */
        tid1 = H5TRcreate(file_id, rid1, (uint64_t)2);
        assert(tid1);
        ret = H5TRstart(tid1, H5P_DEFAULT, e_stack);
        assert(0 == ret);

        sid2 = H5Screate(H5S_SCALAR); assert( sid2 );

        /* create group /G1 */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, 
                            H5P_DEFAULT, tid1, e_stack);
        assert(gid1 > 0);

        /* create dataset /G1/D1 */
        did1 = H5Dcreate_ff(gid1, "D1", dtid, sid, H5P_DEFAULT, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did1 > 0);

        /* Commit the datatype dtid to the file. */
        ret = H5Tcommit_ff(file_id, "DT1", dtid, H5P_DEFAULT, H5P_DEFAULT, 
                           H5P_DEFAULT, tid1, e_stack);
        assert(ret == 0);

        /* create a Map object on the root group */
        map1 = H5Mcreate_ff(file_id, "MAP1", H5T_STD_I32LE, H5T_STD_I32LE, 
                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(map1 > 0);

        /* create an attribute on root group */
        aid1 = H5Acreate_ff(file_id, "ROOT_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid1 > 0);

        /* create an attribute on group G1. */
        aid2 = H5Acreate_ff(gid1, "GROUP_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid2 > 0);

        /* create an attribute on dataset */
        aid3 = H5Acreate_ff(did1, "DSET_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid3 > 0);

        /* create an attribute on datatype */
        aid4 = H5Acreate_ff(dtid, "DTYPE_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid4 > 0);

        aid5 = H5Acreate_ff(map1, "MAP_ATTR", dtid, sid2, H5P_DEFAULT, 
                            H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL );
        assert(aid5 > 0);

        H5Sclose(sid2);

        ret = H5Aclose_ff(aid1, e_stack);
        assert(ret == 0);
        ret = H5Aclose_ff(aid2, e_stack);
        assert(ret == 0);
        ret = H5Aclose_ff(aid3, e_stack);
        assert(ret == 0);
        ret = H5Aclose_ff(aid4, e_stack);
        assert(ret == 0);
        ret = H5Aclose_ff(aid5, e_stack);
        assert(ret == 0);
        ret = H5Dclose_ff(did1, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid1, e_stack);
        assert(ret == 0);

#if 0
        printf("OPEN DT by token:\n");
    {
        size_t token_size;
        void *token;
        hid_t new_id;

        H5Oget_token(dtid, NULL, &token_size);
        token = malloc(token_size);
        H5Oget_token(dtid, token, &token_size);

        new_id = H5Oopen_by_token(token, tid1, H5_EVENT_STACK_NULL);
        H5Tclose(new_id);
        free(token);
    }
#endif

        /* make this synchronous so we know the container version has been acquired */
        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        /* release container version 1. This is async. */
        ret = H5RCrelease(rid1, e_stack);
        assert(0 == ret);

        /* Local op */
        ret = H5TRclose(tid1);
        assert(0 == ret);
    }

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);
    if(0 != num_events) assert(status == H5ES_STATUS_SUCCEED);

    /* test creating internal references */
    {
        href_ff_t ref1, ref2, ref3, ref4, ref5, ref6;
        hsize_t start[1], count[1];
        hid_t dspace_region;
        H5O_type_t obj_type;

        ret = H5Rcreate_object_ff(&ref1, file_id, "/G1", H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref1);
        assert(ret == 0);

        ret = H5Rcreate_object_ff(&ref2, map1, ".", H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref2);
        assert(ret == 0);

        start[0] = 10;
        count[0] = 30;
        ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, NULL);
        assert(ret == 0);
        ret = H5Rcreate_region_ff(&ref3, file_id, "/G1/D1", sid, H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref3);
        assert(ret == 0);

        ret = H5Rcreate_attr_ff(&ref4, map1, ".", "Temperature", H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref4);
        assert(ret == 0);
        ret = H5Rcreate_attr_ff(&ref5, dtid, ".", "TYPE_SIZE1", H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref5);
        assert(ret == 0);
        ret = H5Rcreate_attr_ff(&ref6, file_id, "/DT1", "TYPE_SIZE2", H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref6);
        assert(ret == 0);

        H5E_BEGIN_TRY {
            dspace_region = H5Rget_region_ff(&ref1);
        } H5E_END_TRY;
        assert (dspace_region < 0);
        H5E_BEGIN_TRY {
            dspace_region = H5Rget_region_ff(&ref4);
        } H5E_END_TRY;
        assert (dspace_region < 0);

        dspace_region = H5Rget_region_ff(&ref3);
        assert(dspace_region > 0);
        assert(H5Sget_simple_extent_ndims(dspace_region) == 1);
        assert(H5Sget_select_npoints(dspace_region) == 30);
        ret = H5Sclose(dspace_region);
        assert(0 == ret);

        H5Rget_obj_type_ff(&ref1, &obj_type);
        assert(obj_type == H5O_TYPE_GROUP);
        H5Rget_obj_type_ff(&ref2, &obj_type);
        assert(obj_type == H5O_TYPE_MAP);
        H5Rget_obj_type_ff(&ref3, &obj_type);
        assert(obj_type == H5O_TYPE_DATASET);
        H5Rget_obj_type_ff(&ref6, &obj_type);
        assert(obj_type == H5O_TYPE_NAMED_DATATYPE);

        H5E_BEGIN_TRY {
            assert (H5Rget_filename_ff(&ref1, NULL, 0) < 0);
            assert (H5Rget_filename_ff(&ref3, NULL, 0) < 0);
            assert (H5Rget_filename_ff(&ref6, NULL, 0) < 0);
            assert (H5Rget_name_ff(&ref1, NULL, 0) < 0);
            assert (H5Rget_name_ff(&ref3, NULL, 0) < 0);
            assert (H5Rget_name_ff(&ref6, NULL, 0) < 0);
        } H5E_END_TRY;

        ret = H5Rdestroy_ff(&ref1);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref2);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref3);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref4);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref5);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref6);
        assert(ret == 0);
    }

    ret = H5Mclose_ff(map1, e_stack);
    assert(ret == 0);

    /* test creating external references */
    {
        href_ff_t ref1, ref2, ref3;
        hid_t dspace_region;
        hsize_t start[1], count[1];
        H5O_type_t obj_type;
        char fname[1024], pname[1024];

        ret = H5Rcreate_object_ext_ff(&ref1, "ref1_file", "/G1/G2/G3");
        assert(ret == 0);

        start[0] = 10;
        count[0] = 30;
        ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, NULL);
        assert(ret == 0);
        ret = H5Rcreate_region_ext_ff(&ref2, "ref2_file", "/G5/D4", sid);
        assert(ret == 0);

        ret = H5Rcreate_attr_ext_ff(&ref3, "ref3_file", "/G18/M6", "Temperature");
        assert(ret == 0);

        ret = H5Rprint_ref(&ref1);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref2);
        assert(ret == 0);
        ret = H5Rprint_ref(&ref3);
        assert(ret == 0);

        H5E_BEGIN_TRY {
            dspace_region = H5Rget_region_ff(&ref1);
        } H5E_END_TRY;
        assert (dspace_region < 0);
        H5E_BEGIN_TRY {
            dspace_region = H5Rget_region_ff(&ref3);
        } H5E_END_TRY;
        assert (dspace_region < 0);

        dspace_region = H5Rget_region_ff(&ref2);
        assert(dspace_region > 0);
        assert(H5Sget_simple_extent_ndims(dspace_region) == 1);
        assert(H5Sget_select_npoints(dspace_region) == 30);
        ret = H5Sclose(dspace_region);
        assert(0 == ret);

        H5E_BEGIN_TRY {
            ret = H5Rget_obj_type_ff(&ref1, &obj_type);
            assert (ret < 0);
            ret = H5Rget_obj_type_ff(&ref2, &obj_type);
            assert (ret < 0);
            ret = H5Rget_obj_type_ff(&ref3, &obj_type);
            assert (ret < 0);
        } H5E_END_TRY;

        assert(H5Rget_filename_ff(&ref1, fname, 1024) == 9);
        assert(strcmp(fname, "ref1_file") == 0);
        assert(H5Rget_filename_ff(&ref2, fname, 1024) == 9);
        assert(strcmp(fname, "ref2_file") == 0);
        assert(H5Rget_filename_ff(&ref3, fname, 1024) == 9);
        assert(strcmp(fname, "ref3_file") == 0);

        assert(H5Rget_name_ff(&ref1, pname, 1024) == 9);
        assert(strcmp(pname, "/G1/G2/G3") == 0);
        assert(H5Rget_name_ff(&ref2, pname, 1024) == 6);
        assert(strcmp(pname, "/G5/D4") == 0);
        assert(H5Rget_name_ff(&ref3, pname, 1024) == 7);
        assert(strcmp(pname, "/G18/M6") == 0);

        ret = H5Rdestroy_ff(&ref1);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref2);
        assert(ret == 0);
        ret = H5Rdestroy_ff(&ref3);
        assert(ret == 0);
    }

    if(0 == my_rank) {
        /* release container version 2. This is async. */
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);

        ret = H5RCclose(rid1);
        assert(0 == ret);
        ret = H5RCclose(rid2);
        assert(0 == ret);
    }

    ret = H5Sclose(sid);
    assert(ret == 0);
    ret = H5Tclose(dtid);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);

    H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);

    ret = H5ESclose(e_stack);
    assert(ret == 0);

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
