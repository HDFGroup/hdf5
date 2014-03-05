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
    const char file_name[]="eff_file_attr.h5";

    hid_t file_id;
    hid_t gid1;
    hid_t sid, dtid, stype_id;
    hid_t sid2;
    hid_t did1, map;
    hid_t aid1, aid2, aid3, aid4, aid5;
    hid_t tid1, tid2, rid1, rid2, rid3;
    hid_t fapl_id, trspl_id;
    hid_t e_stack;
    hbool_t exists1;
    hbool_t exists2;

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

    /* set the metada data integrity checks to happend at transfer through mercury */
    cs_scope |= H5_CHECKSUM_TRANSFER;
    ret = H5Pset_metadata_integrity_scope(fapl_id, cs_scope);
    assert(ret == 0);

    /* allocate and initialize arrays for dataset I/O */
    wdata1 = malloc (sizeof(int32_t)*nelem);
    wdata2 = malloc (sizeof(int32_t)*nelem);
    rdata1 = malloc (sizeof(int32_t)*nelem);
    rdata2 = malloc (sizeof(int32_t)*nelem);
    for(i=0;i<nelem;++i) {
        rdata1[i] = 0;
        rdata2[i] = 0;
        wdata1[i]=i;
        wdata2[i]=i*2;
    }

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

    stype_id = H5Tcopy(H5T_C_S1 ); assert( stype_id );
    ret = H5Tset_strpad(stype_id, H5T_STR_NULLTERM ); assert( ret == 0 );
    ret = H5Tset_size(stype_id, 128); assert( ret == 0 );

    /* acquire container version 0 - EXACT.  
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    /* start transaction 1 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        /* create transaction object */
        tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
        assert(tid1);
        ret = H5TRstart(tid1, H5P_DEFAULT, e_stack);
        assert(0 == ret);

        sid2 = H5Screate(H5S_SCALAR); assert( sid2 );

        /* create group /G1 */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, 
                            H5P_DEFAULT, tid1, e_stack);
        assert(gid1 > 0);

        /* create dataset /G1/D1 */
        did1 = H5Dcreate_ff(gid1, "D1", dtid, sid2, H5P_DEFAULT, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did1 > 0);

        /* Commit the datatype dtid to the file. */
        ret = H5Tcommit_ff(file_id, "DT1", dtid, H5P_DEFAULT, H5P_DEFAULT, 
                           H5P_DEFAULT, tid1, e_stack);
        assert(ret == 0);

        /* create a Map object on the root group */
        map = H5Mcreate_ff(file_id, "MAP1", H5T_STD_I32LE, H5T_STD_I32LE, 
                           H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid1, e_stack);
        assert(map > 0);

        /* create an attribute on root group */
        aid1 = H5Acreate_ff(file_id, "ROOT_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid1);

        /* create an attribute on group G1. */
        aid2 = H5Acreate_ff(gid1, "GROUP_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid2);

        /* write data to attributes */
        ret = H5Awrite_ff(aid1, dtid, wdata1, tid1, e_stack);
        assert(ret == 0);
        ret = H5Awrite_ff(aid2, dtid, wdata2, tid1, e_stack);
        assert(ret == 0);

        /* create an attribute on dataset */
        aid3 = H5Acreate_ff(did1, "DSET_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid3);

        /* create an attribute on datatype */
        aid4 = H5Acreate_ff(dtid, "DTYPE_ATTR", dtid, sid, 
                            H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(aid4);


        aid5 = H5Acreate_ff(map, "MAP_ATTR", stype_id, sid2, H5P_DEFAULT, 
                            H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL );
        assert(aid5);
        ret = H5Awrite_ff(aid5, stype_id, "/AA by rank 0", tid1, H5_EVENT_STACK_NULL ); 
        assert( ret == 0 );

        H5Sclose(sid2);

        ret = H5Aclose_ff(aid1, e_stack);
        assert(ret == 0);

        /* make this synchronous so we know the container version has been acquired */
        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        /* Local op */
        ret = H5TRclose(tid1);
        assert(0 == ret);
    }

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    if(0 == my_rank) {
        /* create transaction object */
        tid2 = H5TRcreate(file_id, rid2, (uint64_t)2);
        assert(tid1);
        ret = H5TRstart(tid2, H5P_DEFAULT, e_stack);
        assert(0 == ret);

        /* Delete an attribute */
        ret = H5Adelete_ff(file_id, "ROOT_ATTR", tid2, e_stack);
        assert(ret == 0);
        /* rename an attribute */
        ret = H5Arename_ff(gid1, "GROUP_ATTR", "RENAMED_GROUP_ATTR", tid2, e_stack);
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
        ret = H5Mclose_ff(map, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid1, e_stack);
        assert(ret == 0);

        /* make this synchronous so we know the container version has been acquired */
        ret = H5TRfinish(tid2, H5P_DEFAULT, &rid3, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        /* Local op */
        ret = H5TRclose(tid2);
        assert(0 == ret);

        /* release container version 1. This is async. */
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);

        version = 2;
    }

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    /* Tell other procs that container version 2 is acquired */
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        assert(2 == version);
        rid3 = H5RCcreate(file_id, version);
        assert(rid3 > 0);
    }

    /* do some reads/gets from container version 1 */

    ret = H5Aexists_ff(file_id, "ROOT_ATTR", &exists1, rid3, e_stack);
    assert(ret == 0);
    ret = H5Aexists_by_name_ff(file_id, "G1", "RENAMED_GROUP_ATTR", H5P_DEFAULT, 
                               &exists2, rid3, e_stack);
    assert(ret == 0);

    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid3, e_stack);

    aid2 = H5Aopen_ff(gid1, "RENAMED_GROUP_ATTR", H5P_DEFAULT, rid3, e_stack);
    assert(aid2);
    ret = H5Aread_ff(aid2, dtid, rdata2, rid3, e_stack);
    assert(ret == 0);

    aid5 = H5Aopen_by_name_ff(file_id, "MAP1", "MAP_ATTR", H5P_DEFAULT, 
                              H5P_DEFAULT, rid3, e_stack);
    assert(aid5);
    ret = H5Aread_ff(aid5, stype_id, str_data, rid3, e_stack);
    assert(ret == 0);

    ret = H5Aclose_ff(aid2, e_stack);
    assert(ret == 0);
    ret = H5Aclose_ff(aid5, e_stack);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, e_stack);
    assert(ret == 0);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    /* barrier so root knows all are done reading */
    MPI_Barrier(MPI_COMM_WORLD);

    if(my_rank == 0) {
        /* release container version 2. This is async. */
        ret = H5RCrelease(rid3, e_stack);
        assert(0 == ret);
    }

    ret = H5Sclose(sid);
    assert(ret == 0);
    ret = H5Tclose(dtid);
    assert(ret == 0);
    ret = H5Tclose(stype_id);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);

    H5Fclose_ff(file_id, H5_EVENT_STACK_NULL);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    if(0 == my_rank) {
        ret = H5RCclose(rid2);
        assert(0 == ret);
    }
    ret = H5RCclose(rid3);
    assert(0 == ret);

    assert(!exists1);
    assert(exists2);

    fprintf(stderr, "RENAMED_GROUP_ATTR value: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata2[i]);
    fprintf(stderr, "\n");

    fprintf(stderr, "MAP_ATTR value: %s\n", str_data);

    ret = H5ESclose(e_stack);
    assert(ret == 0);

    free(wdata1);
    free(wdata2);
    free(rdata1);
    free(rdata2);

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
