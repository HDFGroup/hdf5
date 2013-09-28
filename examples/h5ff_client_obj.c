/* 
 * test_client_obj.c: Client side of H5O routines
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

int main(int argc, char **argv) {
    const char file_name[]="map_file.h5";
    hid_t file_id;
    hid_t gid;
    hid_t did, map;
    hid_t sid, dtid;
    hid_t tid1, rid1, rid2;
    hid_t fapl_id, dxpl_id;
    hid_t e_stack;
    hbool_t exists;

    const unsigned int nelem=60;
    hsize_t dims[1];

    uint64_t version;
    uint64_t trans_num;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req;

    H5ES_status_t status;
    size_t num_events = 0;
    herr_t ret;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    /* Call EFF_init to initialize the EFF stack.  
       As a result of this call, the Function Shipper client is started, 
       and HDF5 VOL calls are registered with the function shipper.
       An "IOD init" call is forwarded from the FS client to the FS server 
       which should already be running. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    fprintf(stderr, "Create the FAPL to set the IOD VOL plugin and create the file\n");
    /* Choose the IOD VOL plugin to use with this file. 
       First we create a file access property list. Then we call a new routine to set
       the IOD plugin to use with this fapl */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* create an event Queue for managing asynchronous requests.

       Event Queues will releive the use from managing and completing
       individual requests for every operation. Instead of passing a
       request for every operation, the event queue is passed and
       internally the HDF5 library creates a request and adds it to
       the event queue.

       Multiple Event queue can be created used by the application. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* create the file. This is asynchronous, but the file_id can be used. */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(file_id);

    /* create 1-D dataspace with 60 elements */
    dims [0] = nelem;
    sid = H5Screate_simple(1, dims, NULL);
    dtid = H5Tcopy(H5T_STD_I32LE);

    /* acquire container version 0 - EXACT.  
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);

    /* start transaction 1 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        ret = H5TRstart(tid1, H5P_DEFAULT, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        trans_num = 1;
    }

    /* Tell other procs that transaction 1 is started */
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Process 0 can continue writing to transaction 1, 
       while others wait for the bcast to complete */
    if(0 != my_rank)
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);

    assert(1 == trans_num);

    /* 
       Assume the following object create calls are collective.

       In a real world scenario, the following create calls are
       independent by default; i.e. only 1 process, typically a leader
       process, calls them. The user does the local-to-global,
       global-to-local thing to get the objects accessible to all delegates. 

       This will not fail here because IOD used for now is skeletal,
       so it does not matter if the calls are collective or
       independent.
    */

    /* create objects */
    gid = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
    ret = H5Oset_comment_ff(gid, "Testing Object Comment", tid1, e_stack);
    assert(ret == 0);
    did = H5Dcreate_ff(gid, "D1", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
    ret = H5Tcommit_ff(file_id, "DT1", dtid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
    assert(ret == 0);
    map = H5Mcreate_ff(file_id, "MAP1", H5T_STD_I32LE, H5T_STD_I32LE, 
                       H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid1, e_stack);
    assert(map > 0);

    /* none leader procs have to complete operations before notifying the leader */
    if(0 != my_rank) {
        /* wait on all requests and print completion status */
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
    }

    /* Barrier to make sure all processes are done writing so Process
       0 can finish transaction 1 and acquire a read context on it. */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Leader process finished the transaction after all clients
       finish their updates. Leader also asks the library to acquire
       the committed transaction, that becomes a readable version
       after the commit completes. */
    if(0 == my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);

        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    /* Close transaction object. Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);

    assert(H5Gclose_ff(gid, e_stack) == 0);
    assert(H5Dclose_ff(did, e_stack) == 0);
    assert(H5Tclose_ff(dtid, e_stack) == 0);
    assert(H5Mclose_ff(map, e_stack) == 0);

    /* wait on all requests and print completion status */
    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    /* Process 0 tells other procs that container version 1 is acquired */
    version = 1;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }

    gid = H5Oopen_ff(file_id, "G1", H5P_DEFAULT, rid2);
    assert(gid);
    dtid = H5Oopen_ff(file_id, "DT1", H5P_DEFAULT, rid2);
    assert(dtid);
    did = H5Oopen_ff(gid,"D1", H5P_DEFAULT, rid2);
    assert(did);
    map = H5Oopen_ff(file_id,"MAP1", H5P_DEFAULT, rid2);
    assert(did);

    {
        ssize_t ret_size = 0;
        char *comment = NULL;
        H5O_ff_info_t oinfo;

        ret = H5Oget_comment_ff(gid, NULL, 0, &ret_size, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);

        fprintf(stderr, "size of comment is %d\n", ret_size);

        comment = malloc((size_t)ret_size + 1);

        ret = H5Oget_comment_ff(gid, comment, (size_t)ret_size + 1, 
                                &ret_size, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);

        fprintf(stderr, "size of comment is %d Comment is %s\n", ret_size, comment);
        free(comment);

        ret = H5Oget_info_ff(gid, &oinfo, rid2, H5_EVENT_STACK_NULL);
        assert(ret == 0);

        switch(oinfo.type) {
        case H5O_TYPE_GROUP:
            fprintf(stderr, 
                    "OBJECT GET INFO return a GROUP TYPE with IOD ID: %llu, num attrs = %llu, reference count = %d\n", 
                    oinfo.addr, oinfo.num_attrs, oinfo.rc);
            break;
        default:
            fprintf(stderr, "Unexpected result from oget_info\n");
            exit(1);
        }
    }

    /* check if an object exists. This is asynchronous, so checking
       the value should be done after the wait */
    ret = H5Oexists_by_name_ff(file_id, "G1_COPY", &exists, 
                               H5P_DEFAULT, rid2, e_stack);
    assert(ret == 0);

    if(my_rank == 0) {
        /* release container version 1. This is async. */
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);
    }

    assert(H5Oclose_ff(gid, e_stack) == 0);
    assert(H5Oclose_ff(did, e_stack) == 0);
    assert(H5Oclose_ff(dtid, e_stack) == 0);
    assert(H5Oclose_ff(map, e_stack) == 0);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

    /* closing the container also acts as a wait all on all pending requests 
       on the container. */
    assert(H5Fclose_ff(file_id, e_stack) == 0);

    /* wait on all requests and print completion status */
    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    H5Sclose(sid);
    H5Pclose(fapl_id);
    H5ESclose(e_stack);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}
