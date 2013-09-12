/* 
 * test_client_map.c: Client side of Map Object
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
    hid_t gid1, gid2, gid3, gid4, gid5;
    hid_t did1, did2, did3;
    hid_t sid, dtid;
    hid_t tid1, tid2, rid1, rid2, rid3;
    hid_t fapl_id, dxpl_id, trspl_id;
    hid_t event_q;

    const unsigned int nelem=60;
    hsize_t dims[1];

    uint64_t version;
    uint64_t trans_num;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req;

    H5_status_t *status = NULL;
    int num_requests = 0, i;
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
    event_q = H5EQcreate(fapl_id);
    assert(event_q);

    /* create the file. This is asynchronous, but the file_id can be used. */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(file_id);

    /* create 1-D dataspace with 60 elements */
    dims [0] = nelem;
    sid = H5Screate_simple(1, dims, NULL);
    dtid = H5Tcopy(H5T_STD_I32LE);

    /* acquire container version 0 - EXACT */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_QUEUE_NULL);
    assert(0 == version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);

    /* start transaction 1 with default num_peers (= 0). */
    if(0 == my_rank) {
        ret = H5TRstart(tid1, H5P_DEFAULT, H5_EVENT_QUEUE_NULL);
        assert(0 == ret);
    }

    /* Tell other procs that transaction 1 is started */
    trans_num = 1;
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Process 0 can continue writing to transaction 1, 
       while others wait for the bcast to complete */
    if(0 != my_rank)
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);

    /* create objects */
    gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    gid3 = H5Gcreate_ff(gid2, "G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    did1 = H5Dcreate_ff(gid3, "D1", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    did2 = H5Dcreate_ff(gid3, "D2", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    did3 = H5Dcreate_ff(gid3, "D3", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);

    gid4 = H5Gcreate_ff(file_id, "G4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    gid5 = H5Gcreate_ff(gid4, "G5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);

    /* create a hard link to D1 (created previosuly) so that it can be
       accessed from G5/newD1. */
    ret = H5Lcreate_hard_ff(did1, ".", gid5, "newD1", H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(ret == 0);

    /* none leader procs have to complete operations before notifying the leader */
    if(0 != my_rank) {
        H5EQwait(event_q, &num_requests, &status);
        printf("%d requests in event queue. Completions: ", num_requests);
        for(i=0 ; i<num_requests; i++)
            fprintf(stderr, "%d ",status[i]);
        fprintf(stderr, "\n");
        free(status);
    }

    /* Barrier to make sure all processes are done writing so Process
       0 can finish transaction 1 and acquire a read context on it. */
    MPI_Barrier(MPI_COMM_WORLD);

    if(0 == my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);

        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_QUEUE_NULL);
        assert(0 == ret);
    }

    /* another barrier so other processes know that container version is acquried */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Close transaction object. Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, event_q);
    assert(0 == ret);

    assert(H5Dclose_ff(did2, event_q) == 0);

    H5EQwait(event_q, &num_requests, &status);
    printf("%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    /* Process 0 tells other procs that container version 1 is acquired */
    version = 1;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }

    /* Try and open the dataset. This is asynchronous. */
    did2 = H5Dopen_ff(file_id,"G4/G5/newD1", H5P_DEFAULT, rid2, event_q);
    assert(did2);

    /* create & start transaction 2 with num_peers = n */
    tid2 = H5TRcreate(file_id, rid2, (uint64_t)2);
    assert(tid2);
    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, event_q);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    ret = H5Lcreate_soft_ff("/G1/G2/G3/D4", gid4, "G5/newD2", 
                            H5P_DEFAULT, H5P_DEFAULT, tid2, event_q);
    assert(ret == 0);

    ret = H5Lmove_ff(gid3, "D3", file_id, "/G4/G5/D3moved", 
                     H5P_DEFAULT, H5P_DEFAULT, tid2, event_q);
    assert(ret == 0);

    ret = H5Ldelete_ff(file_id, "/G1/G2/G3/D2", H5P_DEFAULT, tid2, event_q);
    assert(ret == 0);

    /* finish transaction 2 */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, event_q);
    assert(0 == ret);

    /* release container version 1. This is async. */
    ret = H5RCrelease(rid2, event_q);
    assert(0 == ret);

    assert(H5Gclose_ff(gid1, event_q) == 0);
    assert(H5Gclose_ff(gid2, event_q) == 0);
    assert(H5Gclose_ff(gid3, event_q) == 0);
    assert(H5Gclose_ff(gid5, event_q) == 0);

    assert(H5Dclose_ff(did1, event_q) == 0);
    assert(H5Dclose_ff(did2, event_q) == 0);
    assert(H5Dclose_ff(did3, event_q) == 0);

    H5EQwait(event_q, &num_requests, &status);
    printf("%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);
    ret = H5TRclose(tid2);
    assert(0 == ret);

    /* acquire container version 2 - EXACT */
    version = 2;
    rid3 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_QUEUE_NULL);
    assert(2 == version);

    {
        H5L_ff_info_t linfo;
        char *link_buf;

        ret = H5Lget_info_ff(gid4, "G5/newD2", &linfo, H5P_DEFAULT, rid3, H5_EVENT_QUEUE_NULL);
        assert(ret == 0);

        switch(linfo.type) {
        case H5L_TYPE_SOFT:
            fprintf(stderr, 
                    "LINK GET INFO return a SOFT LINK with value size: %zu\n", 
                    linfo.u.val_size);
            break;
        default:
            fprintf(stderr, "Unexpected result from lget_info\n");
            exit(1);
        }

        link_buf = malloc(linfo.u.val_size);

        ret = H5Lget_val_ff(gid4, "G5/newD2", link_buf, linfo.u.val_size, 
                            H5P_DEFAULT, rid3, H5_EVENT_QUEUE_NULL);
        assert(ret == 0);

        fprintf(stderr, "Link value = %s\n", link_buf);

        free(link_buf);
    }

    /* release container version 1. This is async. */
    ret = H5RCrelease(rid3, event_q);
    assert(0 == ret);

    assert(H5Gclose_ff(gid4, event_q) == 0);

    /* closing the container also acts as a wait all on all pending requests 
       on the container. */
    assert(H5Fclose_ff(file_id, event_q) == 0);

    /* wait on all requests and print completion status */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    ret = H5RCclose(rid3);
    assert(0 == ret);

    H5Sclose(sid);
    H5Tclose(dtid);
    H5Pclose(fapl_id);
    H5EQclose(event_q);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}
