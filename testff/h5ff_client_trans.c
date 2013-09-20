/* 
 * test_client.c: Client side of Milestone 4.2 Asynchronous I/O and initial
 * IOD VOL plugin demonstration.  This is, in effect, the application program that 
 * would run on one or more compute nodes and make calls to the HDF5 API.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

int main(int argc, char **argv) {
    const char file_name[]="eff_file.h5";
    hid_t file_id;
    hid_t fapl_id, trspl_id;
    hid_t tid1, tid2, tid3;
    hid_t rid1, rid2, rid3;
    uint64_t version;
    int my_rank, my_size;
    int provided;
    hid_t event_q;
    H5_status_t *status = NULL;
    int i, num_requests = 0;
    herr_t ret;
    H5_request_t req1;
    H5_status_t status1;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Initialize EFF stack\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

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

    event_q = H5EQcreate(fapl_id);
    assert(event_q);

    /* create the file */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(file_id);

    version = 0;
    /* acquire container version 0 - EXACT */
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_QUEUE_NULL);

    /* create 2 transactions objects (does not start transactions). Local call. */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);
    tid2 = H5TRcreate(file_id, rid1, (uint64_t)555);
    assert(tid2);

    /* start transaction 1 with default num_peers (= 0). 
       This is asynchronous. */
    if(my_rank == 0) {
        hid_t gid1, gid2;

        ret = H5TRstart(tid1, H5P_DEFAULT, event_q);
        assert(0 == ret);

        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
        assert(gid1);
        gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
        assert(gid2);

        assert(H5Gclose_ff(gid1, event_q) == 0);
        assert(H5Gclose_ff(gid2, event_q) == 0);
    }

    /* skip transactions 2 till 554. This is asynchronous. */
    ret = H5TRskip(file_id, (uint64_t)2, (uint64_t)553, event_q);
    assert(0 == ret);

    /* start transaction 555 with num_peers = n */
    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, event_q);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* set dependency from transaction 555 on 2. 
       This is asynchronous but has a dependency on H5TRstart() of tid2. */
    ret = H5TRset_dependency(tid2, (uint64_t)1, event_q);
    assert(0 == ret);

    /* finish transaction 1. 
       This is asynchronous, but has a dependency on H5TRstart() of tid1. */
    if(my_rank == 0) {
        ret = H5TRfinish(tid1, H5P_DEFAULT, NULL, event_q);
        assert(0 == ret);
    }

    /* Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* finish transaction 555 and acquire a read context for it */
    ret = H5TRfinish(tid2, H5P_DEFAULT, &rid2, event_q);
    assert(0 == ret);
    ret = H5TRclose(tid2);
    assert(0 == ret);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, event_q);
    assert(0 == ret);

    ret = H5RCclose(rid1);
    assert(0 == ret);

    /* wait on all requests in event queue */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    if(my_rank == 0) {
        /* Start transaction 556 from read context 555 */
        tid3 = H5TRcreate(file_id, rid2, (uint64_t)556);
        assert(tid1);

        ret = H5TRstart(tid3, H5P_DEFAULT, event_q);
        assert(0 == ret);

        /* abort transaction 556 */
        ret = H5TRabort(tid3, event_q);
        assert(tid1);

        ret = H5TRclose(tid3);
        assert(0 == ret);
    }

    /* persist version 555 */
    ret = H5RCpersist(rid2, event_q);
    assert(0 == ret);

    /* snapshot version 555 */
    ret = H5RCsnapshot(rid2, "container_555", event_q);
    assert(0 == ret);

    ret = H5RCclose(rid2);
    assert(0 == ret);

    ret = H5Fclose(file_id);
    assert(0 == ret);

    /* wait on all requests in event queue */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    H5EQclose(event_q);
    H5Pclose(fapl_id);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Finalize EFF stack\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}
