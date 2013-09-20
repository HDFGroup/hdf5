/* 
 * test_client_path.c: This example demonstrates what can and can't be
 * done with access through Paths in the IOD plugin.
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
    hid_t gid1, gid2, gid3;
    hid_t map;
    hid_t tid1, tid2, rid1, rid2;
    hid_t fapl_id, dxpl_id, trspl_id;
    hid_t event_q;

    uint64_t version;
    uint64_t trans_num;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req;

    H5_status_t *status = NULL;
    int num_requests = 0, i;
    herr_t ret;

    hsize_t count = -1;
    int key, value;
    hbool_t exists = -1;
    H5_request_t req1;
    H5_status_t status1;

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

    /* acquire container version 0 - EXACT */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_QUEUE_NULL);
    assert(0 == version);

    /******************************** Transaction 1 ****************************************/
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

    /* create a group G1 on the root group */
    gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);

    /* to be able to create G2 on G1, we have to use G1's ID as the starting location */
    gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);

    /* If we happen to do something like this :

       gid2 = H5Gcreate_ff(file_id, "G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);

       We will get a failure when the operation completes, because the
       path G1/G2 indicates that we have to read G1 from IOD in read
       context 0. Since G1 is created in transaction 1, it is still
       not yet visible so we can not read from it. We can only write
       to it, which makes the earlier create directly on G1
       possible. This operation will succeed in a subsequent
       transaction that has a read context 1. 
    */


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

    /******************************** END Transaction 1 ****************************************/

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, event_q);
    assert(0 == ret);

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


    /******************************** Transaction 2 ****************************************/

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

    /* create G3 under /G1/G2. This can be done by either providing
       the file_id as the starting location, and the entire path to
       where G3 needs to be created (G1/G2/G3), since the intermediate
       groups G1 and G2 are readable, or gid2 as the starting
       location. The latter would be more performant, since we already
       have G2 open, so it will avoid extra access to get to G2 as
       would be in the former case. */
    gid3 = H5Gcreate_ff(gid2, "G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid2, event_q);
    /* This will work too:
       gid3 = H5Gcreate_ff(file_id, "G1/G2/G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid2, event_q);
    */

    /* create a Map object under G3. Since G3 is created in the same
       transaction as the map create will occur, the direct location
       for G3 is needed for the create to succeed. */
    map = H5Mcreate_ff(gid3, "MAP", H5T_STD_I32LE, H5T_STD_I32LE, 
                       H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid2, event_q);

    /* finish transaction 2 */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, event_q);
    assert(0 == ret);

    ret = H5TRclose(tid2);
    assert(0 == ret);

    /******************************** END Transaction 2 ****************************************/

    /* release container version 1. This is async. */
    ret = H5RCrelease(rid2, event_q);
    assert(0 == ret);

    assert(H5Gclose_ff(gid1, event_q) == 0);
    assert(H5Gclose_ff(gid2, event_q) == 0);
    assert(H5Gclose_ff(gid3, event_q) == 0);
    assert(H5Mclose_ff(map, event_q) == 0);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

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

    H5EQclose(event_q);
    H5Pclose(fapl_id);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}
