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
    hid_t did; 
    hid_t map;
    hid_t dtid;
    hid_t rid1;
    hid_t aid;
    hid_t fapl_id;
    hid_t event_q;
    uint64_t version;
    int my_rank, my_size;
    int provided;
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

    /* Open the file and ask to acquire the latest readable version */
    file_id = H5Fopen_ff(file_name, H5F_ACC_RDONLY, fapl_id, H5_EVENT_QUEUE_NULL, &rid1);
    assert(file_id);

    /* query the latest readable version number */
    H5RCget_version(rid1, &version);
    /* MSC - assert the fake version value that is returned for now */
    assert(version == 1024);

    /* create objects */
    gid = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid1, event_q);
    assert(gid > 0);
    did = H5Dopen_ff(gid, "D1", H5P_DEFAULT, rid1, event_q);
    assert(did > 0);
    map = H5Mopen_ff(file_id, "MAP1", H5P_DEFAULT, rid1, event_q);
    assert(map > 0);
    dtid = H5Topen_ff(file_id, "DT1", H5P_DEFAULT, rid1, event_q);
    assert(dtid > 0);
    aid = H5Aopen_ff(did, "ATTR_DSET", H5P_DEFAULT, rid1, event_q);
    assert(aid > 0);

    assert(H5Gclose_ff(gid, event_q) == 0);
    assert(H5Dclose_ff(did, event_q) == 0);
    assert(H5Tclose_ff(dtid, event_q) == 0);
    assert(H5Mclose_ff(map, event_q) == 0);
    assert(H5Aclose_ff(aid, event_q) == 0);

    /* release container version 1024. This is async. */
    ret = H5RCrelease(rid1, event_q);
    assert(0 == ret);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    /* closing the container also acts as a wait all on all pending requests 
       on the container. */
    assert(H5Fclose_ff(file_id, event_q) == 0);

    H5EQwait(event_q, &num_requests, &status);
    printf("%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    H5Pclose(fapl_id);
    H5EQclose(event_q);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}
