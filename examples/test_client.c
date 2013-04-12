/* 
 * test_client.c: Client side of Milestone 3.3 Asynchronous I/O and initial
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
    const char file_name[]="test_file.h5";
    hid_t file_id;
    hid_t gid1, gid2;
    hid_t dataspaceId;
    hid_t did1, did2, did3;
    hid_t fapl_id, dxpl_id;
    const unsigned int nelem=60;
    int *data = NULL, *r_data = NULL, *r2_data = NULL, *data2 = NULL, *data3 = NULL;
    unsigned int i = 0;
    hsize_t dims[1];
    int my_rank, my_size;
    int provided;
    hid_t event_q;
    H5_status_t *status = NULL;
    int num_requests = 0;
    hsize_t extent = 20;
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

    /* Choose the IOD VOL plugin to use with this file. 
       First we create a file access property list. Then we call a new routine to set
       the IOD plugin to use with this fapl */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* allocate and initialize 3 arrays for dataset writes, and 2 arrays 
       for dataset reads. 
       The write arrays are intialized to contain 60 integers (0-59). 
       The read arrays are intialized to contain 60 integers all 0s. */
    data = malloc (sizeof(int)*nelem);
    data2 = malloc (sizeof(int)*nelem);
    data3 = malloc (sizeof(int)*nelem);
    r_data = malloc (sizeof(int)*nelem);
    r2_data = malloc (sizeof(int)*nelem);
    for(i=0;i<nelem;++i) {
        r_data[i] = 0;
        r2_data[i] = 0;
        data[i]=i;
        data2[i]=i;
        data3[i]=i;
    }

    /* create an event Queue for managing asynchronous requests */
    event_q = H5EQcreate(fapl_id);
    assert(event_q);

    /* create the file. This is asynchronous. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, event_q);

    /* create a group G1 on the file. We creat it here synchronously just to
       show that we can intermix the original HDF5 API with the new 
       Async API.
       Internally there is a built in wait on the file_id, which has already been
       completed when we called H5AOwait on the file create request earlier*/
    gid1 = H5Gcreate2(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /* this comment block shows how we could create G1 asynchronously and wait
    *
    * gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
    *                     0, &req2);
    *
    * assert(H5AOwait(&req2, &status2) == 0);
    * assert (status2);
    */
    assert(gid1);

    /* create a dataspace. This is a local Bookeeping operation that 
       does not touch the file */
    dims [0] = 60;
    dataspaceId = H5Screate_simple(1, dims, NULL);

    /* create a Dataset D1 on the file, in group /G1/G2/G3. This is asynchronous. 
       Internally to the IOD-VOL this call traverses the path G1/G2/G3. 
       It realizes that group G1 has been created locally, but has no info about 
       G2 and G3.
       This enforces a wait for the previous H5Gcreate on G1 to complete 
       at the client (which has already completed because the gcreate was done 
       synchronously), then forwards the call asynchronously to the server, 
       with the path G2/G3/D1 */
    did1 = H5Dcreate_ff(file_id,"G1/G2/G3/D1",H5T_NATIVE_INT,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did1);

    /* similar to the previous H5Dcreate. As soon as G1 is created, this can execute 
       asynchronously and concurrently with the H5Dcreate for D1 
       (i.e. no dependency)*/
    did2 = H5Dcreate_ff(file_id,"G1/G2/G3/D2",H5T_NATIVE_INT,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did2);

    /* similar to the previous H5Dcreate. As soon as G1 is created, this can execute 
       asynchronously and concurrently with the H5Dcreate for D1 and D2 
       (i.e. no dependency)*/
    did3 = H5Dcreate_ff(file_id,"G1/G2/G3/D3",H5T_NATIVE_INT,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did3);

    /* Raw data write on D1. This is asynchronous, but it is delayed
       internally at the IOD-VOL plugin client until the create for D1
       is completed.  Internal to the IOD-VOL plugin client we
       generate a checksum for data and ship it with the write bulk
       data function shipper handle to the server. */
    H5Dwrite_ff(did1, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, data, 
                0, event_q);

    /* Raw data write on D2. This is asynchronous, but it is delayed
       internally at the client until the create for D2 is
       completed. Internally we generate a checksum for data2 and ship
       it with the write call to the server.*/
    H5Dwrite_ff(did2, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, data2, 
                0, event_q);

    /* Raw data write on D3. This is asynchronous, but it is delayed
       internally at the client until the create for D3 is
       completed. Internally we generate a checksum for data3 and ship
       it with the write call to the server.*/
    H5Dwrite_ff(did3, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, data3, 
                0, event_q);

    /* NOTE: all raw data reads/writes execute concurrently at the
       server if they get scheduled by the Asynchronous eXecution
       Engine, AXE, (i.e. no dependencies on each other). */


    /* Pop the request from the event queue to wait on it next. This
       will return the request associated with the last H5Dwrite_ff
       call on D3. Note that this also removes the request from the
       event queue, so the user now owns the request and is
       responsible to ensure its completion. */
    if(H5EQpop(event_q, &req1) < 0)
        exit(1);

    /* Wait on the write request. This is not required to be done now.
       But we are demoing how a particular request of interest can be
       obtained and waited on. */
    assert(H5AOwait(req1, &status1) == 0);
    assert (status1);


    /* Raw data read on D1. This is asynchronous, but it is delayed
       internally at the client until the create for D1 is completed,
       which it is since we waited on it in the previous H5Dwrite(D1).

       At the server side, since the IOD library is "skeletal" and no
       data exists, I am creating an array with the same size and
       elements as the data that is written.

       The server returns, along with the data array, a checksum for
       the data that should be stored, but for now generated.  */
    H5Dread_ff(did1, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, r_data, 
               0, event_q);

    /* Pop head request from the event queue to test it next. This is the 
       request that belongs to the previous H5Dread_ff call. */
    if(H5EQpop(event_q, &req1) < 0)
        exit(1);

    /* Test if the Read operation has completed. Since it is asynchronous, It is
       most likely that the operation is pending */
    assert(H5AOtest(req1, &status1) == 0);
    (status1 == H5AO_PENDING) ? fprintf(stderr, "Read is still pending\n") : fprintf(stderr, "Read has completed\n");

    /* Print the received buffer before a completion call on the read is 
       issued. This should print 0s or partial data recieved. */
    fprintf(stderr, "Printing Just after Test (before waiting) ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",r_data[i]);
    fprintf(stderr, "\n");

    /* Here we demo that we can pass hints down to the IOD server. 
       We create a new property, for demo purposes, to tell the server to inject 
       corrupted data into the received array, and hence an incorrect checksum. 
       This also detects that we are passing checksum values in both directions for 
       raw data to ensure data integrity. The read should fail when we wait on it in
       the H5Dclose(D1) later, but for the demo purposes we are not actually going to 
       fail the close, but just print a Fatal error. */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    H5Pset_dxpl_inject_bad_checksum(dxpl_id, 1);
    H5Dread_ff(did1, H5T_NATIVE_INT, dataspaceId, dataspaceId, dxpl_id, r2_data, 
               0, event_q);
    H5Pclose(dxpl_id);

    /* Issue other operations to query certain metadata values or
       update previously created objects */
    {
        unsigned intent;
        char temp_name[50];
        hid_t plist_id;

        H5Fget_intent(file_id, &intent);
        fprintf(stderr, "Intent %d   %d\n", intent, H5F_ACC_RDWR);
        H5Fget_name(gid1, temp_name, 50);
        fprintf(stderr, "File name %s   %s\n", temp_name, file_name);

        plist_id = H5Fget_access_plist(file_id);
        assert(plist_id);
        H5Pclose(plist_id);

        plist_id = H5Fget_create_plist(file_id);
        assert(plist_id);
        H5Pclose(plist_id);

        /* change the dataset dimensions for Dataset D1. This is
           asynchronous, but internally there is a built in
           "wait_some" on operations pending on that dataset */
        assert(H5Dset_extent_ff(did1, &extent, event_q) == 0);
    }

    /* closing did1 acts as a wait_some on all pending requests that are issued
       on did1 (the H5Dwrite and 2 H5Dreads above). This is asynchronous. */
    assert(H5Dclose_ff(did1, event_q) == 0);

    /* closing did2 acts as a wait_some on all pending requests that are issued
       on did2 (the H5Dwrite above). This is asynchronous. */
    assert(H5Dclose_ff(did2, event_q) == 0);

    /* closing did3 acts as a wait_some on all pending requests that are issued
       on did3 (the H5Dwrite above). This is asynchronous. */
    assert(H5Dclose_ff(did3, event_q) == 0);

    H5Sclose(dataspaceId);

    assert(H5Gclose_ff(gid1, event_q) == 0);

    if(my_rank == 4)
        sleep(5);
    /* flush all the contents of file to disk. This is asynchronous. */
    assert(H5Fflush_ff(file_id, H5F_SCOPE_GLOBAL, event_q) == 0);

    /* If the read request did no complete earlier when we tested it, Wait on it now.
       We have to do this since we popped it earlier from the event queue */
    if(H5AO_PENDING == status1) {
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);
    }
    else
        assert(H5AO_SUCCEEDED == status1);


    /* closing the container also acts as a wait all on all pending requests 
       on the container. */
    assert(H5Fclose_ff(file_id, event_q) == 0);
    fprintf(stderr, "%d: done fclose\n", my_rank);
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Expecting 14. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Expecting 0. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    /* Print the data that has been read, after we have issued a wait 
       (in the H5Dclose).
       This should printf the correct array (0-59) */
    fprintf(stderr, "Printing After Waiting ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",r_data[i]);
    fprintf(stderr, "\n");

    /* Print the data that has been read with an injected fault,
       This should print the array similar to the previous one, but with the 
       first value modified to be 10 (the injected error) */
    fprintf(stderr, "Printing Corrupted Data ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",r2_data[i]);
    fprintf(stderr, "\n");

    /* Now we test the Open routines. Since there is no underneath
       container, the underlying VOL server is just going to "fake"
       open calls and assume they exist. However there is no metadata
       information returned since nothing is stored on disk for
       now. */

    /* Open the file. This is asynchronous. Waiting on requests can be
       left for the IOD VOL plugin to handle as necessary, as we do
       here. We also can wait on a request using the new H5AOwait()
       routine */
    file_id = H5Fopen_ff(file_name, H5F_ACC_RDONLY, fapl_id, event_q);
    assert(file_id);
    /* Open a group G1 on the file. 
       Internally there is a built in wait on the file_id.*/
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, 0, event_q);
    assert(gid1);

    /* Open a dataset D1 on the file in a group hierarchy. 
       Internally there is a built in wait on group G1.*/
    did1 = H5Dopen_ff(file_id,"G1/G2/G3/D1", H5P_DEFAULT, 0, event_q);
    assert(did1);

    assert(H5Dclose(did1) == 0);
    assert(H5Gclose(gid1) == 0);
    assert(H5Fclose(file_id) == 0);

    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Expecting 3. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    H5EQclose(event_q);
    H5Pclose(fapl_id);


    /*
    assert(H5AOwait(&req1, &status1) == 0);
    assert (status1);
    assert(H5AOwait(&req2, &status2) == 0);
    assert (status2);
    assert(H5AOwait(&req3, &status3) == 0);
    assert (status3);
    */
    free(data);
    free(r_data);
    free(data2);
    free(r2_data);
    free(data3);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}

