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
    const char file_name[]="eff_file.h5";
    hid_t file_id;
    hid_t gid1, gid2, gid3;
    hid_t dataspaceId;
    hid_t did1, did2, did3;
    hid_t aid1, aid2, aid3;
    hid_t fapl_id, dxpl_id;
    const unsigned int nelem=60;
    int *data = NULL, *r_data = NULL, *r2_data = NULL, *data2 = NULL;
    int16_t *data3 = NULL;
    int16_t *r3_data = NULL;
    int *a_data = NULL, *ra_data = NULL;
    unsigned int i = 0;
    hsize_t dims[1];
    int my_rank, my_size;
    int provided;
    hid_t event_q;
    hid_t int_id;
    H5_status_t *status = NULL;
    int num_requests = 0;
    hsize_t extent = 20;
    hbool_t exists;
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

    /* allocate and initialize arrays for dataset & attribute writes and reads. 
       The write arrays are intialized to contain 60 integers (0-59). 
       The read arrays are intialized to contain 60 integers all 0s. */
    data = malloc (sizeof(int)*nelem);
    data2 = malloc (sizeof(int)*nelem);
    data3 = malloc (sizeof(int16_t)*nelem);
    a_data = malloc (sizeof(int)*nelem);
    ra_data = malloc (sizeof(int)*nelem);
    r_data = malloc (sizeof(int)*nelem);
    r2_data = malloc (sizeof(int)*nelem);
    r3_data = malloc (sizeof(int16_t)*nelem);
    for(i=0;i<nelem;++i) {
        r_data[i] = 0;
        ra_data[i] = 0;
        r2_data[i] = 0;
        r3_data[i] = 0;
        data[i]=i;
        data2[i]=i;
        data3[i]=i;
        a_data[i]=i;
    }

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
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, event_q);

    {
        char temp_name[50];
        H5Fget_name(file_id, temp_name, 50);
        fprintf(stderr, "File name %s   %s\n", temp_name, file_name);
    }

    /* create 3 groups in the file. All calls are completely
       asychronous. Even though they depend one each other here and
       they also depend on the file, they will be shipped immdeiately
       and asynchronously to the server and return to the user. At the
       server, dependencies are noted in the AXE and functions execute
       in the order they are supposed to execute. */
    gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    H5Oset_comment_ff(gid1, "Testing Object Comment", 0, event_q);
    gid2 = H5Gcreate_ff(file_id, "G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    gid3 = H5Gcreate_ff(file_id, "G1/G2/G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);

    assert(gid1);
    assert(gid2);
    assert(gid3);

    /* create a 32 bit integer LE datatype. This is a local operation
       that does not touch the file */
    int_id = H5Tcopy(H5T_STD_I32LE);

    /* Commit the datatype to the file. This is asynchronous & immediate. 
     * Other Local H5T type operations can be issued before completing this call
     * because they do not depend on the committed state of the datatype.
     */
    H5Tcommit_ff(file_id, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                 0, event_q);

    /* create a dataspace. This is a local Bookeeping operation that 
       does not touch the file */
    dims [0] = 60;
    dataspaceId = H5Screate_simple(1, dims, NULL);

    /* create an attribute on group G1. This is asynchronous and
       returns immediately, however at the server it won't start until
       the group G1 creation op is completed */
    aid1 = H5Acreate_ff(gid1, "ATTR1", H5T_NATIVE_INT, dataspaceId, 
                        H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(aid1);

    /* close the attribute right after the create. This is
       asynchronous. The attribute id can not be used anymore, however
       the creation operation will occur first at the server before
       the close */
    H5Aclose_ff(aid1, event_q);

    /* Check if the attribute on group G1 exists. This is
       asynchronous, so the exists status is correct to examine after
       the completion of the operation */
    assert(H5Aexists_by_name_ff(file_id,"G1","ATTR1", H5P_DEFAULT, &exists, 0, event_q) == 0);

    /* Pop the request for the last operation from the event
       queue. The Event queue does not manage it anymore and it is the
       application's responsibilty to wait/test and ensure
       completion */
    if(H5EQpop(event_q, &req1) < 0)
        exit(1);
    /* wait synchronously on the operation */
    assert(H5AOwait(req1, &status1) == 0);
    assert (status1);

    if(exists)
        printf("Attribute ATTR1 exists!\n");
    else
        printf("Attribute ATTR1 does NOT exist. This must be the test without a native backend\n");

    /* Delete the attribute just created, this is asynchronous */
    assert(H5Adelete_by_name_ff(file_id, "G1", "ATTR1", H5P_DEFAULT, 0, event_q) == 0);

    /* check if it exists now. This is asynchronous. Can't check the
       exists status until the operation completes. */
    assert(0 == H5Aexists_ff(gid1, "ATTR1", &exists, 0, event_q));

    /* create a Dataset D1 on the file, in group /G1/G2/G3. This is
       asynchronous.  Internally to the IOD-VOL this call traverses
       the path G1/G2/G3.  it forwards the call asynchronously to the
       server, with a dependency on Group G3 creation operation */
    did1 = H5Dcreate_ff(file_id,"G1/G2/G3/D1",int_id,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did1);

    /* create an attribute on dataset D1. This is asynchronous, but
       executes at the server after D1 is created */
    aid2 = H5Acreate_by_name_ff(file_id, "G1/G2/G3/D1", "ATTR2_tmp", H5T_NATIVE_INT, 
                                dataspaceId, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(aid2);

    /* rename the attribute. This is asynchronous with a dependency on
       the create at the server*/
    H5Arename_ff(did1, "ATTR2_tmp", "ATTR2", 0, event_q);

    /* write data to the attribute. This is asynchronous but will wait
       for the creation and rename calls at the server to complete */
    H5Awrite_ff(aid2, int_id, a_data, 0, event_q);

    /* similar to the previous H5Dcreate. */
    did2 = H5Dcreate_ff(file_id,"G1/G2/G3/D2",int_id,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did2);

    /* similar to the previous H5Dcreate. */
    did3 = H5Dcreate_ff(file_id,"G1/G2/G3/D3",int_id,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did3);

    /* Raw data write on D1. This is asynchronous, but it is delayed
       internally at the server until the create for D1
       is completed.  Internal to the IOD-VOL plugin client we
       generate a checksum for data and ship it with the write bulk
       data function shipper handle to the server. */
    H5Dwrite_ff(did1, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, data, 
                0, event_q);

    /* Raw data write on D2. same as previous. */
    H5Dwrite_ff(did2, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, data2, 
                0, event_q);

    /* Raw data write on D3. Same as previous; however we specify that
       the data in the buffer is in BE byte order and of smaller
       extent than the datatype of the dataset. Type conversion will
       happen at the server when we detect that the dataset type is of
       LE order and the datatype here is in BE order and the
       difference in size. The buffer is resized at the server also to
       encompass the increase. */
    H5Dwrite_ff(did3, H5T_STD_I16LE, dataspaceId, dataspaceId, H5P_DEFAULT, data3, 
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

    /* Raw data read on D1. This is asynchronous.

       At the server side, since the IOD library is "skeletal" and no
       data exists, I am creating an array with the same size and
       elements as the data that is written.

       The server returns, along with the data array, a checksum for
       the data that should be stored, but for now generated.  */
    H5Dread_ff(did1, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, r_data, 
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
    H5Dread_ff(did1, int_id, dataspaceId, dataspaceId, dxpl_id, r2_data, 
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

        /* change the dataset dimensions for Dataset D1. */
        assert(H5Dset_extent_ff(did1, &extent, event_q) == 0);
    }

    /* closing did1 acts as a barrier task at the server for all
       operations dependeing on the dataset. This is asynchronous. */
    assert(H5Dclose_ff(did1, event_q) == 0);

    assert(H5Dclose_ff(did2, event_q) == 0);
    assert(H5Dclose_ff(did3, event_q) == 0);
    assert(H5Aclose_ff(aid2, event_q) == 0);
    assert(H5Tclose_ff(int_id, event_q) == 0);
    assert(H5Gclose_ff(gid1, event_q) == 0);
    assert(H5Gclose_ff(gid2, event_q) == 0);
    assert(H5Gclose_ff(gid3, event_q) == 0);

    /* Test Links */

    /* create two new groups /G4 and /G4/G5 */
    gid1 = H5Gcreate_ff(file_id, "G4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    gid2 = H5Gcreate_ff(file_id, "G4/G5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);

    /* create a hard link to D1 (created previosuly) so that it can be
       accessed from G5/newD1. This is asynchronous; however all
       operation that depend on G5 now will have this operation as a
       parent at the AXE on the server. */
    H5Lcreate_hard_ff(file_id, "G1/G2/G3/D1", gid1, "G5/newD1", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);

    /* Try and open the dataset. This is asynchronous. */
    did1 = H5Dopen_ff(file_id,"G4/G5/newD1", H5P_DEFAULT, 0, event_q);

    H5Lcreate_soft_ff("/G1/G2/G3/D4", gid1, "G5/newD2", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    H5Lmove_ff(file_id, "/G1/G2/G3/D3", file_id, "/G4/G5/D3moved", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    H5Ldelete_ff(file_id, "/G1/G2/G3/D2", H5P_DEFAULT, 0, event_q);

    assert(H5Dclose_ff(did1, event_q) == 0);
    assert(H5Gclose_ff(gid1, event_q) == 0);
    assert(H5Gclose_ff(gid2, event_q) == 0);

    /* flush all the contents of file to disk. This is a barrier task
       at the server. This is asynchronous. */
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

    /* wait on all requests and print completion status */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    /* wait again.. event queue should be empty now */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Expecting 0. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    /* Now we can check operations that were issued previously */
    if(exists)
        printf("Attribute ATTR1 exists after being removed! Something is wrong!\n");
    else
        printf("Attribute ATTR1 does NOT exist. Good, it was removed!\n");

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

    /* test object copy. This is asynchronous */
    assert(H5Ocopy_ff(file_id, "/G1/G2/G3/D1", file_id, "D1_copy", 
                      H5P_DEFAULT, H5P_DEFAULT, 0, event_q) == 0);

    /* check if an object exists. This is asynchronous, so checking
       the value should be done after the wait */
    H5Oexists_by_name_ff(file_id, "G1/G2/G3", &exists, H5P_DEFAULT, 0, event_q);

    /* Open objects using the general H5O routines. This has an
       asynchronous interface, but right now we implement it
       synchronously, because we don't the object type to be able to
       generate the ID. */
    gid1 = H5Oopen_ff(file_id, "G1", H5P_DEFAULT, 0, event_q);
    assert(gid1);
    int_id = H5Oopen_ff(file_id, "int", H5P_DEFAULT, 0, event_q);
    assert(int_id);
    did1 = H5Oopen_ff(file_id,"G1/G2/G3/D1", H5P_DEFAULT, 0, event_q);
    assert(did1);

    /* open attribute by name. This is asynchronous. */
    aid2 = H5Aopen_by_name_ff(file_id, "G1/G2/G3/D1", "ATTR2", 
                              H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(aid2);

    assert(H5Aclose_ff(aid2, event_q) == 0);
    assert(H5Oclose_ff(did1, event_q) == 0);
    assert(H5Oclose_ff(int_id, event_q) == 0);
    assert(H5Oclose_ff(gid1, event_q) == 0);

    /* Open a group G1 on the file. This is asynchronous */
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, 0, event_q);
    assert(gid1);

    /* get operations on the group */
    {
        ssize_t ret_size;
        char *comment = NULL;

        H5Oget_comment_ff(gid1, NULL, 0, &ret_size, 0, event_q);
        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);
        fprintf(stderr, "size of comment is %d\n", ret_size);

        comment = malloc((size_t)ret_size);

        H5Oget_comment_ff(gid1, comment, (size_t)ret_size + 1, &ret_size, 0, event_q);
        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);
        fprintf(stderr, "size of comment is %d Comment is %s\n", ret_size, comment);
        free(comment);
    }

    /* Open a named datatype in the file. 
     * This is implemented synchronously for now. */
    int_id = H5Topen_ff(file_id, "int", H5P_DEFAULT, 0, event_q);
    assert(int_id);

    /* Open a dataset D1 on the file in a group hierarchy. 
       This is asynchronous */
    did1 = H5Dopen_ff(file_id,"G1/G2/G3/D1", H5P_DEFAULT, 0, event_q);
    assert(did1);

    /* Raw data read on D1. This is asynchronous.  Note that the type
       is different than the dataset type. The dataset was created in
       int LE order with data written to it earlier in the same type
       (0 - 60).

       Reading this now will read those values, and convert them to BE
       16 bit integers at the server and send them to the
       client. Printing this data will result in 0 - 60 in 16 bit BE byte
       order. */
    H5Dread_ff(did1, H5T_STD_I32BE, dataspaceId, dataspaceId, H5P_DEFAULT, r_data, 
               0, event_q);
    if(H5EQpop(event_q, &req1) < 0)
        exit(1);
    assert(H5AOwait(req1, &status1) == 0);
    assert (status1);
    fprintf(stderr, "Printing Dataset value read in 16 bit BE order. This should be interesting: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",r_data[i]);
    fprintf(stderr, "\n");
    assert(H5Dclose(did1) == 0);

    /* open attribute on dataset D1. This is asynchronous */
    aid2 = H5Aopen_by_name_ff(file_id, "G1/G2/G3/D1", "ATTR2", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(aid2);

    /* read data from attribute. this is asynchronous */
    H5Aread_ff(aid2, int_id, ra_data, 0, event_q);
    assert(H5Aclose(aid2) == 0);

    assert(H5Tclose(int_id) == 0);
    assert(H5Gclose(gid1) == 0);
    assert(H5Fclose(file_id) == 0);

    /* wait on all requests in event queue */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    fprintf(stderr, "Printing Attribute data (after EQ wait): ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",ra_data[i]);
    fprintf(stderr, "\n");

    if(exists)
        printf("Group G3 exists!\n");
    else
        printf("Group G3 does NOT exist. This must be the test without a native backend\n");

    H5EQclose(event_q);
    H5Pclose(fapl_id);
    H5Sclose(dataspaceId);

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
    free(ra_data);
    free(a_data);
    free(data2);
    free(r2_data);
    free(r3_data);
    free(data3);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}

