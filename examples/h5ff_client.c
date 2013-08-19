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
    hid_t gid1, gid2, gid3;
    hid_t dataspaceId;
    hid_t did1, did2, did3;
    hid_t aid1, aid2, aid3;
    hid_t fapl_id, dxpl_id;
    const unsigned int nelem=60;
    int *data = NULL, *r_data = NULL, *r2_data = NULL, *data2 = NULL;
    int *buf = NULL;
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
    herr_t ret;
    uint32_t cs = 0,read1_cs = 0, read2_cs = 0;
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
    assert(file_id);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Create Three Groups, a Named datatype, an Attribute, and 3 Datasets \n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* create 3 groups in the file. All calls are completely
       asychronous. Even though they depend one each other here and
       they also depend on the file, they will be shipped immdeiately
       and asynchronously to the server and return to the user. At the
       server, dependencies are noted in the AXE and functions execute
       in the order they are supposed to execute. */
    gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    ret = H5Oset_comment_ff(gid1, "Testing Object Comment", 0, event_q);
    assert(ret == 0);
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
    ret = H5Tcommit_ff(file_id, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                       0, event_q);
    assert(ret == 0);

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
    ret = H5Aclose_ff(aid1, event_q);
    assert(ret == 0);

    /* Check if the attribute on group G1 exists. This is
       asynchronous, so the exists status is correct to examine after
       the completion of the operation */
    ret = H5Aexists_by_name_ff(file_id,"G1","ATTR1", H5P_DEFAULT, &exists, 0, event_q);
    assert(ret == 0);

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
        printf("Attribute ATTR1 does NOT exist. This must be the test without a storage backend\n");

    /* Delete the attribute just created, this is asynchronous */
    ret = H5Adelete_by_name_ff(file_id, "G1", "ATTR1", H5P_DEFAULT, 0, event_q);
    assert(ret == 0);

    /* check if it exists now. This is asynchronous. Can't check the
       exists status until the operation completes. */
    ret = H5Aexists_ff(gid1, "ATTR1", &exists, 0, event_q);
    assert(ret == 0);

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
    ret = H5Arename_ff(did1, "ATTR2_tmp", "ATTR2", 0, event_q);
    assert(ret == 0);

    /* write data to the attribute. This is asynchronous but will wait
       for the creation and rename calls at the server to complete */
    ret = H5Awrite_ff(aid2, int_id, a_data, 0, event_q);
    assert(ret == 0);

    /* similar to the previous H5Dcreate. */
    did2 = H5Dcreate_ff(file_id,"G1/G2/G3/D2",int_id,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did2);

    /* similar to the previous H5Dcreate. */
    did3 = H5Dcreate_ff(file_id,"G1/G2/G3/D3",int_id,dataspaceId,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    assert(did3);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Perform Dataset I/O operations on the created datasets\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* Attach a checksum to the dxpl which is verified all the way
       down at the server */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    cs = H5checksum(data, sizeof(int) * nelem, NULL);
    H5Pset_dxpl_checksum(dxpl_id, cs);

    /* Raw data write on D1. This is asynchronous, but it is delayed
       internally at the server until the create for D1
       is completed.  Internal to the IOD-VOL plugin client we
       generate a checksum for data and ship it with the write bulk
       data function shipper handle to the server. */
    ret = H5Dwrite_ff(did1, int_id, dataspaceId, dataspaceId, dxpl_id, data, 
                      0, event_q);
    assert(ret == 0);

    /* Raw data write on D2. same as previous, but here we indicate
       through the property list that we want to inject a
       corruption. */
    cs = H5checksum(data2, sizeof(int) * nelem, NULL);
    H5Pset_dxpl_checksum(dxpl_id, cs);
    H5Pset_dxpl_inject_corruption(dxpl_id, 1);
    ret = H5Dwrite_ff(did2, int_id, dataspaceId, dataspaceId, dxpl_id, data2, 
                      0, event_q);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Raw data write on D3. Same as previous; however we specify that
       the data in the buffer is in BE byte order. Type conversion will
       happen at the server when we detect that the dataset type is of
       LE order and the datatype here is in BE order. */
    ret = H5Dwrite_ff(did3, H5T_STD_I16BE, dataspaceId, dataspaceId, H5P_DEFAULT, data3, 
                      0, event_q);
    assert(ret == 0);

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

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    /* Raw data read on D1. This is asynchronous.

       At the server side, since the IOD library is "skeletal" and no
       data exists, I am creating an array with the same size and
       elements as the data that is written.

       The server returns, along with the data array, a checksum for
       the data that should be stored, but for now generated.  */

    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read1_cs);
    /* Issue the read Data */
    ret = H5Dread_ff(did1, int_id, dataspaceId, dataspaceId, dxpl_id, r_data, 
                     0, event_q);
    assert(ret == 0);
    H5Pclose(dxpl_id);

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
    H5Pset_dxpl_inject_corruption(dxpl_id, 1);

    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read2_cs);
    ret = H5Dread_ff(did1, int_id, dataspaceId, dataspaceId, dxpl_id, r2_data, 
                     0, event_q);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Issue other operations to query certain metadata values or
       update previously created objects */
    {
        unsigned intent;
        char temp_name[50];
        hid_t plist_id;

        ret = H5Fget_intent(file_id, &intent);
        assert(ret == 0);

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
        ret = H5Dset_extent_ff(did3, &extent, 0, event_q);
        assert(ret == 0);
    }

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Close Objects\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

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

    /* If the read request did no complete earlier when we tested it, Wait on it now.
       We have to do this since we popped it earlier from the event queue */
    if(H5AO_PENDING == status1) {
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);
    }
    else
        assert(H5AO_SUCCEEDED == status1);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Test Links\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* create two new groups /G4 and /G4/G5 */
    gid1 = H5Gcreate_ff(file_id, "G4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(gid1);
    gid2 = H5Gcreate_ff(file_id, "G4/G5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(gid2);

    /* create a hard link to D1 (created previosuly) so that it can be
       accessed from G5/newD1. This is asynchronous; however all
       operation that depend on G5 now will have this operation as a
       parent at the AXE on the server. */
    ret = H5Lcreate_hard_ff(file_id, "G1/G2/G3/D1", gid1, "G5/newD1", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(ret == 0);

    /* Try and open the dataset. This is asynchronous. */
    did1 = H5Dopen_ff(file_id,"G4/G5/newD1", H5P_DEFAULT, 0, event_q);
    assert(did1);

    ret = H5Lcreate_soft_ff("/G1/G2/G3/D4", gid1, "G5/newD2", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(ret == 0);

    {
        H5L_ff_info_t linfo;
        char *link_buf;

        ret = H5Lget_info_ff(gid1, "G5/newD2", &linfo, H5P_DEFAULT, 0, event_q);
        assert(ret == 0);

        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);

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

        ret = H5Lget_val_ff(gid1, "G5/newD2", link_buf, linfo.u.val_size, H5P_DEFAULT, 0, event_q);
        assert(ret == 0);

        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);

        fprintf(stderr, "Link value = %s\n", link_buf);

        free(link_buf);
    }

    ret = H5Lmove_ff(file_id, "/G1/G2/G3/D3", file_id, "/G4/G5/D3moved", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(ret == 0);
    ret = H5Ldelete_ff(file_id, "/G1/G2/G3/D2", H5P_DEFAULT, 0, event_q);
    assert(ret == 0);

    assert(H5Dclose_ff(did1, event_q) == 0);
    assert(H5Gclose_ff(gid1, event_q) == 0);
    assert(H5Gclose_ff(gid2, event_q) == 0);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Flush and Close File\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* flush all the contents of file to disk. This is a barrier task
       at the server. This is asynchronous. */
    ret = H5Fflush_ff(file_id, H5F_SCOPE_GLOBAL, event_q);
    assert(ret == 0);

    /* closing the container also acts as a wait all on all pending requests 
       on the container. */
    assert(H5Fclose_ff(file_id, event_q) == 0);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Wait on everything in EQ and check Results of operations in EQ\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

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
    fprintf(stderr, "Checksum Receieved = %u  Checksum Computed = %u (Should be Equal)\n", read1_cs, cs);

    /* Print the data that has been read with an injected fault,
       This should print the array similar to the previous one, but with the 
       first value modified to be 10 (the injected error) */
    fprintf(stderr, "Printing Corrupted Data ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",r2_data[i]);
    fprintf(stderr, "\n");
    fprintf(stderr, "Checksum Receieved = %u  Checksum Computed = %u (Should NOT be Equal)\n", read2_cs, cs);

    assert(read1_cs == cs);
    assert(read2_cs != cs);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Open The Container\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

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

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Test H5O* routines. H5Oopens are synchronous\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* test object copy. This is asynchronous */
    ret = H5Ocopy_ff(file_id, "/G1/G2/G3/D1", file_id, "D1_copy", 
                     H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(ret == 0);

    /* check if an object exists. This is asynchronous, so checking
       the value should be done after the wait */
    ret = H5Oexists_by_name_ff(file_id, "G1/G2/G3", &exists, H5P_DEFAULT, 0, event_q);
    assert(ret == 0);

    /* Open objects using the general H5O routines. This has an
       asynchronous interface, but right now we implement it
       synchronously, because we don't the object type to be able to
       generate the ID. */
    gid1 = H5Oopen(file_id, "G1", H5P_DEFAULT);
    assert(gid1);
    int_id = H5Oopen(file_id, "int", H5P_DEFAULT);
    assert(int_id);
    did1 = H5Oopen(file_id,"G1/G2/G3/D1", H5P_DEFAULT);
    assert(did1);

    /* open attribute by name. This is asynchronous. */
    aid2 = H5Aopen_by_name_ff(file_id, "G1/G2/G3/D1", "ATTR2", 
                              H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(aid2);

    assert(H5Aclose_ff(aid2, event_q) == 0);
    assert(H5Oclose_ff(did1, event_q) == 0);
    assert(H5Oclose_ff(int_id, event_q) == 0);
    assert(H5Oclose_ff(gid1, event_q) == 0);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Test Regular object Open Containers and Read from them\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* Open a group G1 on the file. This is asynchronous */
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, 0, event_q);
    assert(gid1);

    /* get operations on the group */
    {
        ssize_t ret_size = 0;
        char *comment = NULL;
        H5O_ff_info_t oinfo;

        ret = H5Oget_comment_ff(gid1, NULL, 0, &ret_size, 0, event_q);
        assert(ret == 0);

        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);
        fprintf(stderr, "size of comment is %d\n", ret_size);

        comment = malloc((size_t)ret_size + 1);

        ret = H5Oget_comment_ff(gid1, comment, (size_t)ret_size + 1, &ret_size, 0, event_q);
        assert(ret == 0);
        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);
        fprintf(stderr, "size of comment is %d Comment is %s\n", ret_size, comment);
        free(comment);

        ret = H5Oget_info_ff(gid1, &oinfo, 0, event_q);
        assert(ret == 0);
        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);

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
    memset(r_data, 0, nelem*sizeof(int));
    ret = H5Dread_ff(did1, H5T_STD_I16BE, dataspaceId, dataspaceId, H5P_DEFAULT, r_data, 
                     0, event_q);
    assert(ret == 0);
    if(H5EQpop(event_q, &req1) < 0)
        exit(1);
    assert(H5AOwait(req1, &status1) == 0);
    assert (status1);
    fprintf(stderr, "Printing Dataset value read in 16 bit BE order. This should be interesting: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",r_data[i]);
    fprintf(stderr, "\n");

    /* Raw data read on D1. This is asynchronous.  The read is done into a 
       noncontiguous memory dataspace selection */
    {
        hid_t mem_space;
        hsize_t start = 0;
        hsize_t stride = 2;
        hsize_t count = 60;
        hsize_t block = 1;

        buf = calloc (120, sizeof(int));

        /* create a dataspace. This is a local Bookeeping operation that 
           does not touch the file */
        dims [0] = 120;
        mem_space = H5Screate_simple(1, dims, NULL);
        H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, &start,&stride,&count,&block);

        ret = H5Dread_ff(did1, H5T_STD_I32LE, mem_space, dataspaceId, H5P_DEFAULT, buf, 
                         0, event_q);
        assert(ret == 0);
        H5Sclose(mem_space);
    }

    H5Sclose(dataspaceId);
    assert(H5Dclose(did1) == 0);

    /* open attribute on dataset D1. This is asynchronous */
    aid2 = H5Aopen_by_name_ff(file_id, "G1/G2/G3/D1", "ATTR2", H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    assert(aid2);

    /* read data from attribute. this is asynchronous */
    ret = H5Aread_ff(aid2, int_id, ra_data, 0, event_q);
    assert(ret == 0);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Close all open objects then Wait for events in EQ\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    assert(H5Tclose(int_id) == 0);
    assert(H5Aclose(aid2) == 0);
    assert(H5Gclose(gid1) == 0);

    {
        hid_t map,dset,dtype,grp;

        dset = H5Oopen_by_addr_ff(file_id, 123456789, H5O_TYPE_DATASET, 0, H5_EVENT_QUEUE_NULL);
        dtype = H5Oopen_by_addr_ff(file_id, 123456789, H5O_TYPE_NAMED_DATATYPE, 0, H5_EVENT_QUEUE_NULL);
        map = H5Oopen_by_addr_ff(file_id, 123456789, H5O_TYPE_MAP, 0, H5_EVENT_QUEUE_NULL);
        grp = H5Oopen_by_addr_ff(file_id, 123456789, H5O_TYPE_GROUP, 0, H5_EVENT_QUEUE_NULL);

        H5Dclose_ff(dset, event_q);
        H5Tclose_ff(dtype, event_q);
        H5Mclose_ff(map, event_q);
        H5Gclose_ff(grp, event_q);
    }

    assert(H5Fclose(file_id) == 0);

    /* wait on all requests in event queue */
    H5EQwait(event_q, &num_requests, &status);
    fprintf(stderr, "%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    fprintf(stderr, "Printing all Dataset values. We should have a 0 after each element: ");
    for(i=0;i<120;++i)
        fprintf(stderr, "%d ", buf[i]);
    fprintf(stderr, "\n");

    /*
    fprintf(stderr, "Printing Attribute data (after EQ wait): ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",ra_data[i]);
    fprintf(stderr, "\n");
    */

    if(exists)
        printf("Group G3 exists!\n");
    else
        printf("Group G3 does NOT exist. This must be the test without a storage backend\n");

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
    free(ra_data);
    free(a_data);
    free(data2);
    free(r2_data);
    free(r3_data);
    free(data3);
    free(buf);

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
