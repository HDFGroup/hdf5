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
    const char file_name1[]="eff_file_1.h5";
    const char file_name2[]="eff_file_2.h5";
    hid_t fid1, fid2;
    hid_t gid1, gid2, gid3, gid4;
    hid_t sid;
    hid_t did1, did2;
    hid_t tid1, tid2;
    hid_t fapl_id;
    const unsigned int nelem=60;
    int *wdata1 = NULL, *wdata2 = NULL;
    int *rdata1 = NULL, *rdata2 = NULL;
    unsigned int i = 0;
    hsize_t dims[1];
    int my_rank, my_size;
    int provided;
    hid_t event_q;
    H5_status_t *status = NULL;
    int num_requests = 0;
    herr_t ret;
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
    wdata1 = malloc (sizeof(int)*nelem);
    wdata2 = malloc (sizeof(int)*nelem);
    rdata1 = malloc (sizeof(int)*nelem);
    rdata2 = malloc (sizeof(int)*nelem);
    for(i=0;i<nelem;++i) {
        rdata1[i] = 0;
        wdata1[i]=i;
        rdata2[i] = 0;
        wdata2[i]=i;
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

    /* create the file. This is asynchronous, but the fid1 can be used. */
    fid1 = H5Fcreate_ff(file_name1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, event_q);
    assert(fid1);

    /* create the file. This is asynchronous, but the fid1 can be used. */
    fid2 = H5Fcreate_ff(file_name2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, event_q);
    assert(fid1);

    /* create groups */
    gid1 = H5Gcreate_ff(fid1, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    gid2 = H5Gcreate_ff(fid1, "G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    gid3 = H5Gcreate_ff(fid2, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);
    gid4 = H5Gcreate_ff(fid2, "G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 0, event_q);

    /* create a 32 bit integer LE datatype. This is a local operation
       that does not touch the file */
    tid1 = H5Tcopy(H5T_STD_I32LE);
    tid2 = H5Tcopy(H5T_STD_I32LE);

    /* Commit the datatype to the file. This is asynchronous & immediate. 
     * Other Local H5T type operations can be issued before completing this call
     * because they do not depend on the committed state of the datatype.
     */
     H5Tcommit_ff(fid1, "int", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                  0, event_q);
     H5Tcommit_ff(fid2, "int", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                  0, event_q);

    /* create a dataspace. This is a local Bookeeping operation that 
       does not touch the file */
    dims [0] = 60;
    sid = H5Screate_simple(1, dims, NULL);

    /* create Datasets */
    did1 = H5Dcreate_ff(fid1,"G1/G2/D1",tid1,sid,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);
    did2 = H5Dcreate_ff(fid2,"G1/G2/D1",tid2,sid,
                        H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, 0, event_q);

    /* Raw data write on D1. */
    H5Dwrite_ff(did1, tid1, sid, sid, H5P_DEFAULT, wdata1, 0, event_q);
    H5Dwrite_ff(did2, tid2, sid, sid, H5P_DEFAULT, wdata2, 0, event_q);

    /* Issue the read Data */
    H5Dread_ff(did1, tid1, sid, sid, H5P_DEFAULT, rdata1, 0, event_q);
    H5Dread_ff(did2, tid2, sid, sid, H5P_DEFAULT, rdata2, 0, event_q);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Close Objects\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    /* closing did1 acts as a barrier task at the server for all
       operations dependeing on the dataset. This is asynchronous. */
    assert(H5Dclose_ff(did1, event_q) == 0);
    assert(H5Dclose_ff(did2, event_q) == 0);

    assert(H5Tclose_ff(tid1, event_q) == 0);
    assert(H5Tclose_ff(tid2, event_q) == 0);

    assert(H5Gclose_ff(gid1, event_q) == 0);
    assert(H5Gclose_ff(gid2, event_q) == 0);
    assert(H5Gclose_ff(gid3, event_q) == 0);
    assert(H5Gclose_ff(gid4, event_q) == 0);

    assert(H5Fclose_ff(fid1, event_q) == 0);
    assert(H5Fclose_ff(fid2, event_q) == 0);

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

    fprintf(stderr, "Printing After Waiting for data in first file");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata1[i]);
    fprintf(stderr, "\n");

    fprintf(stderr, "Printing After Waiting for data in second file");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata2[i]);
    fprintf(stderr, "\n");

    H5EQclose(event_q);
    H5Pclose(fapl_id);

    free(wdata1);
    free(rdata1);
    free(wdata2);
    free(rdata2);

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
