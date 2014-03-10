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
    const char file_name1[]="eff_file_1";
    const char file_name2[]="eff_file_2";
    hid_t fid1, fid2;
    hid_t gid1, gid2;
    hid_t did1, did2;
    hid_t dtid1, dtid2;
    hid_t sid;
    hid_t tid1, tid2, rid1, rid2, rid3, rid4;
    hid_t fapl_id, trspl_id;
    hid_t e_stack;

    void *token1, *token2, *token3, *token4, *token5, *token6;
    size_t token_size1, token_size2, token_size3, token_size4, token_size5, token_size6;
    uint64_t version;
    uint64_t trans_num;

    const unsigned int nelem=60;
    int *wdata1 = NULL, *wdata2 = NULL;
    int *rdata1 = NULL, *rdata2 = NULL;
    unsigned int i = 0;
    hsize_t dims[1];
    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req, mpi_reqs[12];

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

    /* allocate and initialize arrays for dataset & attribute writes and reads. 
       The write arrays are intialized to contain 60 integers (0-59). 
       The read arrays are intialized to contain 60 integers all 0s. */
    wdata1 = malloc (sizeof(int)*nelem);
    wdata2 = malloc (sizeof(int)*nelem);
    rdata1 = malloc (sizeof(int)*nelem);
    rdata2 = malloc (sizeof(int)*nelem);
    for(i=0;i<nelem;++i) {
        rdata1[i] = 0;
        wdata1[i]=my_rank+1;
        rdata2[i] = 0;
        wdata2[i]=my_rank+1;
    }

    /* create a 32 bit integer LE datatype. This is a local operation
       that does not touch the file */
    dtid1 = H5Tcopy(H5T_NATIVE_INT);
    dtid2 = H5Tcopy(H5T_NATIVE_INT);

    /* create a dataspace. This is a local Bookeeping operation that 
       does not touch the file */
    dims [0] = 60;
    sid = H5Screate_simple(1, dims, NULL);

    /* create an event Queue for managing asynchronous requests.

       Event Queues will releive the use from managing and completing
       individual requests for every operation. Instead of passing a
       request for every operation, the event queue is passed and
       internally the HDF5 library creates a request and adds it to
       the event queue.

       Multiple Event queue can be created used by the application. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* create the file. */
    fid1 = H5Fcreate(file_name1, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(fid1);
    /* create second file */
    fid2 = H5Fcreate(file_name2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(fid2);

    /* acquire container version 0 on both containers - EXACT 
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    version = 0;
    rid1 = H5RCacquire(fid1, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);
    rid2 = H5RCacquire(fid2, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    /* create transaction objects */
    tid1 = H5TRcreate(fid1, rid1, (uint64_t)1);
    assert(tid1);
    tid2 = H5TRcreate(fid2, rid2, (uint64_t)1);
    assert(tid2);

    /* start transaction 1 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        ret = H5TRstart(tid1, H5P_DEFAULT, e_stack);
        assert(0 == ret);
        ret = H5TRstart(tid2, H5P_DEFAULT, e_stack);
        assert(0 == ret);

        /* create groups */
        gid1 = H5Gcreate_ff(fid1, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        gid2 = H5Gcreate_ff(fid2, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid2, e_stack);

        /* Commit datatypes to the files */
        H5Tcommit_ff(fid1, "int", dtid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                     tid1, e_stack);
        H5Tcommit_ff(fid2, "int", dtid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                     tid2, e_stack);

        /* create Datasets */
        did1 = H5Dcreate_ff(gid1, "D1", dtid1, sid,
                            H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid1, e_stack);
        did2 = H5Dcreate_ff(gid2, "D1", dtid2, sid,
                            H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid2, e_stack);

        /* Raw data write on D1. */
        H5Dwrite_ff(did1, dtid1, sid, sid, H5P_DEFAULT, wdata1, tid1, e_stack);
        H5Dwrite_ff(did2, dtid2, sid, sid, H5P_DEFAULT, wdata2, tid2, e_stack);

        trans_num = 1;
    }

    /* Tell other procs that transaction 1 is started */
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Do the local-to-global, global-to-local, so all delegates can
       write the gids created in transaction 1 */

    if(0 == my_rank) {
        /* get the token size of each map */
        ret = H5Oget_token(gid1, NULL, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(gid2, NULL, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(dtid1, NULL, &token_size3);
        assert(0 == ret);
        ret = H5Oget_token(dtid2, NULL, &token_size4);
        assert(0 == ret);
        ret = H5Oget_token(did1, NULL, &token_size5);
        assert(0 == ret);
        ret = H5Oget_token(did2, NULL, &token_size6);
        assert(0 == ret);

        /* allocate buffers for each token */
        token1 = malloc(token_size1);
        token2 = malloc(token_size2);
        token3 = malloc(token_size3);
        token4 = malloc(token_size4);
        token5 = malloc(token_size5);
        token6 = malloc(token_size6);

        /* get the token buffer */
        ret = H5Oget_token(gid1, token1, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(gid2, token2, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(dtid1, token3, &token_size3);
        assert(0 == ret);
        ret = H5Oget_token(dtid2, token4, &token_size4);
        assert(0 == ret);
        ret = H5Oget_token(did1, token5, &token_size5);
        assert(0 == ret);
        ret = H5Oget_token(did2, token6, &token_size6);
        assert(0 == ret);

        /* make sure the create operations have completed before
           telling the delegates to open them */
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);

        /* bcast the token sizes and the tokens */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Ibcast(&token_size4, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(&token_size5, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(&token_size6, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
        MPI_Ibcast(token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[6]);
        MPI_Ibcast(token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[7]);
        MPI_Ibcast(token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[8]);
        MPI_Ibcast(token4, token_size4, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[9]);
        MPI_Ibcast(token5, token_size5, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[10]);
        MPI_Ibcast(token6, token_size6, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[11]);
    }

    /* Leader can continue writing to transaction 1, 
       while others wait for the ibcast to complete */
    if(0 != my_rank) {

        /* this wait if for the transaction start */
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        assert(1 == trans_num);

        /* recieve the token sizes */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Ibcast(&token_size4, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(&token_size5, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(&token_size6, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
        MPI_Waitall(6, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffers for each token */
        token1 = malloc(token_size1);
        token2 = malloc(token_size2);
        token3 = malloc(token_size3);
        token4 = malloc(token_size4);
        token5 = malloc(token_size5);
        token6 = malloc(token_size6);

        /* recieve the tokens */
        MPI_Ibcast(token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Ibcast(token4, token_size4, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(token5, token_size5, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(token6, token_size6, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
        MPI_Waitall(6, mpi_reqs, MPI_STATUS_IGNORE);

        gid1 = H5Oopen_by_token(token1, tid1, e_stack);
        gid2 = H5Oopen_by_token(token2, tid2, e_stack);
        dtid1 = H5Oopen_by_token(token3, tid1, e_stack);
        dtid2 = H5Oopen_by_token(token4, tid2, e_stack);
        did1 = H5Oopen_by_token(token5, tid1, e_stack);
        did2 = H5Oopen_by_token(token6, tid2, e_stack);

        /* Raw data write on D1. */
        H5Dwrite_ff(did1, dtid1, sid, sid, H5P_DEFAULT, wdata1, tid1, e_stack);
        H5Dwrite_ff(did2, dtid2, sid, sid, H5P_DEFAULT, wdata2, tid2, e_stack);

        /* none leader procs have to complete operations before notifying the leader */
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
    }

    /* Barrier to make sure all processes are done writing so Process
       0 can finish transaction 1 and acquire a read context on it. */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Leader process finishes/commits the transaction */
    if(0 == my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        MPI_Waitall(12, mpi_reqs, MPI_STATUS_IGNORE);

        ret = H5TRfinish(tid1, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
        assert(0 == ret);
        ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the first transaction object. Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);
    ret = H5TRclose(tid2);
    assert(0 == ret);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5RCrelease(rid2, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

    version = 1;
    rid1 = H5RCacquire(fid1, &version, H5P_DEFAULT, e_stack);
    assert(1 == version);
    rid2 = H5RCacquire(fid2, &version, H5P_DEFAULT, e_stack);
    assert(1 == version);

    {
        hid_t dxpl_id;
        uint64_t cs_scope;

        dxpl_id = H5Pcreate (H5P_DATASET_XFER);
        /* tell HDF5 to disable all data integrity checks for this write */
        cs_scope = 0;
        ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
        assert(ret == 0);

        ret = H5Dread_ff(did1, dtid1, sid, sid, dxpl_id, rdata1, rid1, e_stack);
        assert(ret == 0);
        ret = H5Dread_ff(did2, dtid2, sid, sid, dxpl_id, rdata2, rid2, e_stack);
        assert(ret == 0);

        H5Pclose(dxpl_id);

        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);

        fprintf(stderr, "Read Data from container 1: ");
        for(i=0;i<nelem;++i)
            fprintf(stderr, "%d ",rdata1[i]);
        fprintf(stderr, "\n");

        fprintf(stderr, "Read Data from container 2: ");
        for(i=0;i<nelem;++i)
            fprintf(stderr, "%d ",rdata2[i]);
        fprintf(stderr, "\n");
    }

    MPI_Barrier(MPI_COMM_WORLD);
    /* release container version 1. This is async. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);
    ret = H5RCrelease(rid2, e_stack);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Close Objects\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    assert(H5Dclose_ff(did1, e_stack) == 0);
    assert(H5Dclose_ff(did2, e_stack) == 0);

    assert(H5Tclose_ff(dtid1, e_stack) == 0);
    assert(H5Tclose_ff(dtid2, e_stack) == 0);

    assert(H5Gclose_ff(gid1, e_stack) == 0);
    assert(H5Gclose_ff(gid2, e_stack) == 0);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

    assert(H5Fclose_ff(fid1, 1, H5_EVENT_STACK_NULL) == 0);
    assert(H5Fclose_ff(fid2, 1, H5_EVENT_STACK_NULL) == 0);

    /* wait on all requests and print completion status */
    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    H5ESclose(e_stack);
    H5Sclose(sid);
    H5Pclose(fapl_id);

    free(token1);
    free(token2);
    free(token3);
    free(token4);
    free(token5);
    free(token6);

    free(wdata1);
    free(rdata1);
    free(wdata2);
    free(rdata2);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Finalize EFF stack\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    MPI_Barrier(MPI_COMM_WORLD);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
