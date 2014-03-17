/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

int main(int argc, char **argv) {
    const char file_name[]="eff_file_vl_dset.h5";

    hid_t file_id;
    hid_t sid, vl_dtid, str_dtid;
    hid_t did1, did2;
    hid_t tid1, rid1, rid2;
    hid_t fapl_id, dxpl_id;
    hid_t e_stack;

    hvl_t wdata[5];
    hvl_t rdata[5];
    const char *str_wdata[5]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure.",
        "Abraham Lincoln"
    };   /* Information to write */
    char *str_rdata[5];
    int increment, j, n;

    uint64_t version;
    uint64_t trans_num;

    hsize_t dims[1], max_dims[1];

    void *dset_token1, *dset_token2;
    size_t token_size1, token_size2;
    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req, mpi_reqs[4];

    H5ES_status_t status;
    size_t num_events = 0;
    unsigned int i = 0;
    uint32_t cs_scope = 0;
    herr_t ret;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* create an event Queue for managing asynchronous requests. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* set the metada data integrity checks to happend at transfer through mercury */
    cs_scope |= H5_CHECKSUM_TRANSFER;
    ret = H5Pset_metadata_integrity_scope(fapl_id, cs_scope);
    assert(ret == 0);

    /* create the file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL);
    assert(file_id > 0);

    /* acquire container version 1 - EXACT.  
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    version = 1;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(1 == version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)2);
    assert(tid1);

    /* Create datatypes */
    vl_dtid = H5Tvlen_create (H5T_NATIVE_UINT);
    str_dtid = H5Tcopy(H5T_C_S1);
    H5Tset_size(str_dtid, H5T_VARIABLE);

    /* create a dataspace. This is a local Bookeeping operation that 
       does not touch the file */
    dims [0] = 5;
    sid = H5Screate_simple(1, dims, NULL);

    /* start transaction 1 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        trans_num = 2;
        ret = H5TRstart(tid1, H5P_DEFAULT, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        /* Leader also create some objects in transaction 1 */

        /* create datasets */
        did1 = H5Dcreate_ff(file_id, "D1", vl_dtid, sid, H5P_DEFAULT, H5P_DEFAULT, 
                            H5P_DEFAULT, tid1, e_stack);
        assert(did1 > 0);
        did2 = H5Dcreate_ff(file_id, "D2", str_dtid, sid, H5P_DEFAULT, H5P_DEFAULT, 
                            H5P_DEFAULT, tid1, e_stack);
        assert(did2 > 0);
    }

    /* Tell Delegates that transaction 1 is started */
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Do the local-to-global, global-to-local, so all delegates can
       write to the dsets created in transaction 1 */

    if(0 == my_rank) {
        /* get the token size of each dset */
        ret = H5Oget_token(did1, NULL, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, NULL, &token_size2);
        assert(0 == ret);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);

        /* get the token buffer */
        ret = H5Oget_token(did1, dset_token1, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, dset_token2, &token_size2);
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
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
    }

    /* Leader can continue writing to transaction 1, 
       while others wait for the ibcast to complete */
    if(0 != my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        assert(2 == trans_num);

        /* recieve the token sizes */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Waitall(2, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);

        /* recieve the tokens */
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Waitall(2, mpi_reqs, MPI_STATUS_IGNORE);

        did1 = H5Oopen_by_token(dset_token1, tid1, e_stack);
        did2 = H5Oopen_by_token(dset_token2, tid1, e_stack);
    }

    /* write data to datasets */

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    /* tell HDF5 to disable all data integrity checks for this write */
    cs_scope = 0;
    ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
    assert(ret == 0);

    n = 0;
    increment = 4;
    /* Allocate and initialize VL data to write */
    for(i = 0; i < 5; i++) {
        int temp = i*increment + increment;

        wdata[i].p = malloc(temp * sizeof(unsigned int));
        wdata[i].len = temp;
        for(j = 0; j < temp; j++)
            ((unsigned int *)wdata[i].p)[j] = n ++;
    } /* end for */

    ret = H5Dwrite_ff(did1, vl_dtid, H5S_ALL, H5S_ALL, dxpl_id, wdata, tid1, H5_EVENT_STACK_NULL);
    assert(ret == 0);

    ret = H5Dwrite_ff(did2, str_dtid, H5S_ALL, H5S_ALL, dxpl_id, str_wdata, tid1, H5_EVENT_STACK_NULL);
    assert(ret == 0);

    H5Pclose(dxpl_id);

    /* none leader procs have to complete operations before notifying the leader */
    if(0 != my_rank) {
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

        /* make this synchronous so we know the container version has been acquired */
        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    /* Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);

    /* Tell other procs that container version 1 is acquired */
    version = 2;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }

    /* read data from datasets with read version 1. */
    ret = H5Dread_ff(did1, vl_dtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata, 
                     rid2, H5_EVENT_STACK_NULL);
    assert(ret == 0);
    ret = H5Dread_ff(did2, str_dtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, str_rdata, 
                     rid2, H5_EVENT_STACK_NULL);
    assert(ret == 0);

    MPI_Barrier(MPI_COMM_WORLD);
    if(my_rank == 0) {
        /* release container version 2. This is async. */
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);
    }

    
    /* Print VL DATA */
    for(i = 0; i < 5; i++) {
        int temp = i*increment + increment;

        fprintf(stderr, "Element %d  size %zu: ", i, rdata[i].len);
        for(j = 0; j < temp; j++)
            fprintf(stderr, "%d ",((unsigned int *)rdata[i].p)[j]);
        fprintf(stderr, "\n");
    } /* end for */

    H5Dvlen_reclaim(vl_dtid, sid, H5P_DEFAULT, rdata);
    H5Dvlen_reclaim(vl_dtid, sid, H5P_DEFAULT, wdata);

    fprintf(stderr, "Reading VL Strings: \n");
    for(i=0 ; i<5 ; i++) {
        fprintf(stderr, "%s\n", str_rdata[i]);
    }

    H5Dvlen_reclaim(str_dtid, sid, H5P_DEFAULT, str_rdata);

    /* close objects */
    ret = H5Dclose_ff(did1, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(did2, e_stack);
    assert(ret == 0);

    H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);

    ret = H5Sclose(sid);
    assert(ret == 0);
    ret = H5Tclose(vl_dtid);
    assert(ret == 0);
    ret = H5Tclose(str_dtid);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

    H5ESclear(e_stack);

    ret = H5ESclose(e_stack);
    assert(ret == 0);

    free(dset_token1);
    free(dset_token2);

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
