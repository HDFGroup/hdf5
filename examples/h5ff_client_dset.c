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
    const char file_name[]="eff_file.h5";

    hid_t file_id;
    hid_t gid1, gid2, gid3;
    hid_t sid, dtid;
    hid_t did1, did2, did3;
    hid_t tid1, tid2, rid1, rid2;
    hid_t fapl_id, trspl_id, dxpl_id;
    hid_t event_q;

    uint64_t version;
    uint64_t trans_num;

    int32_t *wdata1 = NULL, *wdata2 = NULL;
    int16_t *wdata3 = NULL;
    int32_t *rdata1 = NULL, *rdata2 = NULL;
    int16_t *rdata3 = NULL;
    const unsigned int nelem=60;
    hsize_t dims[1];
    hsize_t extent;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req;

    H5_status_t *status = NULL;
    int num_requests = 0;
    unsigned int i = 0;
    uint32_t cs = 0,read1_cs = 0, read2_cs = 0;
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

    /* allocate and initialize arrays for dataset I/O */
    wdata1 = malloc (sizeof(int32_t)*nelem);
    wdata2 = malloc (sizeof(int32_t)*nelem);
    wdata3 = malloc (sizeof(int16_t)*nelem);
    rdata1 = malloc (sizeof(int32_t)*nelem);
    rdata2 = malloc (sizeof(int32_t)*nelem);
    rdata3 = malloc (sizeof(int16_t)*nelem);
    for(i=0;i<nelem;++i) {
        rdata1[i] = 0;
        rdata2[i] = 0;
        rdata3[i] = 0;
        wdata1[i]=i;
        wdata2[i]=i;
        wdata3[i]=i;
    }

    /* create an event Queue for managing asynchronous requests. */
    event_q = H5EQcreate(fapl_id);
    assert(event_q);

    /* set the metada data integrity checks to happend at transfer through mercury */
    cs_scope |= H5_CHECKSUM_TRANSFER;
    ret = H5Pset_metadata_integrity_scope(fapl_id, cs_scope);
    assert(ret == 0);

    /* create the file. */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(file_id > 0);

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

    /* create group hierarchy /G1/G2/G3 */
    gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(gid1 > 0);
    gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(gid2 > 0);
    gid3 = H5Gcreate_ff(gid2, "G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(gid3 > 0);

    /* create datasets */
    did1 = H5Dcreate_ff(gid1, "D1", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(did1 > 0);
    did2 = H5Dcreate_ff(gid2, "D2", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(did2 > 0);
    did3 = H5Dcreate_ff(gid3, "D3", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(did3 > 0);

    /* write data to datasets */

    /* Attach a checksum to the dxpl which is verified all the way
       down at the server */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    cs = H5checksum(wdata1, sizeof(int32_t) * nelem, NULL);
    H5Pset_dxpl_checksum(dxpl_id, cs);

    /* tell HDF5 to disable all data integrity checks for this write */
    cs_scope = 0;
    ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
    assert(ret == 0);

    ret = H5Dwrite_ff(did1, dtid, sid, sid, dxpl_id, wdata1, tid1, event_q);
    assert(ret == 0);

    /* Raw data write on D2. same as previous, but here we indicate
       through the property list that we want to inject a
       corruption. */
    cs = H5checksum(wdata2, sizeof(int32_t) * nelem, NULL);
    H5Pset_dxpl_checksum(dxpl_id, cs);
    H5Pset_dxpl_inject_corruption(dxpl_id, 1);

    /* tell HDF5 to disable data integrity checks stored at IOD for this write;
       The transfer checksum will still capture the corruption. */
    cs_scope |= H5_CHECKSUM_TRANSFER;
    ret = H5Pset_rawdata_integrity_scope(dxpl_id, cs_scope);
    assert(ret == 0);

    ret = H5Dwrite_ff(did2, dtid, sid, sid, dxpl_id, wdata2, tid1, event_q);
    assert(ret == 0);

    /* Raw data write on D3. Same as previous; however we specify that
       the data in the buffer is in BE byte order. Type conversion will
       happen at the server when we detect that the dataset type is of
       LE order and the datatype here is in BE order. */
    ret = H5Dwrite_ff(did3, H5T_STD_I16BE, sid, sid, H5P_DEFAULT, wdata3, tid1, event_q);
    assert(ret == 0);

    H5Pclose(dxpl_id);

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

        /* make this synchronous so we know the container version has been acquired */
        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_QUEUE_NULL);
        assert(0 == ret);
    }

    /* another barrier so other processes know that container version is acquried */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* close some objects */
    ret = H5Dclose_ff(did1, event_q);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, event_q);
    assert(ret == 0);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, event_q);
    assert(0 == ret);

    H5EQwait(event_q, &num_requests, &status);
    printf("%d requests in event queue. Completions: ", num_requests);
    for(i=0 ; i<num_requests; i++)
        fprintf(stderr, "%d ",status[i]);
    fprintf(stderr, "\n");
    free(status);

    /* Tell other procs that container version 1 is acquired */
    version = 1;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }

    /* Open objects closed before */
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid2, event_q);
    did1 = H5Dopen_ff(file_id, "G1/D1", H5P_DEFAULT, rid2, event_q);

    /* read data from datasets with read version 1. */

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read1_cs);
    ret = H5Dread_ff(did1, dtid, sid, sid, dxpl_id, rdata1, rid2, event_q);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Here we demo that we can pass hints down to the IOD server. 
       We create a new property, for demo purposes, to tell the server to inject 
       corrupted data into the received array, and hence an incorrect checksum. 
       This also detects that we are passing checksum values in both directions for 
       raw data to ensure data integrity. The read should fail when we wait on it in
       the H5Dclose(D2) later, but for the demo purposes we are not actually going to 
       fail the close, but just print a Fatal error. */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    H5Pset_dxpl_inject_corruption(dxpl_id, 1);
    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read2_cs);

    ret = H5Dread_ff(did2, dtid, sid, sid, dxpl_id, rdata2, rid2, event_q);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Raw data read on D3. This is asynchronous.  Note that the type
       is different than the dataset type. */
    ret = H5Dread_ff(did3, H5T_STD_I16BE, sid, sid, H5P_DEFAULT, rdata3, rid2, event_q);
    assert(ret == 0);



    /* Raw data read on D1. This is asynchronous.  The read is done into a 
       noncontiguous memory dataspace selection */
    {
        hid_t mem_space;
        hsize_t start = 0;
        hsize_t stride = 2;
        hsize_t count = nelem;
        hsize_t block = 1;
        H5_request_t req1;
        H5_status_t status1;
        int *buf = NULL;

        buf = calloc (nelem*2, sizeof(int));

        /* create a dataspace. This is a local Bookeeping operation that 
           does not touch the file */
        dims [0] = nelem*2;
        mem_space = H5Screate_simple(1, dims, NULL);
        H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, &start,&stride,&count,&block);

        ret = H5Dread_ff(did1, H5T_STD_I32LE, mem_space, sid, H5P_DEFAULT, buf, 
                         rid2, event_q);
        assert(ret == 0);
        H5Sclose(mem_space);

        if(H5EQpop(event_q, &req1) < 0)
            exit(1);
        /* wait synchronously on the operation */
        assert(H5AOwait(req1, &status1) == 0);
        assert (status1);

        fprintf(stderr, "Printing all Dataset values. We should have a 0 after each element: ");
        for(i=0;i<120;++i)
            fprintf(stderr, "%d ", buf[i]);
        fprintf(stderr, "\n");

        free(buf);
    }


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

    extent = 10;
    ret = H5Dset_extent_ff(did1, &extent, tid2, event_q);
    assert(ret == 0);

    extent = 30;
    ret = H5Dset_extent_ff(did2, &extent, tid2, event_q);
    assert(ret == 0);

    extent = 60;
    ret = H5Dset_extent_ff(did3, &extent, tid2, event_q);
    assert(ret == 0);

    /* finish transaction 2 */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, event_q);
    assert(0 == ret);

    /* release container version 1. This is async. */
    ret = H5RCrelease(rid2, event_q);
    assert(0 == ret);

    /* close objects */
    ret = H5Dclose_ff(did1, event_q);
    assert(ret == 0);
    ret = H5Dclose_ff(did2, event_q);
    assert(ret == 0);
    ret = H5Dclose_ff(did3, event_q);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, event_q);
    assert(ret == 0);
    ret = H5Gclose_ff(gid2, event_q);
    assert(ret == 0);
    ret = H5Gclose_ff(gid3, event_q);
    assert(ret == 0);

    ret = H5Sclose(sid);
    assert(ret == 0);
    ret = H5Tclose(dtid);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);

    H5Fclose_ff(file_id, event_q);

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

    fprintf(stderr, "Read Data1: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata1[i]);
    fprintf(stderr, "\n");
    fprintf(stderr, 
            "Checksum Receieved = %u  Checksum Computed = %u (Should be Equal)\n", 
            read1_cs, cs);

    fprintf(stderr, "Read Data2 (corrupted): ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata2[i]);
    fprintf(stderr, "\n");
    fprintf(stderr, 
            "Checksum Receieved = %u  Checksum Computed = %u (Should NOT be Equal)\n", 
            read2_cs, cs);

    assert(read1_cs == cs);
    assert(read2_cs != cs);

    fprintf(stderr, "Read Data3 (32 LE converted to 16 bit BE order): ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata3[i]);
    fprintf(stderr, "\n");

    ret = H5EQclose(event_q);
    assert(ret == 0);

    free(wdata1);
    free(wdata2);
    free(wdata3);
    free(rdata1);
    free(rdata2);
    free(rdata3);

    EFF_finalize();
    MPI_Finalize();

    return 0;
}
