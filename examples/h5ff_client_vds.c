/* 
 * h5ff_client_vds.c: Client side test for virtual dataset routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mchecksum.h"
#include "mpi.h"
#include "hdf5.h"

#if 0
static uint64_t
checksum_crc64(const void *buf, size_t buf_size)
{
    const char *hash_method = "crc64";
    size_t hash_size;
    mchecksum_object_t checksum;
    uint64_t ret_value = 0;

    /* Initialize checksum */
    mchecksum_init(hash_method, &checksum);

    /* Update checksum */
    mchecksum_update(checksum, buf, buf_size);

    /* Get size of checksum */
    hash_size = mchecksum_get_size(checksum);

    assert(hash_size == sizeof(uint64_t));

    /* get checksum value */
    mchecksum_get(checksum, &ret_value, hash_size, 1);

    /* Destroy checksum */
    mchecksum_destroy(checksum);

    return ret_value;
}
#endif

int main(int argc, char **argv) {
    char file_name[50];
    hid_t file_id;
    hid_t gid1, gid2, gid3;
    hid_t vds_sid, sds_sid;
    hid_t vdsid, sdsid1, sdsid2;
    hid_t tid1, tid2, tid3, rid1, rid2, rid3;
    hid_t fapl_id, dcpl_id, trspl_id, dxpl_id;
    hid_t e_stack;

    uint64_t version;
    uint64_t trans_num;

    int32_t wdata[5][12];
    int32_t rdata[5][12];
    int32_t erdata[5][12];
    //const unsigned int nelem=60;
    hsize_t dims[2] = {5, 12};
    hsize_t start[2] = {0, 0};
    hsize_t stride[2] = {1, 6};
    hsize_t count[2] = {1, 2};
    hsize_t block[2] = {5, 3};

    void *vds_token, *sds_token1, *sds_token2;
    size_t vds_token_size, sds_token_size1, sds_token_size2;
    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req, mpi_reqs[6];

    H5ES_status_t status;
    size_t num_events = 0;
    unsigned int i = 0, j = 0;
    uint64_t array_cs = 0, elmt_cs = 0, read1_cs = 0, read2_cs = 0;
    uint32_t cs_scope = 0;
    herr_t ret;

    sprintf(file_name, "%s_%s", getenv("USER"), "eff_file_dset.h5");

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        fprintf(stderr, "MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    printf("APP processes = %d, my rank is %d\n", my_size, my_rank);

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* create an event Queue for managing asynchronous requests. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* set the metada data integrity checks to happend at transfer through mercury */
    //cs_scope |= H5_CHECKSUM_TRANSFER;
    //ret = H5Pset_metadata_integrity_scope(fapl_id, cs_scope);
    //assert(ret == 0);

    /* create the file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL);
    assert(file_id > 0);

    /* create 2-D dataspace with 60 elements */
    vds_sid = H5Screate_simple(2, dims, NULL);
    
    /* create 2-D dataspace with 30 elements */
    dims[1] = 6;
    sds_sid = H5Screate_simple(2, dims, NULL);

    /* acquire container version 1 - EXACT.  
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    if(0 == my_rank) {
        version = 1;
        rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);
    assert(1 == version);
    if (my_rank != 0)
        rid1 = H5RCcreate(file_id, version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)2);
    assert(tid1);

    /* start transaction 2 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        hid_t dcpl_id;

        trans_num = 2;
        ret = H5TRstart(tid1, H5P_DEFAULT, H5_EVENT_STACK_NULL);
        assert(0 == ret);

        /* Leader also create some objects in transaction 1 */

        /* create group hierarchy /G1/G2/G3 */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(gid1 > 0);
        gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(gid2 > 0);
        gid3 = H5Gcreate_ff(gid2, "G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(gid3 > 0);

        /* Create VDS dcpl */
        dcpl_id = H5Pcreate (H5P_DATASET_CREATE);
        assert(dcpl_id > 0);
        ret = H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, stride, count, block);
        assert(0 == ret);
        ret = H5Pset_virtual(dcpl_id, vds_sid, ".", "/G1/G2/SDS1", sds_sid);
        assert(0 == ret);
        start[1] = 3;
        ret = H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, stride, count, block);
        assert(0 == ret);
        ret = H5Pset_virtual(dcpl_id, vds_sid, ".", "/G1/G2/G3/SDS2", sds_sid);
        assert(0 == ret);

        //H5Pset_dcpl_stripe_count(dcpl_id, 4);
        //H5Pset_dcpl_stripe_size(dcpl_id, 5);

        /* create datasets */
        vdsid = H5Dcreate_ff(gid1, "VDS", H5T_NATIVE_INT32, vds_sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT, tid1, e_stack);
        assert(vdsid > 0);
        H5Pclose(dcpl_id);

        sdsid1 = H5Dcreate_ff(gid2, "SDS1", H5T_NATIVE_INT32, sds_sid, 
                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(sdsid1 > 0);
        sdsid2 = H5Dcreate_ff(gid3, "SDS2", H5T_NATIVE_INT32, sds_sid, 
                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(sdsid2 > 0);
    }

    /* Tell Delegates that transaction 1 is started */
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Do the local-to-global, global-to-local, so all delegates can
       write to the dsets created in transaction 1 */

    if(0 == my_rank) {
        /* get the token size of each dset */
        ret = H5Oget_token(vdsid, NULL, &vds_token_size);
        assert(0 == ret);
        ret = H5Oget_token(sdsid1, NULL, &sds_token_size1);
        assert(0 == ret);
        ret = H5Oget_token(sdsid2, NULL, &sds_token_size2);
        assert(0 == ret);

        /* allocate buffers for each token */
        vds_token = malloc(vds_token_size);
        sds_token1 = malloc(sds_token_size1);
        sds_token2 = malloc(sds_token_size2);

        /* get the token buffer */
        ret = H5Oget_token(vdsid, vds_token, &vds_token_size);
        assert(0 == ret);
        ret = H5Oget_token(sdsid1, sds_token1, &sds_token_size1);
        assert(0 == ret);
        ret = H5Oget_token(sdsid2, sds_token2, &sds_token_size2);
        assert(0 == ret);

        /* make sure the create operations have completed before
           telling the delegates to open them */
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);

        /* bcast the token sizes and the tokens */ 
        MPI_Ibcast(&vds_token_size, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&sds_token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&sds_token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Ibcast(vds_token, vds_token_size, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(sds_token1, sds_token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(sds_token2, sds_token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
    }

    /* Leader can continue writing to transaction 2, 
       while others wait for the ibcast to complete */
    if(0 != my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        assert(2 == trans_num);

        /* recieve the token sizes */ 
        MPI_Ibcast(&vds_token_size, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&sds_token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&sds_token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffers for each token */
        vds_token = malloc(vds_token_size);
        sds_token1 = malloc(sds_token_size1);
        sds_token2 = malloc(sds_token_size2);

        /* recieve the tokens */
        MPI_Ibcast(vds_token, vds_token_size, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(sds_token1, sds_token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(sds_token2, sds_token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        vdsid = H5Oopen_by_token(vds_token, tid1, e_stack);
        sdsid1 = H5Oopen_by_token(sds_token1, tid1, e_stack);
        sdsid2 = H5Oopen_by_token(sds_token2, tid1, e_stack);
    }

    /* Write data to source datasets */
    /* Initialize wdata and erdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            wdata[i][j] = (int32_t)((sizeof(wdata[0]) / sizeof(wdata[0][0])) * i + j);
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            erdata[i][j] = -1;

    /* Attach a checksum to the dxpl which is verified all the way
       down at the server */
    /* Until implemented (maybe never) disable user checksums -NAF */
    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    //array_cs = checksum_crc64(wdata1, sizeof(int32_t) * nelem);
    //H5Pset_dxpl_checksum(dxpl_id, array_cs);
    //printf("Checksum computed for raw data: %016lX\n", array_cs);

    /* Write data to first source dataset */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = 6;
    ret = H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, NULL, count, NULL);
    assert(ret == 0);
    ret = H5Dwrite_ff(sdsid1, H5T_NATIVE_INT32, vds_sid, H5S_ALL, dxpl_id, wdata, tid1, e_stack);
    assert(ret == 0);

    /* Update erdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < 3; j++) {
            erdata[i][j] = wdata[i][j];
            erdata[i][j + 6] = wdata[i][j + 3];
        } /* end for */

    /* Wait for previous write to complete so we can reuse wdata buffer */
    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);
    assert(status == H5ES_STATUS_SUCCEED);

    /* Update wdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            wdata[i][j] += (int32_t)(sizeof(wdata) / sizeof(wdata[0][0]));

    /* Write data to second source dataset */
    start[1] = 6;
    ret = H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, NULL, count, NULL);
    assert(ret == 0);
    ret = H5Dwrite_ff(sdsid2, H5T_NATIVE_INT32, vds_sid, H5S_ALL, dxpl_id, wdata, tid1, e_stack);
    assert(ret == 0);

    /* Update erdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < 3; j++) {
            erdata[i][j + 3] = wdata[i][j + 6];
            erdata[i][j + 9] = wdata[i][j + 9];
        } /* end for */

    /* none leader procs have to complete operations before notifying the leader */
    if(0 != my_rank) {
        H5ESget_count(e_stack, &num_events);
        H5ESwait_all(e_stack, &status);
        H5ESclear(e_stack);
        printf("%d events in event stack. Completion status = %d\n", num_events, status);
        assert(status == H5ES_STATUS_SUCCEED);
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

    /* release container version 1. This is async. */
    if(0 == my_rank) {
        ret = H5RCrelease(rid1, e_stack);
        assert(0 == ret);
    }

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);
    assert(status == H5ES_STATUS_SUCCEED);

    /* Tell other procs that container version 2 is acquired */
    version = 2;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* other processes just create a read context object; no need to
       acquire it */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }

    /* close some objects */
    if(0 == my_rank) {
        ret = H5Gclose_ff(gid1, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid2, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid3, e_stack);
        assert(ret == 0);
    }

    ret = H5Dclose_ff(vdsid, H5_EVENT_STACK_NULL);
    assert(ret == 0);

    /* Open objects closed before */
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid2, e_stack);
    vdsid = H5Dopen_ff(file_id, "G1/VDS", H5P_DEFAULT, rid2, e_stack);

    /* Read data from datasets with read version 2. */
    /* Initialize rdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            rdata[i][j] = -1;


    /* Give a location to the DXPL to store the checksum once the read has completed */
    //H5Pset_dxpl_checksum_ptr(dxpl_id, &read1_cs);
    ret = H5Dread_ff(vdsid, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL, dxpl_id, rdata, rid2, e_stack);
    assert(ret == 0);

    H5ESwait(e_stack, 0, &status);
    printf("ESWait H5Dread Completion status = %d\n", status);
    assert(status == H5ES_STATUS_SUCCEED);

    /* Verify read data */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            assert(rdata[i][j] == erdata[i][j]);

    /* create & start transaction 3 with num_peers = my_size. This
       means all processes are transaction leaders, and all have to
       call start and finish on the transaction. */
    tid2 = H5TRcreate(file_id, rid2, (uint64_t)3);
    assert(tid2);
    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, e_stack);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Do more updates on transaction 3 */

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);
    assert(status == H5ES_STATUS_SUCCEED);
    MPI_Barrier(MPI_COMM_WORLD);

    /* Write data to VDS */
    /* Update wdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            wdata[i][j] += (int32_t)(sizeof(wdata) / sizeof(wdata[0][0]));

    /* Write data */
    ret = H5Dwrite_ff(vdsid, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL, dxpl_id, wdata, tid2, e_stack);
    assert(ret == 0);

    /* Update erdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < 3; j++) {
            erdata[i][j] = wdata[i][j];
            erdata[i][j + 3] = wdata[i][j + 6];
            erdata[i][j + 6] = wdata[i][j + 3];
            erdata[i][j + 9] = wdata[i][j + 9];
        } /* end for */

    /* finish transaction 3 */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", 
           num_events, status);
    H5ESclear(e_stack);
    assert(status == H5ES_STATUS_SUCCEED);
    MPI_Barrier(MPI_COMM_WORLD);

    if(0 == my_rank) {
        version = 3;
        rid3 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    }
    MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
    assert(3 == version);
    if (my_rank != 0)
        rid3 = H5RCcreate(file_id, version);

    /* Read from source datasets */
    /* Initialize rdata */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            rdata[i][j] = -1;

    /* Read from first source dataset */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = 6;
    ret = H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, NULL, count, NULL);
    assert(ret == 0);
    ret = H5Dread_ff(sdsid1, H5T_NATIVE_INT32, vds_sid, H5S_ALL, H5P_DEFAULT, rdata, 
                     rid3, e_stack);
    assert(ret == 0);

    /* Read from second source dataset */
    start[1] = 6;
    ret = H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, NULL, count, NULL);
    assert(ret == 0);
    ret = H5Dread_ff(sdsid2, H5T_NATIVE_INT32, vds_sid, H5S_ALL, H5P_DEFAULT, rdata, 
                     rid3, e_stack);
    assert(ret == 0);

    /* Wait for reads to complete */
    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);
    assert(status == H5ES_STATUS_SUCCEED);

    /* Verify read data */
    for(i = 0; i < sizeof(wdata) / sizeof(wdata[0]); i++)
        for(j = 0; j < sizeof(wdata[0]) / sizeof(wdata[0][0]); j++)
            assert(rdata[i][j] == erdata[i][j]);

    MPI_Barrier(MPI_COMM_WORLD);    
    if(my_rank == 0) {
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);
        ret = H5RCrelease(rid3, e_stack);
        assert(0 == ret);
    }

    /* close objects */
    ret = H5Dclose_ff(vdsid, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(sdsid1, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(sdsid2, e_stack);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, e_stack);
    assert(ret == 0);

    H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);

    H5ESget_count(e_stack, &num_events);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);
    assert(status == H5ES_STATUS_SUCCEED);

    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    assert(status == H5ES_STATUS_SUCCEED);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);
    assert(status == H5ES_STATUS_SUCCEED);

    ret = H5Sclose(vds_sid);
    assert(ret == 0);
    ret = H5Sclose(sds_sid);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);
    ret = H5Pclose(dxpl_id);
    assert(ret == 0);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);

    H5ESclear(e_stack);

    ret = H5ESclose(e_stack);
    assert(ret == 0);

    free(vds_token);
    free(sds_token1);
    free(sds_token2);

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Finalize();

    return 0;
}
