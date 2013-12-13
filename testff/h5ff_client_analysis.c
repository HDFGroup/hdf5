/* 
 * h5ff_client_dset.c: Client side test for Dataset routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

#define NX1 32
#define NY1 64

#define NX2 128
#define NY2 256

/* Local sum and global sum */
const char *split_sum_script =
        "import numpy as np\n"
        "def split(array):\n"
        "  print 'Split sum: ' + str(array.sum())\n"
        "  return np.array([array.sum(), array.size])\n";

const char *combine_sum_script =
        "import numpy as np\n"
        "def combine(arrays):\n"
        "  global_sum = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "  print 'Combined sum: ' + str(global_sum)\n"
        "  return np.array([global_sum, len(arrays)])\n";

int main(int argc, char **argv) {
    const char file_name[]="eff_file.h5";

//    hid_t file_id;
//    hid_t gid1, gid2, gid3;
//    hid_t sid, dtid;
//    hid_t did1, did2, did3;
//    hid_t tid1, tid2, rid1, rid2;
//    hid_t fapl_id, trspl_id, dxpl_id;
    hid_t       tid1, rid1, trspl_id;
    hid_t       e_stack;
    hid_t       file_id, group_id, dataset_id, dataspace_id;  /* identifiers */
    hsize_t     dims[2];
    herr_t      status;
    int         i, j, dset1_data[NX1][NY1], dset2_data[NX2][NY2];
    herr_t ret;

    uint64_t version;
    uint64_t trans_num;

    double query_limit = 39.1;
    hid_t query_id;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req, mpi_reqs[6];

//    H5ES_status_t status;
//    size_t num_events = 0;
//    unsigned int i = 0;
//    uint32_t cs = 0,read1_cs = 0, read2_cs = 0;
//    uint32_t cs_scope = 0;
//    herr_t ret;

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

    /* Initialize the first dataset. */
    for (i = 0; i < NX1; i++)
       for (j = 0; j < NY1; j++)
          dset1_data[i][j] = j + 1;

    /* Initialize the second dataset. */
    for (i = 0; i < NX2; i++)
       for (j = 0; j < NY2; j++)
          dset2_data[i][j] = j + 1;

    /* create an event Queue for managing asynchronous requests. */
    e_stack = H5EScreate();
    assert(e_stack);

    /* Open an existing file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL);

    /* acquire container version 0 - EXACT.
       This can be asynchronous, but here we need the acquired ID
       right after the call to start the transaction so we make synchronous. */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);

    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid1, trspl_id, e_stack);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Create the data space for the first dataset. */
    dims[0] = NX1;
    dims[1] = NY1;
    dataspace_id = H5Screate_simple(2, dims, NULL);

    /* Create a dataset in group "MyGroup". */
    dataset_id = H5Dcreate_ff(file_id, "D1", H5T_NATIVE_INT, dataspace_id,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, H5_EVENT_STACK_NULL);

    dataspace_id = H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, ,);

    /* Write the first dataset. */
    status = H5Dwrite_ff(dataset_id, H5T_NATIVE_INT, H5S_ALL, dataspace_id, H5P_DEFAULT,
            dset1_data, tid1, H5_EVENT_STACK_NULL);

    /* Close the data space for the first dataset. */
    status = H5Sclose(dataspace_id);

    /* Close the first dataset. */
    status = H5Dclose_ff(dataset_id, H5_EVENT_STACK_NULL);

//    /* Open an existing group of the specified file. */
//    group_id = H5Gcreate_ff(file_id, "/G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
//
//    /* Create the data space for the second dataset. */
//    dims[0] = NX2;
//    dims[1] = NY2;
//    dataspace_id = H5Screate_simple(2, dims, NULL);
//
//    /* Create the second dataset in group "/G1/G2". */
//    dataset_id = H5Dcreate_ff(group_id, "D2", H5T_NATIVE_INT, dataspace_id,
//                           H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
//
//    /* Write the second dataset. */
//    status = H5Dwrite_ff(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
//                      dset2_data, tid1, e_stack);
//
//    /* Close the data space for the second dataset. */
//    status = H5Sclose(dataspace_id);
//
//    /* Close the second dataset */
//    status = H5Dclose_ff(dataset_id, e_stack);
//
//    /* Close the group. */
//    status = H5Gclose_ff(group_id, e_stack);

    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the file. */
    status = H5Fclose_ff(file_id, H5_EVENT_STACK_NULL);


    /* start transaction 1 with default Leader/Delegate model. Leader
       which is rank 0 here starts the transaction. It can be
       asynchronous, but we make it synchronous here so that the
       Leader can tell its delegates that the transaction is
       started. */
    if(0 == my_rank) {
        trans_num = 1;
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

        /* create datasets */
        did1 = H5Dcreate_ff(gid1, "D1", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did1 > 0);
        did2 = H5Dcreate_ff(gid2, "D2", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did2 > 0);
        did3 = H5Dcreate_ff(gid3, "D3", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        assert(did3 > 0);

        ret = H5Gclose_ff(gid1, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid2, e_stack);
        assert(ret == 0);
        ret = H5Gclose_ff(gid3, e_stack);
        assert(ret == 0);
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
        ret = H5Oget_token(did3, NULL, &token_size3);
        assert(0 == ret);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);
        dset_token3 = malloc(token_size3);

        /* get the token buffer */
        ret = H5Oget_token(did1, dset_token1, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(did2, dset_token2, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(did3, dset_token3, &token_size3);
        assert(0 == ret);

        /* bcast the token sizes and the tokens */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(dset_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
    }

    /* Leader can continue writing to transaction 1, 
       while others wait for the ibcast to complete */
    if(0 != my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        assert(1 == trans_num);

        /* recieve the token sizes */ 
        MPI_Ibcast(&token_size1, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(&token_size2, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(&token_size3, sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffers for each token */
        dset_token1 = malloc(token_size1);
        dset_token2 = malloc(token_size2);
        dset_token3 = malloc(token_size3);

        /* recieve the tokens */
        MPI_Ibcast(dset_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(dset_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(dset_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        did1 = H5Oopen_by_token(dset_token1, rid1, e_stack);
        did2 = H5Oopen_by_token(dset_token2, rid1, e_stack);
        did3 = H5Oopen_by_token(dset_token3, rid1, e_stack);
    }

    /* write data to datasets */


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

    /* close some objects */
    ret = H5Dclose_ff(did1, e_stack);
    assert(ret == 0);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);
    H5ESclear(e_stack);

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
    gid1 = H5Gopen_ff(file_id, "G1", H5P_DEFAULT, rid2, e_stack);
    did1 = H5Dopen_ff(file_id, "G1/D1", H5P_DEFAULT, rid2, e_stack);

    /* read data from datasets with read version 1. */

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    /* Give a location to the DXPL to store the checksum once the read has completed */
    H5Pset_dxpl_checksum_ptr(dxpl_id, &read1_cs);
    ret = H5Dread_ff(did1, dtid, sid, sid, dxpl_id, rdata1, rid2, e_stack);
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

    ret = H5Dread_ff(did2, dtid, sid, sid, dxpl_id, rdata2, rid2, e_stack);
    assert(ret == 0);
    H5Pclose(dxpl_id);

    /* Raw data read on D3. This is asynchronous.  Note that the type
       is different than the dataset type. */
    ret = H5Dread_ff(did3, H5T_STD_I16BE, sid, sid, H5P_DEFAULT, rdata3, rid2, e_stack);
    assert(ret == 0);



    /* Raw data read on D1. This is asynchronous.  The read is done into a 
       noncontiguous memory dataspace selection */
    {
        hid_t mem_space;
        hsize_t start = 0;
        hsize_t stride = 2;
        hsize_t count = nelem;
        hsize_t block = 1;
        int *buf = NULL;

        buf = calloc (nelem*2, sizeof(int));

        /* create a dataspace. This is a local Bookeeping operation that 
           does not touch the file */
        dims [0] = nelem*2;
        mem_space = H5Screate_simple(1, dims, NULL);
        H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, &start,&stride,&count,&block);

        ret = H5Dread_ff(did1, H5T_STD_I32LE, mem_space, sid, H5P_DEFAULT, buf, 
                         rid2, e_stack);
        assert(ret == 0);
        H5Sclose(mem_space);

        H5EStest(e_stack, 0, &status);
        printf("ESTest H5Dread Completion status = %d\n", status);

        H5ESwait(e_stack, 0, &status);
        printf("ESWait H5Dread Completion status = %d\n", status);
        assert (status);

        fprintf(stderr, "Printing all Dataset values. We should have a 0 after each element: ");
        for(i=0;i<120;++i)
            fprintf(stderr, "%d ", buf[i]);
        fprintf(stderr, "\n");

        free(buf);
    }


    /* create & start transaction 2 with num_peers = my_size. This
       means all processes are transaction leaders, and all have to
       call start and finish on the transaction. */
    tid2 = H5TRcreate(file_id, rid2, (uint64_t)2);
    assert(tid2);
    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, e_stack);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    /* Do more updates on transaction 2 */

    if(0 == my_rank) {
        extent = 10;
        ret = H5Dset_extent_ff(did1, &extent, tid2, e_stack);
        assert(ret == 0);
    }

    if((my_size > 1 && 1 == my_rank) || 
       (my_size == 1 && 0 == my_rank)) {
        extent = 30;
        ret = H5Dset_extent_ff(did2, &extent, tid2, e_stack);
        assert(ret == 0);
    }

    if((my_size > 2 && 2 == my_rank) || 
       (my_size == 1 && 0 == my_rank)) {
        extent = 60;
        ret = H5Dset_extent_ff(did3, &extent, tid2, e_stack);
        assert(ret == 0);
    }

    /* finish transaction 2 - all have to call */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, e_stack);
    assert(0 == ret);

    if(my_rank == 0) {
        /* release container version 1. This is async. */
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);
    }

    /* close objects */
    ret = H5Dclose_ff(did1, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(did2, e_stack);
    assert(ret == 0);
    ret = H5Dclose_ff(did3, e_stack);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, e_stack);
    assert(ret == 0);

    ret = H5Sclose(sid);
    assert(ret == 0);
    ret = H5Tclose(dtid);
    assert(ret == 0);
    ret = H5Pclose(fapl_id);
    assert(ret == 0);

    H5Fclose_ff(file_id, e_stack);

    H5ESget_count(e_stack, &num_events);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);

    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);

    H5ESclear(e_stack);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5RCclose(rid2);
    assert(0 == ret);
    ret = H5TRclose(tid2);
    assert(0 == ret);

    assert(read1_cs == cs);
    assert(read2_cs != cs);

    if(0 == my_rank) {
        query_id = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN, H5T_NATIVE_DOUBLE, &query_limit);

        H5ASexecute(file_name, "D1", query_id, split_script, combine_script, H5_EVENT_STACK_NULL);
//        H5ESwait_all(e_stack, &status);

        ret = H5Qclose(query_id);
        assert(0 == ret);
    }

//    ret = H5ESclose(e_stack);
//    assert(ret == 0);

    EFF_finalize();
    MPI_Finalize();

    return 0;
}
