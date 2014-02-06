/* 
 * test_client_map.c: Client side of Map Object
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

int main(int argc, char **argv) {
    const char file_name[]="eff_file_map.h5";
    hid_t file_id;
    hid_t gid1, gid2, dtid1, dtid2;
    hid_t map1, map2, map3;
    hid_t tid1, tid2, rid1, rid2, rid3;
    hid_t fapl_id, dxpl_id, trspl_id;
    hid_t e_stack;

    const char *str_wdata[5]= {
        "Four score and seven years ago our forefathers brought forth on this continent a new nation,",
        "conceived in liberty and dedicated to the proposition that all men are created equal.",
        "Now we are engaged in a great civil war,",
        "testing whether that nation or any nation so conceived and so dedicated can long endure.",
        "President Abraham Lincoln"
    };   /* Information to write */
    char *str_rdata[5];   /* Information read in */
    hvl_t wdata[5];   /* Information to write */
    hvl_t rdata[5];   /* Information to write */
    int i;

    void *map_token1, *map_token2, *map_token3;
    size_t token_size1, token_size2, token_size3;
    uint64_t version;
    uint64_t trans_num;

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req, mpi_reqs[6];

    H5ES_status_t status;
    size_t num_events = 0;
    herr_t ret;

    hsize_t count = -1;
    int key, value;
    hbool_t exists;

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
    e_stack = H5EScreate();
    assert(e_stack);


    /* set write buffer for VL data*/
    {
        int increment, j, n;

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
    }

    /* Create an HDF5 VL datatype */
    dtid1 = H5Tvlen_create (H5T_NATIVE_UINT);
    /* Create an HDF5 VL string datatype */
    dtid2 = H5Tcopy(H5T_C_S1);
    H5Tset_size(dtid2,H5T_VARIABLE);

    /* create the file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL);
    assert(file_id > 0);

    /* acquire container version 0 - EXACT.  
       This can be asynchronous, but here we need the acquired ID 
       right after the call to start the transaction so we make synchronous. */
    version = 0;
    rid1 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(0 == version);

    MPI_Barrier(MPI_COMM_WORLD);

    /* create transaction object */
    tid1 = H5TRcreate(file_id, rid1, (uint64_t)1);
    assert(tid1);

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

        /* create two groups */
        gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);
        gid2 = H5Gcreate_ff(gid1, "G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, e_stack);

        /* Create 3 Map objects with the key type being 32 bit LE integer */

        /* First Map object with a Value type a 32 bit LE integer */
        map1 = H5Mcreate_ff(file_id, "MAP_1", H5T_STD_I32LE, H5T_STD_I32LE, 
                            H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid1, e_stack);

        /* Second Map object with a Value type being an HDF5 VL datatype */
        map2 = H5Mcreate_ff(gid1, "MAP_2", H5T_STD_I32LE, dtid1, 
                            H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid1, e_stack);

        /* Third Map object with a Value type being an HDF5 VL string */
        map3 = H5Mcreate_ff(gid2, "MAP_3", H5T_STD_I32LE, dtid2, 
                            H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT, tid1, e_stack);

        /* write some KV pairs to each map object. */
        {
            key = 1;
            value = 1000;
            ret = H5Mset_ff(map1, H5T_STD_I32LE, &key, H5T_STD_I32LE, &value,
                            H5P_DEFAULT, tid1, e_stack);
            assert(ret == 0);

            for(i=0 ; i<5 ; i++) {
                key = i;
                ret = H5Mset_ff(map2, H5T_STD_I32LE, &key, dtid1, &wdata[i],
                                H5P_DEFAULT, tid1, e_stack);
                assert(ret == 0);
            }

            for(i=0 ; i<5 ; i++) {
                key = i;
                ret = H5Mset_ff(map3, H5T_STD_I32LE, &key, dtid2, str_wdata[i],
                                H5P_DEFAULT, tid1, e_stack);
                assert(ret == 0);
            }
        }
    }

    /* Tell Delegates that transaction 1 is started */
    MPI_Ibcast(&trans_num, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD, &mpi_req);

    /* Do the local-to-global, global-to-local, so all delegates can
       write the maps created in transaction 1 */

    if(0 == my_rank) {
        /* get the token size of each map */
        ret = H5Oget_token(map1, NULL, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(map2, NULL, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(map3, NULL, &token_size3);
        assert(0 == ret);

        /* allocate buffers for each token */
        map_token1 = malloc(token_size1);
        map_token2 = malloc(token_size2);
        map_token3 = malloc(token_size3);

        /* get the token buffer */
        ret = H5Oget_token(map1, map_token1, &token_size1);
        assert(0 == ret);
        ret = H5Oget_token(map2, map_token2, &token_size2);
        assert(0 == ret);
        ret = H5Oget_token(map3, map_token3, &token_size3);
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
        MPI_Ibcast(map_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[3]);
        MPI_Ibcast(map_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[4]);
        MPI_Ibcast(map_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[5]);
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
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        /* allocate buffers for each token */
        map_token1 = malloc(token_size1);
        map_token2 = malloc(token_size2);
        map_token3 = malloc(token_size3);

        /* recieve the tokens */
        MPI_Ibcast(map_token1, token_size1, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[0]);
        MPI_Ibcast(map_token2, token_size2, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[1]);
        MPI_Ibcast(map_token3, token_size3, MPI_BYTE, 0, MPI_COMM_WORLD, &mpi_reqs[2]);
        MPI_Waitall(3, mpi_reqs, MPI_STATUS_IGNORE);

        map1 = H5Oopen_by_token(map_token1, tid1, e_stack);
        map2 = H5Oopen_by_token(map_token2, tid1, e_stack);
        map3 = H5Oopen_by_token(map_token3, tid1, e_stack);
    }


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

    /* Leader process finishes/commits the transaction and acquires a
       read context on it */
    if(0 == my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);
        MPI_Waitall(6, mpi_reqs, MPI_STATUS_IGNORE);

        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }

    /* another barrier so other processes know that container version is acquried */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Close the first transaction object. Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* release container version 0. This is async. */
    ret = H5RCrelease(rid1, e_stack);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    /* Leader process tells delegates that container version 1 is acquired */
    version = 1;
    MPI_Bcast(&version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD);

    /* delegates just create a read context object; no need to acquire
       it, since it has been acquired by the leader. */
    if(0 != my_rank) {
        rid2 = H5RCcreate(file_id, version);
        assert(rid2 > 0);
    }
    if(0 == my_rank) {
        assert(H5Gclose_ff(gid1, e_stack) == 0);
        assert(H5Gclose_ff(gid2, e_stack) == 0);
    }

    /* issue some read operations using the read context acquired */

    ret = H5Mget_count_ff(map2, &count, rid2, e_stack);
    assert(ret == 0);

    key = 1;
    ret = H5Mexists_ff(map1, H5T_STD_I32LE, &key, &exists, rid2, e_stack);
    assert(ret == 0);

    /* create & start transaction 2 with Multiple Leader - No Delegate Model. */
    tid2 = H5TRcreate(file_id, rid2, (uint64_t)2);
    assert(tid2);
    trspl_id = H5Pcreate (H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, my_size);
    assert(0 == ret);
    ret = H5TRstart(tid2, trspl_id, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    MPI_Barrier(MPI_COMM_WORLD);

    if((my_size > 1 && 1 == my_rank) || 
       (my_size == 1 && 0 == my_rank)) {
        hid_t temp_id;

        /* modify container contents using transaction started. */
        key = 1;
        ret = H5Mdelete_ff(map3, H5T_STD_I32LE, &key, tid2, H5_EVENT_STACK_NULL);

        temp_id = H5Gcreate_ff(file_id, "temp_group", H5P_DEFAULT, H5P_DEFAULT, 
                               H5P_DEFAULT, tid2, H5_EVENT_STACK_NULL);
        if(temp_id > 0)
            assert(H5Gclose_ff(temp_id, H5_EVENT_STACK_NULL) ==0);
    }

    /* finish transaction 2 */
    if(my_rank == 0) {
        ret = H5TRabort(tid2, H5_EVENT_STACK_NULL);
        assert(0 == ret);
    }
    else {
        ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL);
        if(ret < 0)
            fprintf(stderr, "Transaction finish failed (aborted)\n");
    }

    MPI_Barrier(MPI_COMM_WORLD);

    assert(H5Mclose_ff(map1, e_stack) == 0);
    assert(H5Mclose_ff(map2, e_stack) == 0);
    assert(H5Mclose_ff(map3, e_stack) == 0);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    ret = H5RCclose(rid1);
    assert(0 == ret);
    ret = H5TRclose(tid2);
    assert(0 == ret);

    /* Barrier so all processes are guranteed to have finished/aborted transaction 2 */
    MPI_Barrier(MPI_COMM_WORLD);

    /* acquire container version 2 - EXACT  (Should Fail since 2 is aborted) */
    version = 2;
    rid3 = H5RCacquire(file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL);
    assert(rid3 < 0);

    /* Now read some data from the container at version 1*/

    map1 = H5Mopen_ff(file_id, "MAP_1", H5P_DEFAULT, rid2, e_stack);
    map2 = H5Mopen_ff(file_id, "G1/MAP_2", H5P_DEFAULT, rid2, e_stack);
    map3 = H5Mopen_ff(file_id, "G1/G2/MAP_3", H5P_DEFAULT, rid2, e_stack);

    {
        int key, value;
        int increment=4, j=0;

        key = 1;
        value = -1;
        ret = H5Mget_ff(map1, H5T_STD_I32LE, &key, H5T_STD_I32LE, &value,
                        H5P_DEFAULT, rid2, e_stack);

        H5ESwait(e_stack, 0, &status);
        printf("H5Mget Completion status = %d\n", status);
        assert (status);

        printf("Value recieved = %d\n", value);

        for(i=0 ; i<5 ; i++) {
            key = i;
            ret = H5Mget_ff(map2, H5T_STD_I32LE, &key, dtid1, &rdata[i],
                            H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
            assert(ret == 0);
        }

        /* Print VL DATA */
        fprintf(stderr, "Reading VL Data: \n");
        for(i = 0; i < 5; i++) {
            int temp = i*increment + increment;

            fprintf(stderr, "Key %d  size %zu: ", i, rdata[i].len);
            for(j = 0; j < temp; j++)
                fprintf(stderr, "%d ",((unsigned int *)rdata[i].p)[j]);
            fprintf(stderr, "\n");
        } /* end for */

        for(i=0 ; i<5 ; i++) {
            key = i;
            str_rdata[i] = NULL;
            ret = H5Mget_ff(map3, H5T_STD_I32LE, &key, dtid2, &str_rdata[i],
                            H5P_DEFAULT, rid2, H5_EVENT_STACK_NULL);
            assert(ret == 0);
        }

        fprintf(stderr, "Reading VL Strings: \n");

        for(i=0 ; i<5 ; i++) {
            fprintf(stderr, "Key %d:  %s\n", i, str_rdata[i]);
            free(str_rdata[i]);
        }
    }

    ret = H5Tclose(dtid1);
    assert(ret == 0);
    ret = H5Tclose(dtid2);
    assert(ret == 0);

    assert(H5Mclose_ff(map1, e_stack) == 0);
    assert(H5Mclose_ff(map2, e_stack) == 0);
    assert(H5Mclose_ff(map3, e_stack) == 0);

    if(my_rank == 0) {
        /* release container version 1. This is async. */
        ret = H5RCrelease(rid2, e_stack);
        assert(0 == ret);
    }
    ret = H5RCclose(rid2);
    assert(0 == ret);

    H5ESget_count(e_stack, &num_events);
    H5ESwait_all(e_stack, &status);
    H5ESclear(e_stack);
    printf("%d events in event stack. Completion status = %d\n", num_events, status);

    /* closing the container also acts as a wait all on all pending requests 
       on the container. */
    assert(H5Fclose_ff(file_id, H5_EVENT_STACK_NULL) == 0);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Wait on everything in ES and check Results of operations in ES\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    H5ESget_count(e_stack, &num_events);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);

    H5ESwait_all(e_stack, &status);
    printf("%d events in event stack. H5ESwait_all Completion status = %d\n", num_events, status);

    H5EStest_all(e_stack, &status);
    printf("%d events in event stack. H5EStest_all Completion status = %d\n", num_events, status);

    H5ESclear(e_stack);

    assert (count == 5);
    assert (exists > 0);

    fprintf(stderr, "\n*****************************************************************************************************************\n");
    fprintf(stderr, "Finalize EFF stack\n");
    fprintf(stderr, "*****************************************************************************************************************\n");

    H5ESclose(e_stack);
    H5Pclose(fapl_id);

    for(i=0 ; i<5 ; i++) {
        free(wdata[i].p);
        free(rdata[i].p);
    }

    free(map_token1);
    free(map_token2);
    free(map_token3);

    MPI_Barrier(MPI_COMM_WORLD);
    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}
