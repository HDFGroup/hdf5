/* 
 * h5ff_client_attr.c: Client side test for attribute routines.
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
    hid_t gid1;
    hid_t sid, dtid;
    hid_t did1;
    hid_t aid1, aid2, aid3, aid4;
    hid_t tid1, tid2, rid1, rid2;
    hid_t fapl_id, trspl_id;
    hid_t event_q;
    hbool_t exists1;
    hbool_t exists2;

    uint64_t version;
    uint64_t trans_num;

    int *wdata1 = NULL, *wdata2 = NULL;
    int *rdata1 = NULL, *rdata2 = NULL;
    const unsigned int nelem=60;
    hsize_t dims[1];

    int my_rank, my_size;
    int provided;
    MPI_Request mpi_req;

    H5_status_t *status = NULL;
    int num_requests = 0;
    H5_request_t req1;
    H5_status_t status1;
    unsigned int i = 0;
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
    rdata1 = malloc (sizeof(int32_t)*nelem);
    rdata2 = malloc (sizeof(int32_t)*nelem);
    for(i=0;i<nelem;++i) {
        rdata1[i] = 0;
        rdata2[i] = 0;
        wdata1[i]=i;
        wdata2[i]=i*2;
    }

    /* create an event Queue for managing asynchronous requests. */
    event_q = H5EQcreate(fapl_id);
    assert(event_q);

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

    /* start transaction 1 with default num_peers (= 0). 
       This is asynchronous. */
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

    /* create group /G1 */
    gid1 = H5Gcreate_ff(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(gid1 > 0);

    /* create dataset /G1/D1 */
    did1 = H5Dcreate_ff(gid1, "D1", dtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(did1 > 0);

    /* Commit the datatype dtid to the file. */
    ret = H5Tcommit_ff(file_id, "DT1", dtid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, 
                       tid1, event_q);
    assert(ret == 0);

    /* create an attribute on root group */
    aid1 = H5Acreate_ff(file_id, "ROOT_ATTR", dtid, sid, 
                        H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(aid1);

    /* create an attribute on group G1. */
    aid2 = H5Acreate_ff(gid1, "GROUP_ATTR", dtid, sid, 
                        H5P_DEFAULT, H5P_DEFAULT, tid1, event_q);
    assert(aid2);

    /* write data to attributes */
    ret = H5Awrite_ff(aid1, dtid, wdata1, tid1, event_q);
    assert(ret == 0);
    ret = H5Awrite_ff(aid2, dtid, wdata2, tid1, event_q);
    assert(ret == 0);

    /* Barrier to make sure all processes are done writing so Process
       0 can finish transaction 1 and acquire a read context on it. */
    MPI_Barrier(MPI_COMM_WORLD);

    if(0 == my_rank) {
        MPI_Wait(&mpi_req, MPI_STATUS_IGNORE);

        ret = H5TRfinish(tid1, H5P_DEFAULT, &rid2, H5_EVENT_QUEUE_NULL);
        assert(0 == ret);
    }

    /* another barrier so other processes know that container version is acquried */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Local op */
    ret = H5TRclose(tid1);
    assert(0 == ret);

    /* close attribute objects */
    ret = H5Aclose_ff(aid1, event_q);
    assert(ret == 0);
    ret = H5Aclose_ff(aid2, event_q);
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

    ret = H5Aexists_ff(file_id, "ROOT_ATTR", &exists1, rid2, event_q);
    assert(ret == 0);
    ret = H5Aexists_by_name_ff(file_id, "G1", "GROUP_ATTR", H5P_DEFAULT, &exists2, rid2, event_q);
    assert(ret == 0);

    aid1 = H5Aopen_ff(file_id, "ROOT_ATTR", H5P_DEFAULT, rid2, event_q);
    aid2 = H5Aopen_ff(gid1, "GROUP_ATTR", H5P_DEFAULT, rid2, event_q);

    /* read data from datasets with read version 1. */
    ret = H5Aread_ff(aid1, dtid, rdata1, rid2, event_q);
    assert(ret == 0);
    ret = H5Aread_ff(aid2, dtid, rdata2, rid2, event_q);
    assert(ret == 0);

    /* close attribute objects */
    ret = H5Aclose_ff(aid1, event_q);
    assert(ret == 0);
    ret = H5Aclose_ff(aid2, event_q);
    assert(ret == 0);

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

    /* Delete an attribute */
    ret = H5Adelete_ff(file_id, "ROOT_ATTR", tid2, event_q);
    assert(ret == 0);

    /* rename an attribute */
    ret = H5Arename_ff(gid1, "GROUP_ATTR", "RENAMED_GROUP_ATTR", tid2, event_q);
    assert(ret == 0);

    /* create an attribute on dataset */
    aid3 = H5Acreate_ff(did1, "DSET_ATTR", dtid, sid, 
                        H5P_DEFAULT, H5P_DEFAULT, tid2, event_q);
    assert(aid1);

    /* create an attribute on datatype */
    aid4 = H5Acreate_ff(dtid, "DTYPE_ATTR", dtid, sid, 
                        H5P_DEFAULT, H5P_DEFAULT, tid2, event_q);
    assert(aid2);

    /* finish transaction 2 */
    ret = H5TRfinish(tid2, H5P_DEFAULT, NULL, event_q);
    assert(0 == ret);

    /* release container version 1. This is async. */
    ret = H5RCrelease(rid2, event_q);
    assert(0 == ret);

    /* close objects */
    ret = H5Aclose_ff(aid3, event_q);
    assert(ret == 0);
    ret = H5Aclose_ff(aid4, event_q);
    assert(ret == 0);
    ret = H5Dclose_ff(did1, event_q);
    assert(ret == 0);
    ret = H5Gclose_ff(gid1, event_q);
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

    /* Now we can check operations that were issued previously */
    if(exists1)
        printf("Attribute ROOT_ATTR exists!\n");
    else
        printf("Errr Attribute ROOT_ATTR does NOT exist. \n");

    if(exists2)
        printf("Attribute GROUP_ATTR exists!\n");
    else
        printf("Errr Attribute GROUP_ATTR does NOT exist. \n");

    fprintf(stderr, "Read Data1: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata1[i]);
    fprintf(stderr, "\n");

    fprintf(stderr, "Read Data2: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",rdata2[i]);
    fprintf(stderr, "\n");

    ret = H5EQclose(event_q);
    assert(ret == 0);

    free(wdata1);
    free(wdata2);
    free(rdata1);
    free(rdata2);

    EFF_finalize();
    MPI_Finalize();

    return 0;
}
