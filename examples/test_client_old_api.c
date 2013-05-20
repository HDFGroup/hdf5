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
    int *data = NULL, *r_data = NULL, *r2_data = NULL, *data2 = NULL, *data3 = NULL, *a_data = NULL;
    unsigned int i = 0;
    hsize_t dims[1];
    int my_rank, my_size;
    int provided;
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

    /* allocate and initialize 3 arrays for dataset writes, and 2 arrays 
       for dataset reads. 
       The write arrays are intialized to contain 60 integers (0-59). 
       The read arrays are intialized to contain 60 integers all 0s. */
    data = malloc (sizeof(int)*nelem);
    data2 = malloc (sizeof(int)*nelem);
    data3 = malloc (sizeof(int)*nelem);
    r_data = malloc (sizeof(int)*nelem);
    a_data = malloc (sizeof(int)*nelem);
    r2_data = malloc (sizeof(int)*nelem);
    for(i=0;i<nelem;++i) {
        r_data[i] = 0;
        a_data[i] = 0;
        r2_data[i] = 0;
        data[i]=i;
        data2[i]=i;
        data3[i]=i;
    }

    /* create the file. */
    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    assert(file_id);

    gid1 = H5Gcreate2(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid2 = H5Gcreate2(file_id, "G1/G2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid3 = H5Gcreate2(file_id, "G1/G2/G3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(gid1);
    assert(gid2);
    assert(gid3);

    int_id = H5Tcopy(H5T_NATIVE_INT);
    H5Tcommit(file_id, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    dims [0] = 60;
    dataspaceId = H5Screate_simple(1, dims, NULL);

    aid1 = H5Acreate2(gid1, "ATTR1", H5T_NATIVE_INT, dataspaceId, H5P_DEFAULT, H5P_DEFAULT);
    assert(aid1);
    H5Aclose(aid1);

    exists = H5Aexists_by_name(file_id,"G1","ATTR1", H5P_DEFAULT);
    if(exists)
        printf("Attribute ATTR1 exists!\n");
    else
        printf("Attribute ATTR1 does NOT exist. This must be the test without a native backend\n");

    assert(H5Adelete_by_name(file_id, "G1", "ATTR1", H5P_DEFAULT) == 0);
    assert(!H5Aexists(gid1, "ATTR1"));

    did1 = H5Dcreate(file_id,"G1/G2/G3/D1",int_id,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    assert(did1);

    /* create an attribute on dataset D1 */
    aid2 = H5Acreate_by_name(file_id, "G1/G2/G3/D1", "ATTR2", H5T_NATIVE_INT, 
                             dataspaceId, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(aid2);
    H5Awrite(aid2, int_id, data);

    did2 = H5Dcreate(file_id,"G1/G2/G3/D2",int_id,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    assert(did2);

    did3 = H5Dcreate(file_id,"G1/G2/G3/D3",int_id,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    assert(did3);

    H5Dwrite(did1, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, data);
    H5Dwrite(did2, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, data2);
    H5Dwrite(did3, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, data3);
    H5Dread(did1, int_id, dataspaceId, dataspaceId, H5P_DEFAULT, r_data);

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    H5Pset_dxpl_inject_bad_checksum(dxpl_id, 1);
    H5Dread(did1, int_id, dataspaceId, dataspaceId, dxpl_id, r2_data);
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
        //assert(H5Dset_extent(did1, &extent) == 0);
    }

    assert(H5Dclose(did1) == 0);
    assert(H5Dclose(did2) == 0);
    assert(H5Dclose(did3) == 0);
    H5Sclose(dataspaceId);
    assert(H5Aclose(aid2) == 0);
    assert(H5Tclose(int_id) == 0);
    assert(H5Gclose(gid1) == 0);
    assert(H5Gclose(gid2) == 0);
    assert(H5Gclose(gid3) == 0);


    /* Test Links */
    gid1 = H5Gcreate2(file_id, "G4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    gid2 = H5Gcreate2(file_id, "G4/G5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    H5Lcreate_hard(file_id, "G1/G2/G3/D1", gid1, "G5/newD1", H5P_DEFAULT, H5P_DEFAULT);
    did1 = H5Dopen2(file_id,"G4/G5/newD1", H5P_DEFAULT);

    H5Lcreate_soft("/G1/G2/G3/D4", gid1, "G5/newD2", H5P_DEFAULT, H5P_DEFAULT);

    H5Lmove(file_id, "/G1/G2/G3/D3", file_id, "/G4/G5/D3moved", H5P_DEFAULT, H5P_DEFAULT);

    H5Ldelete(file_id, "/G1/G2/G3/D2", H5P_DEFAULT);

    assert(H5Dclose(did1) == 0);
    assert(H5Gclose(gid1) == 0);
    assert(H5Gclose(gid2) == 0);

    /* flush all the contents of file to disk. This is asynchronous. */
    assert(H5Fflush(file_id, H5F_SCOPE_GLOBAL) == 0);
    assert(H5Fclose(file_id) == 0);

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

    file_id = H5Fopen(file_name, H5F_ACC_RDONLY, fapl_id);
    assert(file_id);
    gid1 = H5Gopen2(file_id, "G1", H5P_DEFAULT);
    assert(gid1);
    int_id = H5Topen2(file_id, "int", H5P_DEFAULT);
    assert(int_id);
    did1 = H5Dopen2(file_id,"G1/G2/G3/D1", H5P_DEFAULT);
    assert(did1);
    //aid2 = H5Aopen(did1, "ATTR2", H5P_DEFAULT);
    aid2 = H5Aopen_by_name(file_id, "G1/G2/G3/D1", "ATTR2", H5P_DEFAULT, H5P_DEFAULT);
    assert(aid2);
    H5Aread(aid2, int_id, a_data);
    fprintf(stderr, "Printing Attribute data: ");
    for(i=0;i<nelem;++i)
        fprintf(stderr, "%d ",a_data[i]);
    fprintf(stderr, "\n");
    assert(H5Aclose(aid2) == 0);
    assert(H5Dclose(did1) == 0);
    assert(H5Tclose(int_id) == 0);
    assert(H5Gclose(gid1) == 0);
    assert(H5Fclose(file_id) == 0);
    H5Pclose(fapl_id);

    free(data);
    free(r_data);
    free(a_data);
    free(data2);
    free(r2_data);
    free(data3);

    /* This finalizes the EFF stack. ships a terminate and IOD finalize to the server 
       and shutsdown the FS server (when all clients send the terminate request) 
       and client */
    EFF_finalize();

    MPI_Finalize();
    return 0;
}

