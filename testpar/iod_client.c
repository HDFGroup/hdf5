
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

int main(int argc, char **argv) {
    const char file_name[]="test_file.h5";
    const char group_name[]="/Group";
    const char dataset_name[]="Data";
    const char attr_name[]="ATTR";
    char get_name[20];
    char fullpath[500];
    hid_t file_id;
    hid_t gid1, gid2;
    hid_t dataspaceId;
    hid_t did1, did2, did3;
    hid_t acc_tpl;
    hid_t fapl, dxpl_id;
    hid_t plist_id = -1;
    hid_t vol_id;
    hid_t int_id;
    hid_t attr;
    hid_t space;
    const unsigned int nelem=60;
    int *data = NULL, *r_data = NULL, *r2_data = NULL, *data2 = NULL, *data3 = NULL;
    unsigned int i = 0;
    hsize_t dims[1];
    hsize_t n = 1;
    ssize_t len;
    char name[25];
    int my_rank, my_size;
    H5A_info_t ainfo, ainfo1, ainfo2;
    H5G_info_t ginfo, ginfo1, ginfo2;
    H5O_info_t oinfo;
    hobj_ref_t oref;
    hdset_reg_ref_t dref;
    H5O_type_t obj_type;
    int provided;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        printf("MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    printf("APP processes = %d, my rank is %d\n", my_size, my_rank);

    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    sprintf(fullpath,"%s/%s",group_name,dataset_name);
    data = malloc (sizeof(int)*nelem);
    data2 = malloc (sizeof(int)*nelem);
    data3 = malloc (sizeof(int)*nelem);
    r_data = malloc (sizeof(int)*nelem);
    r2_data = malloc (sizeof(int)*nelem);

    for(i=0;i<nelem;++i) {
        r_data[i] = 0;
        r2_data[i] = 0;
        data[i]=i;
        data2[i]=i;
        data3[i]=i;
    }

    file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    dims [0] = 60;
    dataspaceId = H5Screate_simple(1, dims, NULL);

    gid1 = H5Gcreate2(file_id, "G1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(gid1);

    did1 = H5Dcreate(file_id,"G1/G2/G3/D1",H5T_NATIVE_INT,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    assert(did1);
    did2 = H5Dcreate(file_id,"G1/G2/G3/D2",H5T_NATIVE_INT,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    assert(did2);
    did3 = H5Dcreate(file_id,"G1/G2/G3/D3",H5T_NATIVE_INT,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
    assert(did3);

#if 0
    gid1 = H5Gopen(file_id,group_name,H5P_DEFAULT);
    did1 = H5Dopen(file_id,"G1/G2/G3/D1",H5P_DEFAULT);
    did2 = H5Dopen(file_id,"G1/G2/G3/D2",H5P_DEFAULT);
    did3 = H5Dopen(file_id,"G1/G2/G3/D3",H5P_DEFAULT);
#endif

    H5Dwrite(did1, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, data);
    H5Dwrite(did2, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, data2);
    H5Dwrite(did3, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, data3);


    H5Dread(did1, H5T_NATIVE_INT, dataspaceId, dataspaceId, H5P_DEFAULT, r_data);
    printf("Printing Before Waiting ");
    for(i=0;i<nelem;++i)
        printf("%d ",r_data[i]);
    printf("\n");

    dxpl_id = H5Pcreate (H5P_DATASET_XFER);
    H5Pset_dxpl_inject_bad_checksum(dxpl_id, 1);
    H5Dread(did1, H5T_NATIVE_INT, dataspaceId, dataspaceId, dxpl_id, r2_data);
    H5Pclose(dxpl_id);

    H5Sclose(dataspaceId);

    H5Dclose(did1);
    H5Dclose(did2);
    H5Dclose(did3);

    H5Gclose(gid1);

    H5Fclose(file_id);

    printf("Printing After Waiting ");
    for(i=0;i<nelem;++i)
        printf("%d ",r_data[i]);
    printf("\n");

    printf("Reading Bad Data ");
    for(i=0;i<nelem;++i)
        printf("%d ",r2_data[i]);
    printf("\n");

    free(data);
    free(r_data);
    free(data2);
    free(r2_data);
    free(data3);

    EFF_finalize();
    MPI_Finalize();
    return 0;
}

