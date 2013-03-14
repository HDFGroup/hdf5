
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
        hid_t datasetId;
        hid_t acc_tpl;
        hid_t fapl;
        hid_t plist_id = -1;
        hid_t vol_id;
        hid_t int_id;
        hid_t attr;
        hid_t space;
	const unsigned int nelem=60;
	int *data = NULL, *r_data = NULL;
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

	MPI_Init(&argc, &argv);
        H5VLeff_init(MPI_COMM_WORLD, MPI_INFO_NULL);

        MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
        MPI_Comm_size(MPI_COMM_WORLD, &my_size);
        printf("APP processes = %d, my rank is %d\n", my_size, my_rank);

        fapl = H5Pcreate (H5P_FILE_ACCESS);
        H5Pset_fapl_iod(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

        sprintf(fullpath,"%s/%s",group_name,dataset_name);
        data = malloc (sizeof(int)*nelem);
        r_data = malloc (sizeof(int)*nelem);

        for(i=0;i<nelem;++i) {
            r_data[i] = 0;
            data[i]=i;
        }

	file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
        dataspaceId = H5Screate_simple(1, dims, NULL);

        if(my_rank == 0) {
            gid1 = H5Gcreate2(file_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            datasetId = H5Dcreate(file_id,"/G1/G2/G3/D1",H5T_NATIVE_INT,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
            H5Dclose(datasetId);
        }
        MPI_Barrier(MPI_COMM_WORLD);
        if(my_rank != 0) {
            gid1 = H5Gopen(file_id,group_name,H5P_DEFAULT);
            //datasetId = H5Dopen(file_id,"/G1/G2/G3/D1",H5P_DEFAULT);
        }

        H5Sclose(dataspaceId);
        H5Gclose(gid1);
        H5Fclose(file_id);

        free(data);
        free(r_data);

        MPI_Finalize();
	return 0;
}

